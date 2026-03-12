program benchmark_compare
    use iso_c_binding, only: c_char, c_double, c_int32_t, c_null_char, c_size_t
    use iso_fortran_env, only: int64, output_unit, real64
    use fast_float_module, only: FFC_OUTCOME_OK, ffc_parse_double, ffc_result
    use ffc_c_bridge, only: benchmark_ffc_lines_c, ffc_parse_double_c
    implicit none(type, external)

    type :: benchmark_line
        character(:), allocatable :: text
        character(kind=c_char), pointer :: c_text(:) => null()
    end type benchmark_line

    type :: benchmark_file
        character(:), allocatable :: path
    end type benchmark_file

    character(len=1024) :: filename, arg
    integer :: i, nargs, repeat_count
    logical :: has_file
    type(benchmark_file), allocatable :: files(:)
    type(benchmark_line), allocatable :: lines(:)
    character(kind=c_char), allocatable :: packed_data(:)
    integer(c_size_t), allocatable :: lengths(:), offsets(:)
    integer :: nlines, volume

    filename = ""
    has_file = .false.
    repeat_count = 100

    nargs = command_argument_count()
    i = 1
    do while (i <= nargs)
        call get_command_argument(i, arg)
        select case (trim(arg))
        case ("-f", "--file")
            i = i + 1
            call get_command_argument(i, filename)
            has_file = .true.
            call add_file(files, trim(filename))
        case ("-r", "--repeat")
            i = i + 1
            call get_command_argument(i, arg)
            read(arg, *) repeat_count
        case ("-h", "--help")
            call print_help()
            stop
        end select
        i = i + 1
    end do

    if (.not. has_file) then
        call print_help()
        stop 1
    end if

    do i = 1, size(files)
        call load_lines(files(i)%path, lines, packed_data, offsets, lengths, nlines, volume)
        call verify(lines)
        call run_benchmark(files(i)%path, lines, packed_data, offsets, lengths, nlines, volume, repeat_count)
    end do

contains

    subroutine print_help()
        write(output_unit, "(a)") "Usage: fpm test --target benchmark_compare -- -f <datafile> [-f <datafile> ...] [-r repeat]"
    end subroutine print_help

    subroutine add_file(files, path)
        type(benchmark_file), allocatable, intent(inout) :: files(:)
        character(*), intent(in) :: path
        type(benchmark_file), allocatable :: tmp(:)
        integer :: n

        if (.not. allocated(files)) then
            allocate(files(1))
            files(1)%path = path
            return
        end if

        n = size(files)
        allocate(tmp(n + 1))
        tmp(1:n) = files
        tmp(n + 1)%path = path
        call move_alloc(tmp, files)
    end subroutine add_file

    subroutine load_lines(fname, lines, packed_data, offsets, lengths, nlines, volume)
        character(*), intent(in) :: fname
        type(benchmark_line), allocatable, intent(out) :: lines(:)
        character(kind=c_char), allocatable, intent(out) :: packed_data(:)
        integer(c_size_t), allocatable, intent(out) :: offsets(:), lengths(:)
        integer, intent(out) :: nlines, volume

        character(len=1024) :: line
        integer :: cursor, i, ios, j, n, unit

        open(newunit=unit, file=fname, status="old", action="read", iostat=ios)
        if (ios /= 0) error stop "failed to open benchmark input file"

        nlines = 0
        volume = 0
        do
            read(unit, "(a)", iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            volume = volume + len_trim(line)
        end do

        allocate(lines(nlines))
        allocate(offsets(nlines), lengths(nlines))
        allocate(packed_data(max(volume, 1)))
        rewind(unit)

        if (volume == 0) packed_data(1) = c_null_char
        cursor = 1
        do i = 1, nlines
            read(unit, "(a)", iostat=ios) line
            if (ios /= 0) error stop "failed while reading benchmark input file"

            n = len_trim(line)
            offsets(i) = int(cursor - 1, c_size_t)
            lengths(i) = int(n, c_size_t)
            if (n == 0) then
                lines(i)%text = ""
                allocate(lines(i)%c_text(1))
                lines(i)%c_text(1) = c_null_char
            else
                lines(i)%text = line(:n)
                allocate(lines(i)%c_text(n))
                lines(i)%c_text = [(line(j:j), j=1, n)]
                packed_data(cursor:cursor+n-1) = lines(i)%c_text
                cursor = cursor + n
            end if
        end do

        close(unit)
    end subroutine load_lines

    subroutine verify(lines)
        type(benchmark_line), intent(in) :: lines(:)

        integer :: i
        integer(c_int32_t) :: c_outcome
        real(c_double) :: c_value
        real(real64) :: f_value
        type(ffc_result) :: f_result

        do i = 1, size(lines)
            f_result = ffc_parse_double(lines(i)%text, f_value)
            call ffc_parse_double_c(lines(i)%c_text, len(lines(i)%text), c_value, c_outcome)

            if (f_result%outcome /= int(c_outcome)) then
                write(output_unit, "(a,i0)") "outcome mismatch at line ", i
                error stop 1
            end if

            if (f_result%outcome == FFC_OUTCOME_OK) then
                if (transfer(f_value, 0_int64) /= transfer(real(c_value, real64), 0_int64)) then
                    write(output_unit, "(a,i0)") "value mismatch at line ", i
                    error stop 1
                end if
            end if
        end do
    end subroutine verify

    subroutine run_benchmark(name, lines, packed_data, offsets, lengths, nlines, volume, repeat_count)
        character(*), intent(in) :: name
        type(benchmark_line), intent(in) :: lines(:)
        character(kind=c_char), intent(in) :: packed_data(:)
        integer(c_size_t), intent(in) :: offsets(:), lengths(:)
        integer, intent(in) :: nlines, volume, repeat_count

        real(real64) :: answer, checksum_c, checksum_f
        real(real64) :: avg_ns_c, avg_ns_f, elapsed_ns, min_ns_c, min_ns_f, volume_mb
        real(real64) :: x_f
        real(c_double) :: x_c
        integer :: i, r
        integer(c_int32_t) :: c_outcome
        integer(int64) :: count_end, count_rate, count_start
        type(ffc_result) :: f_result

        volume_mb = real(volume, real64) / (1024.0_real64 * 1024.0_real64)
        min_ns_f = huge(1.0_real64)
        min_ns_c = huge(1.0_real64)
        avg_ns_f = 0.0_real64
        avg_ns_c = 0.0_real64
        checksum_f = 0.0_real64
        checksum_c = 0.0_real64

        call system_clock(count_rate=count_rate)
        do r = 1, repeat_count
            answer = 0.0_real64
            call system_clock(count=count_start)
            do i = 1, nlines
                f_result = ffc_parse_double(lines(i)%text, x_f)
                if (f_result%outcome /= FFC_OUTCOME_OK) cycle
                if (x_f > answer) answer = x_f
            end do
            call system_clock(count=count_end)
            elapsed_ns = real(count_end - count_start, real64) / real(count_rate, real64) * 1.0e9_real64
            avg_ns_f = avg_ns_f + elapsed_ns
            if (elapsed_ns < min_ns_f) min_ns_f = elapsed_ns
            checksum_f = answer
        end do
        avg_ns_f = avg_ns_f / real(repeat_count, real64)

        do r = 1, repeat_count
            call system_clock(count=count_start)
            call benchmark_ffc_lines_c(packed_data, offsets, lengths, nlines, x_c, c_outcome)
            call system_clock(count=count_end)
            elapsed_ns = real(count_end - count_start, real64) / real(count_rate, real64) * 1.0e9_real64
            avg_ns_c = avg_ns_c + elapsed_ns
            if (elapsed_ns < min_ns_c) min_ns_c = elapsed_ns
            checksum_c = real(x_c, real64)
        end do
        avg_ns_c = avg_ns_c / real(repeat_count, real64)

        write(output_unit, "(a)") ""
        write(output_unit, "(a)") "# file=" // trim(name)
        write(output_unit, "(a,i0,a,f0.6,a)") "# lines=", nlines, " volume=", volume_mb, " MB"
        call print_result("fortran (fast_float_module)", volume_mb, nlines, min_ns_f, avg_ns_f)
        call print_result("c (ffc.h)", volume_mb, nlines, min_ns_c, avg_ns_c)
        write(output_unit, "(a,z16.16)") "fortran checksum bits = ", transfer(checksum_f, 0_int64)
        write(output_unit, "(a,z16.16)") "c checksum bits       = ", transfer(checksum_c, 0_int64)
        write(output_unit, "(a,f8.3,a)") "speed ratio c/fortran = ", min_ns_f / min_ns_c, "x"
    end subroutine run_benchmark

    subroutine print_result(name, volume_mb, nlines, min_ns, avg_ns)
        character(*), intent(in) :: name
        real(real64), intent(in) :: volume_mb, min_ns, avg_ns
        integer, intent(in) :: nlines

        write(output_unit, "(a40,a,f12.2,a,f5.1,a,f12.2,a,f12.2,a)") &
            name, ": ", &
            volume_mb * 1.0e9_real64 / min_ns, " MB/s (+/- ", &
            (avg_ns - min_ns) * 100.0_real64 / avg_ns, " %) ", &
            real(nlines, real64) * 1.0e3_real64 / min_ns, " Mfloat/s  ", &
            min_ns / real(nlines, real64), " ns/f"
    end subroutine print_result

end program benchmark_compare
