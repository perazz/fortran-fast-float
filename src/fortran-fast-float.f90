module fortran_fast_float
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fortran-fast-float!"
  end subroutine say_hello
end module fortran_fast_float
