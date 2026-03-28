    ! Inlined parse_number_string body.
    ! Required variables declared in host:
    !   pns_first, pns_last (integer, input), pns_opts (parse_options), pns_bj (logical)
    !   str (character, from host), p (parsed_number, output)
    !   pns_i, pns_dc, pns_exp, pns_en (integer(i8))
    !   pns_p, pns_sd, pns_eip, pns_bf, pns_le, pns_ic, pns_ie, pns_fe, pns_sp_ (integer)
    !   pns_alp, pns_hdp, pns_ne, pns_hse, pns_hed (logical)

    pns_p = pns_first
    p%valid = .false.
    if (pns_p > pns_last) then
        p%last_idx = pns_p
        goto 900
    end if

    p%negative = (str(pns_p:pns_p) == '-')
    pns_alp = iand(pns_opts%format, FMT_ALLOW_PLUS) /= 0

    if (str(pns_p:pns_p) == '-' .or. &
        (pns_alp .and. .not. pns_bj .and. str(pns_p:pns_p) == '+')) then
        pns_p = pns_p + 1
        if (pns_p > pns_last) then
            p%last_idx = pns_p
            goto 900
        end if
        if (pns_bj) then
            if (.not. is_digit(str(pns_p:pns_p))) then
                p%last_idx = pns_p
                goto 900
            end if
        else
            if (.not. is_digit(str(pns_p:pns_p)) .and. &
                str(pns_p:pns_p) /= pns_opts%decimal_point) then
                p%last_idx = pns_p
                goto 900
            end if
        end if
    end if

    pns_sd = pns_p
    pns_i = 0_i8
    do while (pns_p <= pns_last)
        pns_ic = iachar(str(pns_p:pns_p))
        if (.not. is_digit(pns_ic)) exit
        pns_i = 10*pns_i + int(pns_ic - 48, i8)
        pns_p = pns_p + 1
    end do

    pns_eip = pns_p
    pns_dc = int(pns_eip - pns_sd, i8)
    p%int_start = pns_sd
    p%int_len = int(pns_dc)

    if (pns_bj) then
        if (pns_dc == 0) then
            p%last_idx = pns_p
            goto 900
        end if
        if (str(pns_sd:pns_sd) == '0' .and. pns_dc > 1) then
            p%last_idx = pns_sd
            goto 900
        end if
    end if

    pns_exp = 0_i8
    p%frac_start = 0
    p%frac_len = 0
    pns_hdp = .false.
    if (pns_p <= pns_last) pns_hdp = (str(pns_p:pns_p) == pns_opts%decimal_point)

    if (pns_hdp) then
        pns_p = pns_p + 1
        pns_bf = pns_p
        call loop_parse_eight(pns_p, pns_last, str, pns_i)
        do while (pns_p <= pns_last)
            pns_ic = iachar(str(pns_p:pns_p))
            if (.not. is_digit(pns_ic)) exit
            pns_i = pns_i*10 + int(pns_ic-48, i8)
            pns_p = pns_p + 1
        end do
        pns_exp = int(pns_bf - pns_p, i8)
        p%frac_start = pns_bf
        p%frac_len = pns_p - pns_bf
        pns_dc = pns_dc - pns_exp
    end if

    if (pns_bj) then
        if (pns_hdp .and. pns_exp == 0) then
            p%last_idx = pns_p
            goto 900
        end if
    else
        if (pns_dc == 0) then
            p%last_idx = pns_p
            goto 900
        end if
    end if

    pns_en = 0_i8
    pns_hse = .false.
    if (pns_p <= pns_last) then
        pns_hse = (iand(pns_opts%format, FMT_SCIENTIFIC) /= 0 .and. &
                   (str(pns_p:pns_p) == 'e' .or. str(pns_p:pns_p) == 'E')) .or. &
                  (iand(pns_opts%format, FMT_FORTRAN) /= 0 .and. &
                   (str(pns_p:pns_p) == '+' .or. str(pns_p:pns_p) == '-' .or. &
                    str(pns_p:pns_p) == 'd' .or. str(pns_p:pns_p) == 'D'))
    end if
    if (pns_hse) then
        pns_le = pns_p
        if (str(pns_p:pns_p) == 'e' .or. str(pns_p:pns_p) == 'E' .or. &
            str(pns_p:pns_p) == 'd' .or. str(pns_p:pns_p) == 'D') pns_p = pns_p + 1
        pns_ne = .false.
        if (pns_p <= pns_last) then
            if (str(pns_p:pns_p) == '-') then
                pns_ne = .true.
                pns_p = pns_p + 1
            else if (str(pns_p:pns_p) == '+') then
                pns_p = pns_p + 1
            end if
        end if
        pns_hed = .false.
        if (pns_p <= pns_last) pns_hed = is_digit(str(pns_p:pns_p))
        if (.not. pns_hed) then
            if (iand(pns_opts%format, FMT_FIXED) == 0) then
                p%last_idx = pns_p
                goto 900
            end if
            pns_p = pns_le
        else
            do while (pns_p <= pns_last)
                pns_ic = iachar(str(pns_p:pns_p))
                if (.not. is_digit(pns_ic)) exit
                if (pns_en < 268435456_i8) pns_en = 10*pns_en + int(pns_ic-48, i8)
                pns_p = pns_p + 1
            end do
            if (pns_ne) pns_en = -pns_en
            pns_exp = pns_exp + pns_en
        end if
    else
        if (iand(pns_opts%format, FMT_SCIENTIFIC) /= 0 .and. &
            iand(pns_opts%format, FMT_FIXED) == 0) then
            p%last_idx = pns_p
            goto 900
        end if
    end if

    p%last_idx = pns_p
    p%valid = .true.
    p%too_many_digits = .false.

    if (pns_dc > 19) then
        pns_sp_ = pns_sd
        do while (pns_sp_ <= pns_last)
            if (str(pns_sp_:pns_sp_) /= '0' .and. &
                str(pns_sp_:pns_sp_) /= pns_opts%decimal_point) exit
            if (str(pns_sp_:pns_sp_) == '0') pns_dc = pns_dc - 1
            pns_sp_ = pns_sp_ + 1
        end do
        if (pns_dc > 19) then
            p%too_many_digits = .true.
            pns_i = 0
            pns_p = p%int_start
            pns_ie = pns_p + p%int_len
            do while (unsigned_lt(pns_i, 1000000000000000000_i8) .and. pns_p < pns_ie)
                pns_i = pns_i*10 + int( iachar(str(pns_p:pns_p)) - 48, i8)
                pns_p = pns_p + 1
            end do
            if (unsigned_ge(pns_i, 1000000000000000000_i8)) then
                pns_exp = int(pns_eip - pns_p, i8) + pns_en
            else
                pns_p = p%frac_start
                pns_fe = pns_p + p%frac_len
                do while (unsigned_lt(pns_i, 1000000000000000000_i8) .and. pns_p < pns_fe)
                    pns_i = pns_i*10 + int( iachar(str(pns_p:pns_p)) - 48, i8)
                    pns_p = pns_p + 1
                end do
                pns_exp = int(p%frac_start - pns_p, i8) + pns_en
            end if
        end if
    end if
    p%exponent = pns_exp
    p%mantissa = pns_i
900 continue
