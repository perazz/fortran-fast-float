    ! Inlined compute_float + compute_product.
    ! Required variables declared in host:
    !   cf_q, cf_wi (integer(i8), input), cf_f (float_format, input)
    !   cf_am (adjusted_mantissa, output)
    !   cf_w (integer(i8)), cf_lz (integer(i4)), cf_pr (u128)
    !   cf_ub, cf_sa (integer)
    !   cp_pm, cp_bp (integer(i8)), cp_idx (integer), cp_sp (u128)

    cf_w = cf_wi
    if (cf_w == 0 .or. cf_q < int(cf_f%smallest_pow10, i8)) then
        cf_am = adjusted_mantissa(0, 0)
    else if (cf_q > int(cf_f%largest_pow10, i8)) then
        cf_am = adjusted_mantissa(0, int(cf_f%inf_power, i4))
    else
        cf_lz = int(count_leading_zeros(cf_w), i4)
        cf_w = ishft(cf_w, cf_lz)

        ! Inline compute_product
        cp_bp = int(cf_f%mantissa_bits + 3, i8)
        cp_pm = ishft(not(0_i8), -int(cp_bp))
        cp_idx = 2*int(cf_q - (-342_i8)) + 1
        call mul_u64(cf_w, POWER5_TABLE(cp_idx), cf_pr)
        if (iand(cf_pr%hi, cp_pm) == cp_pm) then
            call mul_u64(cf_w, POWER5_TABLE(cp_idx + 1), cp_sp)
            cf_pr%lo = cf_pr%lo + cp_sp%hi
            if (unsigned_lt(cf_pr%lo, cp_sp%hi)) cf_pr%hi = cf_pr%hi + 1
        end if

        cf_ub = int(ishft(cf_pr%hi, -63))
        cf_sa = cf_ub + 64 - cf_f%mantissa_bits - 3
        cf_am%mantissa = ishft(cf_pr%hi, -cf_sa)
        cf_am%power2 = b10_to_b2(int(cf_q, i4)) + int(cf_ub-cf_lz-cf_f%min_exponent, i4)

        if (cf_am%power2 <= 0) then
            if (-cf_am%power2 + 1 >= 64) then
                cf_am = adjusted_mantissa(0_i8, 0_i4)
            else
                cf_am%mantissa = ishft(cf_am%mantissa, cf_am%power2 - 1_i4)
                cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
                cf_am%mantissa = ishft(cf_am%mantissa, -1)
                if (unsigned_lt(cf_am%mantissa, ishft(1_i8, cf_f%mantissa_bits))) then
                    cf_am%power2 = 0
                else
                    cf_am%power2 = 1
                end if
            end if
        else
            if (iand(cf_pr%lo, not(1_i8)) == 0 .and. &
                cf_q >= int(cf_f%min_round_trip_exp, i8) .and. &
                cf_q <= int(cf_f%max_round_trip_exp, i8) .and. &
                iand(cf_am%mantissa, 3_i8) == 1 .and. &
                ishft(cf_am%mantissa, cf_sa) == cf_pr%hi) &
                    cf_am%mantissa = iand(cf_am%mantissa, not(1_i8))

            cf_am%mantissa = cf_am%mantissa + iand(cf_am%mantissa, 1_i8)
            cf_am%mantissa = ishft(cf_am%mantissa, -1)

            if (unsigned_ge(cf_am%mantissa, ishft(2_i8, cf_f%mantissa_bits))) then
                cf_am%mantissa = ishft(1_i8, cf_f%mantissa_bits)
                cf_am%power2 = cf_am%power2 + 1
            end if

            cf_am%mantissa = iand(cf_am%mantissa, not(ishft(1_i8, cf_f%mantissa_bits)))
            if (cf_am%power2 >= int(cf_f%inf_power, i4)) then
                cf_am%power2 = int(cf_f%inf_power, i4)
                cf_am%mantissa = 0
            end if
        end if
    end if
