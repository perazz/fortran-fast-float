# fortran-fast-float

A modern Fortran port of the [Eisel-Lemire fast float parsing algorithm](https://github.com/fastfloat/fast_float), via the C99 single-header port [ffc.h](https://github.com/kolemannix/ffc.h).

Parses ASCII decimal strings into `real32`, `real64`, `int32`, and `int64` values with exact rounding, typically 4-10x faster than `read(str, *)`.

## Getting started

Add to your `fpm.toml`:

```toml
[dependencies]
fortran-fast-float = { git = "https://github.com/perazz/fortran-fast-float" }
```

## Usage

```fortran
use fast_float_module, only: parse_double, parse_float, parse_i64, parse_i32

real(real64) :: d
real(real32) :: f
integer(int64) :: i8
integer(int32) :: i4

! Basic parsing (pure elemental)
d = parse_double("3.14159265358979323")
f = parse_float("-1.25e-3")
i8 = parse_i64("9223372036854775807", base=10)
i4 = parse_i32("FF", base=16)
```

### Error handling

Standard variants return a `parse_result` with position and outcome:

```fortran
use fast_float_module, only: parse_double, parse_result, outcomes

type(parse_result) :: res
real(real64) :: val

val = parse_double("1.0e999", res=res)
if (res%outcome == outcomes%OUT_OF_RANGE) print *, "overflow"
if (res%outcome == outcomes%INVALID_INPUT) print *, "bad input"
```

### Format options

```fortran
use fast_float_module, only: parse_double, parse_options, &
    PRESET_GENERAL, PRESET_JSON, PRESET_FORTRAN

! Fortran D-exponent notation
d = parse_double("1.5D-10", parse_options(format=PRESET_FORTRAN))

! Custom decimal separator
d = parse_double("3,14", parse_options(decimal_point=','))
```

## Building

```bash
fpm build
fpm test
```

## Benchmarks

Run with `fpm test --profile release --target benchmark_compare` on Apple Silicon (M1 Max).
Data files from [simple_fastfloat_benchmark](https://github.com/lemire/simple_fastfloat_benchmark).
Use `./run_benchmarks.sh` to run the full suite (random + data files) with C++ comparison.

### Random uniform [0,1) -- 100k floats, 2.10 MB

```
netlib              (C)                 :   403.76 MB/s (+/- 1.5 %)    19.24 Mfloat/s
strtod              (C)                 :   786.63 MB/s (+/- 1.3 %)    37.49 Mfloat/s
abseil              (C++)               :   917.36 MB/s (+/- 0.9 %)    43.72 Mfloat/s
fastfloat           (C++)               :  1586.85 MB/s (+/- 1.3 %)    75.63 Mfloat/s
ffc                 (C)                 :  1650.41 MB/s (+/- 1.3 %)    78.66 Mfloat/s
fortran (fast_float)                    :   876.76 MB/s (+/- 0.7 %)    41.79 Mfloat/s
fortran (stdlib to_num)                 :  1066.10 MB/s (+/- 1.0 %)    50.81 Mfloat/s
fortran (str2real)                      :   609.20 MB/s (+/- 0.7 %)    29.04 Mfloat/s
fortran (read *)                        :    56.90 MB/s (+/- 0.4 %)     2.71 Mfloat/s
ffc via fortran interop                 :  1493.30 MB/s (+/- 1.4 %)    71.17 Mfloat/s
```

### canada_short.txt -- 111k lines, 0.70 MB

```
netlib              (C)                 :   546.31 MB/s (+/- 4.0 %)   101.50 Mfloat/s
strtod              (C)                 :   367.00 MB/s (+/- 0.9 %)    68.19 Mfloat/s
abseil              (C++)               :   361.36 MB/s (+/- 1.2 %)    67.14 Mfloat/s
fastfloat           (C++)               :   566.31 MB/s (+/- 1.0 %)   105.22 Mfloat/s
ffc                 (C)                 :   704.02 MB/s (+/- 1.1 %)   130.81 Mfloat/s
fortran (fast_float)                    :   638.99 MB/s (+/- 1.4 %)   118.72 Mfloat/s
fortran (stdlib to_num)                 :   710.33 MB/s (+/- 3.1 %)   131.98 Mfloat/s
fortran (str2real)                      :   243.62 MB/s (+/- 0.4 %)    45.27 Mfloat/s
fortran (read *)                        :    22.14 MB/s (+/- 0.6 %)     4.11 Mfloat/s
ffc via fortran interop                 :   598.70 MB/s (+/- 4.8 %)   111.24 Mfloat/s
```

### canada.txt -- 111k lines, 2.04 MB

```
netlib              (C)                 :   384.11 MB/s (+/- 0.9 %)    22.07 Mfloat/s
strtod              (C)                 :   686.30 MB/s (+/- 1.0 %)    39.44 Mfloat/s
abseil              (C++)               :   868.22 MB/s (+/- 1.2 %)    49.89 Mfloat/s
fastfloat           (C++)               :  1095.50 MB/s (+/- 1.1 %)    62.95 Mfloat/s
ffc                 (C)                 :  1169.13 MB/s (+/- 1.5 %)    67.19 Mfloat/s
fortran (fast_float)                    :  1036.86 MB/s (+/- 0.7 %)    59.58 Mfloat/s
fortran (stdlib to_num)                 :  1038.53 MB/s (+/- 1.0 %)    59.68 Mfloat/s
fortran (str2real)                      :   452.02 MB/s (+/- 0.4 %)    25.98 Mfloat/s
fortran (read *)                        :    49.86 MB/s (+/- 0.7 %)     2.87 Mfloat/s
ffc via fortran interop                 :  1048.10 MB/s (+/- 1.6 %)    60.23 Mfloat/s
```

### mesh.txt -- 73k lines, 0.61 MB

```
netlib              (C)                 :   537.91 MB/s (+/- 2.1 %)    73.28 Mfloat/s
strtod              (C)                 :   522.81 MB/s (+/- 1.2 %)    71.22 Mfloat/s
abseil              (C++)               :   415.05 MB/s (+/- 1.3 %)    56.54 Mfloat/s
fastfloat           (C++)               :   825.10 MB/s (+/- 1.1 %)   112.40 Mfloat/s
ffc                 (C)                 :   947.22 MB/s (+/- 1.3 %)   129.04 Mfloat/s
fortran (fast_float)                    :   885.97 MB/s (+/- 0.9 %)   120.69 Mfloat/s
fortran (stdlib to_num)                 :   796.45 MB/s (+/- 1.2 %)   108.50 Mfloat/s
fortran (str2real)                      :   298.61 MB/s (+/- 2.4 %)    40.68 Mfloat/s
fortran (read *)                        :    28.21 MB/s (+/- 0.6 %)     3.84 Mfloat/s
ffc via fortran interop                 :   838.82 MB/s (+/- 2.1 %)   114.27 Mfloat/s
```

## Acknowledgements

This library is a Fortran translation of the Eisel-Lemire algorithm. Credit goes to:

- **[fast_float](https://github.com/fastfloat/fast_float)** by Daniel Lemire and contributors -- the original C++ implementation. Licensed under Apache-2.0, BSL-1.0, and MIT.
- **[ffc.h](https://github.com/kolemannix/ffc.h)** by Koleman Nix and contributors -- the C99 single-header port used as the direct reference for this Fortran translation. Licensed under Apache-2.0, BSL-1.0, and MIT.

## License

Licensed under your choice of:

- [Apache License, Version 2.0](LICENSE-APACHE)
- [Boost Software License, Version 1.0](LICENSE-BOOST)
- [MIT License](LICENSE-MIT)

matching the upstream projects.
