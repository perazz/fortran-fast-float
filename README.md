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

## Profiling

Use `profile_benchmark.sh` to profile the `benchmark_compare` benchmark on macOS:

```bash
./profile_benchmark.sh xctrace   # Time Profiler trace
./profile_benchmark.sh sample    # CLI sampling report
```

Environment overrides: `REPEAT_COUNT`, `DATA_DIR`, `TRACE_OUT`.

## Benchmarks

Run with `fpm test --profile release --target benchmark_compare` on Apple Silicon (M1 Max).
Data files from [simple_fastfloat_benchmark](https://github.com/lemire/simple_fastfloat_benchmark).
Use `./run_benchmarks.sh` to run the full suite (random + data files) with C++ comparison.

### Random uniform [0,1) -- 100k floats, 2.19 MB

```
netlib              (C)                 :   405.14 MB/s (+/-  1.2 %)    19.31 Mfloat/s
strtod              (C)                 :   787.20 MB/s (+/-  1.0 %)    37.52 Mfloat/s
abseil              (C++)               :   919.15 MB/s (+/-  1.0 %)    43.81 Mfloat/s
fastfloat           (C++)               :  1586.45 MB/s (+/-  1.3 %)    75.61 Mfloat/s
ffc                 (C)                 :  1653.72 MB/s (+/-  1.6 %)    78.82 Mfloat/s
fortran (fast_float)                    :  2394.60 MB/s (+/-  8.8 %)   109.17 Mfloat/s
fortran (stdlib to_num)                 :  1078.39 MB/s (+/-  2.7 %)    49.16 Mfloat/s
fortran (str2real)                      :   666.91 MB/s (+/-  0.5 %)    30.40 Mfloat/s
fortran (read *)                        :    58.58 MB/s (+/-  0.5 %)     2.67 Mfloat/s
ffc via fortran interop                 :  4284.08 MB/s (+/- 11.0 %)   195.31 Mfloat/s
```

### canada.txt -- 111k lines, 1.93 MB

```
netlib              (C)                 :   385.05 MB/s (+/-  1.3 %)    22.13 Mfloat/s
strtod              (C)                 :   686.85 MB/s (+/-  1.4 %)    39.47 Mfloat/s
abseil              (C++)               :   868.24 MB/s (+/-  1.6 %)    49.89 Mfloat/s
fastfloat           (C++)               :  1095.24 MB/s (+/-  1.4 %)    62.94 Mfloat/s
ffc                 (C)                 :  1169.13 MB/s (+/-  1.8 %)    67.19 Mfloat/s
fortran (fast_float)                    :  1011.37 MB/s (+/-  1.3 %)    58.12 Mfloat/s
fortran (stdlib to_num)                 :  1074.30 MB/s (+/-  2.3 %)    61.74 Mfloat/s
fortran (str2real)                      :   463.62 MB/s (+/-  1.5 %)    26.64 Mfloat/s
fortran (read *)                        :    49.77 MB/s (+/-  0.5 %)     2.86 Mfloat/s
ffc via fortran interop                 :  1089.43 MB/s (+/-  0.9 %)    62.61 Mfloat/s
```

### mesh.txt -- 73k lines, 0.54 MB

```
netlib              (C)                 :   537.08 MB/s (+/-  2.7 %)    73.17 Mfloat/s
strtod              (C)                 :   523.55 MB/s (+/-  1.5 %)    71.32 Mfloat/s
abseil              (C++)               :   415.44 MB/s (+/-  1.3 %)    56.59 Mfloat/s
fastfloat           (C++)               :   825.16 MB/s (+/-  1.5 %)   112.41 Mfloat/s
ffc                 (C)                 :   948.34 MB/s (+/-  2.5 %)   129.19 Mfloat/s
fortran (fast_float)                    :   846.78 MB/s (+/-  2.4 %)   115.35 Mfloat/s
fortran (stdlib to_num)                 :   802.41 MB/s (+/-  1.0 %)   109.31 Mfloat/s
fortran (str2real)                      :   301.81 MB/s (+/-  0.4 %)    41.11 Mfloat/s
fortran (read *)                        :    28.43 MB/s (+/-  1.2 %)     3.87 Mfloat/s
ffc via fortran interop                 :   853.52 MB/s (+/-  0.9 %)   116.27 Mfloat/s
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
