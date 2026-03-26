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
fortran (fast_float)                    :  1986.82 MB/s (+/-   5.4 %)    90.58 Mfloat/s
fortran (stdlib to_num)                 :   978.78 MB/s (+/-  28.3 %)    44.62 Mfloat/s
fortran (str2real)                      :   603.59 MB/s (+/-  22.9 %)    27.52 Mfloat/s
fortran (read *)                        :    53.20 MB/s (+/-  17.6 %)     2.43 Mfloat/s
ffc via fortran interop                 :  3966.46 MB/s (+/-   8.9 %)   180.83 Mfloat/s
```

### canada.txt -- 111k lines, 1.93 MB

```
fortran (fast_float)                    :   948.84 MB/s (+/-   0.9 %)    54.53 Mfloat/s
fortran (stdlib to_num)                 :  1009.26 MB/s (+/-   0.3 %)    58.00 Mfloat/s
fortran (str2real)                      :   440.19 MB/s (+/-  36.0 %)    25.30 Mfloat/s
fortran (read *)                        :    44.40 MB/s (+/-  33.8 %)     2.55 Mfloat/s
ffc via fortran interop                 :  1026.40 MB/s (+/-   0.3 %)    58.98 Mfloat/s
```

### mesh.txt -- 73k lines, 0.54 MB

```
fortran (fast_float)                    :   809.68 MB/s (+/-   0.4 %)   110.30 Mfloat/s
fortran (stdlib to_num)                 :   751.77 MB/s (+/-   0.4 %)   102.41 Mfloat/s
fortran (str2real)                      :   285.41 MB/s (+/-   0.1 %)    38.88 Mfloat/s
fortran (read *)                        :    26.86 MB/s (+/-   5.4 %)     3.66 Mfloat/s
ffc via fortran interop                 :   803.61 MB/s (+/-   0.7 %)   109.47 Mfloat/s
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
