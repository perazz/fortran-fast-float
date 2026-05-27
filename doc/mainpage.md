# fortran-fast-float {#mainpage}

Eisel-Lemire fast floating-point parsing for Fortran.

A modern Fortran port of Daniel Lemire's [fast_float](https://github.com/fastfloat/fast_float) algorithm
via the single-header C library [ffc.h](https://github.com/kolemannix/ffc.h).
Parses decimal strings into `real64`, `real32`, `int64`, and `int32` values with exact rounding,
typically 4-10x faster than Fortran's native `read(str, *)`.

## API

| Procedure | Description |
|-----------|-------------|
| `parse_double(s [, options] [, first, last])` | Parse `character(*)` to `real64` (pure elemental) |
| `parse_double(s [, options], res)` | Parse to `real64`, populate `parse_result` in `res` |
| `parse_float(s [, options])` | Parse `character(*)` to `real32` (pure elemental) |
| `parse_float(s [, options], res)` | Parse to `real32`, populate `parse_result` in `res` |
| `parse_i64(s, base)` | Parse `character(*)` to `integer(int64)` (pure elemental) |
| `parse_i64(s, base, res)` | Parse to `int64`, populate `parse_result` in `res` |
| `parse_i32(s, base)` | Parse `character(*)` to `integer(int32)` (pure elemental) |
| `parse_i32(s, base, res)` | Parse to `int32`, populate `parse_result` in `res` |

All procedures return the parsed value directly. The `res` variants additionally populate a `parse_result`
containing the cursor position (`pos`) and an `outcome` status code.

## Quick start

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

## Error handling

The `outcome` field reports the parse status:

| Status | Meaning |
|--------|---------|
| `outcomes%OK` | Successful parse |
| `outcomes%INVALID_INPUT` | Unrecognised or empty input |
| `outcomes%OUT_OF_RANGE` | Value overflows or underflows the target type |

## Format options

Parsing behaviour is controlled by `parse_options`:

```fortran
type(parse_options) :: opts
opts = parse_options(format=PRESET_JSON, decimal_point='.')
r = parse_double("1.23e4", x, opts)
```

Built-in presets:

| Preset | Description |
|--------|-------------|
| `PRESET_GENERAL` | Fixed and scientific notation (default) |
| `PRESET_JSON` | JSON-compatible (no Inf/NaN) |
| `PRESET_FORTRAN` | Fortran `D`/`d` exponent notation |

Individual format flags (`FMT_SCIENTIFIC`, `FMT_FIXED`, `FMT_NO_INFNAN`, `FMT_FORTRAN`, `FMT_ALLOW_PLUS`, `FMT_SKIP_WS`) can be combined with `ior`.

## Building

```bash
fpm build
fpm test                    # unit tests
fpm test benchmark_compare  # benchmarks
```

## Benchmarks

Run with `fpm test --profile release --target benchmark_compare` on Apple Silicon (M1 Max).
Data files from [simple_fastfloat_benchmark](https://github.com/lemire/simple_fastfloat_benchmark).
Use `./run_benchmarks.sh` to run the full suite (random + data files) with C++ comparison.

Built with `-O3` (no LTO).

### Random uniform [0,1) -- 100k floats

```
ffc                                     :  1615.05 MB/s (+/- 8.5 %)    76.98 Mfloat/s
ffc via fortran interop                 :  1573.96 MB/s (+/- 1.6 %)    75.02 Mfloat/s
fastfloat (C++)                         :  1556.20 MB/s (+/- 1.3 %)    74.17 Mfloat/s
fortran (fast_float line)               :  1335.51 MB/s (+/- 1.5 %)    63.65 Mfloat/s
fortran (fast_float fast)               :  1308.85 MB/s (+/- 0.9 %)    62.38 Mfloat/s
fortran (fast_float array)              :  1071.00 MB/s (+/- 0.5 %)    51.05 Mfloat/s
fortran (fast_float stream)             :  1057.50 MB/s (+/- 0.9 %)    50.40 Mfloat/s
fortran (stdlib to_num)                 :  1039.68 MB/s (+/- 0.9 %)    49.55 Mfloat/s
abseil                                  :   915.51 MB/s (+/- 2.8 %)    43.64 Mfloat/s
strtod                                  :   787.20 MB/s (+/- 2.6 %)    37.52 Mfloat/s
fortran (str2real)                      :   593.69 MB/s (+/- 0.5 %)    28.30 Mfloat/s
netlib                                  :   404.53 MB/s (+/- 3.6 %)    19.28 Mfloat/s
fortran (read *)                        :    56.19 MB/s (+/- 0.4 %)     2.68 Mfloat/s
```

### canada.txt -- 111k lines, 2.04 MB

```
ffc                                     :  1338.35 MB/s (+/- 2.3 %)    76.91 Mfloat/s
fortran (fast_float line)               :  1186.35 MB/s (+/- 0.3 %)    68.18 Mfloat/s
fortran (fast_float fast)               :  1166.31 MB/s (+/- 0.5 %)    67.02 Mfloat/s
ffc via fortran interop                 :  1110.07 MB/s (+/- 1.5 %)    63.79 Mfloat/s
fastfloat (C++)                         :  1097.76 MB/s (+/- 1.7 %)    63.08 Mfloat/s
fortran (stdlib to_num)                 :  1022.06 MB/s (+/- 1.4 %)    58.73 Mfloat/s
fortran (fast_float array)              :   948.84 MB/s (+/- 0.4 %)    54.53 Mfloat/s
fortran (fast_float stream)             :   934.18 MB/s (+/- 0.5 %)    53.68 Mfloat/s
abseil                                  :   868.45 MB/s (+/- 2.1 %)    49.91 Mfloat/s
strtod                                  :   676.41 MB/s (+/- 0.3 %)    38.87 Mfloat/s
fortran (str2real)                      :   431.74 MB/s (+/- 0.5 %)    24.81 Mfloat/s
netlib                                  :   386.63 MB/s (+/- 1.9 %)    22.22 Mfloat/s
fortran (read *)                        :    49.02 MB/s (+/- 0.5 %)     2.82 Mfloat/s
```

### mesh.txt -- 73k lines, 0.61 MB

```
ffc                                     :   991.77 MB/s (+/- 1.7 %)   135.11 Mfloat/s
ffc via fortran interop                 :   952.06 MB/s (+/- 2.9 %)   129.70 Mfloat/s
fortran (fast_float line)               :   877.26 MB/s (+/- 1.0 %)   119.51 Mfloat/s
fastfloat (C++)                         :   838.88 MB/s (+/- 0.3 %)   114.28 Mfloat/s
fortran (fast_float fast)               :   836.21 MB/s (+/- 0.4 %)   113.91 Mfloat/s
fortran (stdlib to_num)                 :   765.73 MB/s (+/- 2.5 %)   104.31 Mfloat/s
fortran (fast_float array)              :   641.93 MB/s (+/- 0.5 %)    87.45 Mfloat/s
fortran (fast_float stream)             :   592.27 MB/s (+/- 1.0 %)    80.68 Mfloat/s
netlib                                  :   537.69 MB/s (+/- 3.0 %)    73.25 Mfloat/s
strtod                                  :   518.11 MB/s (+/- 0.3 %)    70.58 Mfloat/s
abseil                                  :   407.59 MB/s (+/- 0.5 %)    55.52 Mfloat/s
fortran (str2real)                      :   287.87 MB/s (+/- 1.3 %)    39.22 Mfloat/s
fortran (read *)                        :    27.50 MB/s (+/- 0.5 %)     3.75 Mfloat/s
```

## License

Triple-licensed: MIT, Apache-2.0, or BSL-1.0.
