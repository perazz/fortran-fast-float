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

## License

Triple-licensed: MIT, Apache-2.0, or BSL-1.0.
