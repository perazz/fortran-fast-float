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
fortran (fast_float)                    :  2311.33 MB/s (+/-   6.7 %)   105.37 Mfloat/s
fortran (stdlib to_num)                 :  1079.45 MB/s (+/-   4.0 %)    49.21 Mfloat/s
fortran (str2real)                      :   646.27 MB/s (+/-   0.8 %)    29.46 Mfloat/s
fortran (read *)                        :    56.92 MB/s (+/-   0.9 %)     2.59 Mfloat/s
ffc via fortran interop                 :  4193.98 MB/s (+/-   5.7 %)   191.20 Mfloat/s
```

### canada.txt -- 111k lines, 1.93 MB

```
fortran (fast_float)                    :   977.63 MB/s (+/-   1.1 %)    56.18 Mfloat/s
fortran (stdlib to_num)                 :  1040.21 MB/s (+/-   1.1 %)    59.78 Mfloat/s
fortran (str2real)                      :   461.40 MB/s (+/-   2.4 %)    26.52 Mfloat/s
fortran (read *)                        :    48.51 MB/s (+/-   1.1 %)     2.79 Mfloat/s
ffc via fortran interop                 :  1073.71 MB/s (+/-   2.3 %)    61.70 Mfloat/s
```

### mesh.txt -- 73k lines, 0.54 MB

```
fortran (fast_float)                    :   829.74 MB/s (+/-   1.0 %)   113.03 Mfloat/s
fortran (stdlib to_num)                 :   771.24 MB/s (+/-   0.9 %)   105.06 Mfloat/s
fortran (str2real)                      :   291.15 MB/s (+/-   1.7 %)    39.66 Mfloat/s
fortran (read *)                        :    27.41 MB/s (+/-   1.4 %)     3.73 Mfloat/s
ffc via fortran interop                 :   823.36 MB/s (+/-   0.8 %)   112.16 Mfloat/s
```

## License

Triple-licensed: MIT, Apache-2.0, or BSL-1.0.
