# fortran-fast-float {#mainpage}

Eisel-Lemire fast floating-point parsing for Fortran.

A modern Fortran port of Daniel Lemire's [fast_float](https://github.com/fastfloat/fast_float) algorithm
via the single-header C library [ffc.h](https://github.com/perazz/ffc.h).
Parses decimal strings into `real64`, `real32`, `int64`, and `int32` values with exact rounding,
typically 4-10x faster than Fortran's native `read(str, *)`.

## API

| Procedure | Description |
|-----------|-------------|
| `parse_double(s, value [, options] [, first, last])` | Parse `character(*)` to `real64` |
| `parse_float(s, value [, options])` | Parse `character(*)` to `real32` |
| `parse_i64(s, value)` | Parse `character(*)` to `integer(int64)` |
| `parse_i32(s, value)` | Parse `character(*)` to `integer(int32)` |

All procedures return a `parse_result` containing the cursor position (`pos`) and an `outcome` status code.
Pure variants (no `stat` argument) are also available.

## Quick start

```fortran
use fast_float_module, only: parse_double, parse_result, outcomes
implicit none
real(8) :: x
type(parse_result) :: r

r = parse_double("3.14159265358979", x)
if (r%outcome == outcomes%OK) print *, "Parsed:", x
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
fpm test check          # unit tests
fpm test benchmark_compare  # benchmarks
```

## License

Triple-licensed: MIT, Apache-2.0, or BSL-1.0.
