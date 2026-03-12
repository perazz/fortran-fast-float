# Fortran-fast-float Coding Style Guide

This repository adopts the Fortran coding conventions from `/Users/federico/fresco/fresco/CLAUDE.md`, adapted for a standalone `fpm` package instead of the FRESCO CFD codebase.

## General Principles

- Keep code thread-safe. Avoid module-level mutable state unless there is a clear need and the concurrency implications are explicit.
- Prefer modular, object-oriented structure when it improves clarity, but do not force class wrappers where a small pure/procedural module is simpler.
- Keep each module focused on one primary responsibility.
- Do not add tool-generated attribution lines to commits, pull requests, or source headers.
- Do not bump the package version for routine code changes unless the task is explicitly a release.

## Building and Testing

Use `fpm` for all routine development work:

```bash
fpm build
fpm test
fpm run
```

If you add benchmarks, examples, or profiles, document the command in `README.md` or the relevant source file.

## File Headers and Documentation

- Prefer lightweight file headers over large project banners.
- Public modules and nontrivial procedures should use Doxygen-Fortran style comments when the extra context is useful.
- Include `@brief` for public-facing modules and procedures.
- Keep comments concise and technical. Do not restate obvious code.

Example:

```fortran
!> @brief Parse decimal text into binary floating-point values.
module fast_float_parser
```

## Coding Style

- Prefer concise syntax when it remains readable.
- Use one-line conditionals for simple actions, for example `if (ok) call finalize()`.
- Prefer array syntax over explicit loops whenever it is clearer and semantically equivalent.
- Use elemental or pure procedures where appropriate, then apply them with array notation.
- Use `merge`, `all`, `any`, and `count` instead of hand-written scalar loops when they express the intent directly.
- Reserve `forall` for simple vectorizable assignments only; otherwise prefer `do concurrent` or array syntax.
- Keep procedure bodies compact. Split long routines into named helpers instead of accumulating local complexity.

## Naming Conventions

- Module names: lowercase with underscores, for example `fast_float_decimal`.
- Derived types: lowercase with underscores, for example `decimal_parser`.
- Procedures: lowercase with underscores, using verb-oriented names where possible.
- Variables: lowercase with underscores.
- Named constants: uppercase for true constants and status codes.
- Use descriptive names; avoid abbreviations unless they are domain-standard.

## Module Structure

- Put `use` statements at the top of the module or procedure, never in the middle of executable code.
- Prefer `use ..., only:` imports.
- Use `implicit none(type, external)` in modules and procedures.
- Default to `private` at module scope and export only the public API.
- Keep module contents ordered consistently:
  1. `use` statements
  2. `implicit none(type, external)`
  3. visibility declarations
  4. parameters
  5. type definitions
  6. interfaces
  7. procedures

Skeleton:

```fortran
module fast_float_decimal
    use iso_fortran_env, only: int32, int64, real64
    implicit none(type, external)
    private

    public :: decimal_parser

    integer(int32), parameter :: MAX_DIGITS = 19_int32

    type :: decimal_parser
    contains
        procedure :: parse
    end type decimal_parser

contains

    subroutine parse(self, text, value, stat)
        class(decimal_parser), intent(in) :: self
        character(*), intent(in) :: text
        real(real64), intent(out) :: value
        integer(int32), intent(out) :: stat
    end subroutine parse

end module fast_float_decimal
```

## Numeric Types

- Always use explicit kinds for numeric declarations.
- Prefer `iso_fortran_env` named kinds such as `int32`, `int64`, and `real64`.
- Match kinds to the algorithm requirements explicitly; do not rely on default `integer` or default `real` for stored data.
- Use named constants for numeric literals when they carry semantic meaning, for example `ZERO_U64` or `POW10_LIMIT`.

Examples:

```fortran
use iso_fortran_env, only: int32, int64, real64

integer(int32) :: i
integer(int64) :: mantissa
real(real64) :: value

integer(int64), parameter :: TEN_I64 = 10_int64
real(real64), parameter :: ZERO_R64 = 0.0_real64
```

## Procedure Style

- Declare `intent` for every dummy argument.
- Mark procedures `pure` or `elemental` when valid.
- Prefer functions for computations with a single clear result; use subroutines when returning multiple outputs or status values.
- Keep argument order consistent across related APIs.
- Validate edge cases explicitly, especially overflow, underflow, invalid syntax, and rounding boundaries.

## Memory and Performance

- Prefer stack/local temporaries over hidden shared state.
- Avoid unnecessary allocation in hot paths.
- When resizing allocatables, prefer ownership transfer with `move_alloc` over copy-and-discard patterns.
- Optimize only after preserving algorithmic clarity; fast-float code is performance-sensitive, but correctness at boundaries is mandatory.

## Testing

- Add or update `fpm` tests for every behavior change.
- Cover normal values, boundary cases, malformed input, subnormals, infinities, NaNs, signed zero, and exponent extremes where relevant.
- Prefer deterministic tests with explicit expected values and status codes.

## Repository-Specific Guidance

- This project implements Lemire-style fast floating-point parsing in modern Fortran. Favor correctness-preserving translations over clever Fortran-specific rewrites that obscure the original algorithm.
- When mirroring constants or tables from upstream references, keep the naming and provenance clear in comments.
- If a low-level optimization changes semantics, add a test before merging it.
