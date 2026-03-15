#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CPP_DIR="${CPP_BENCH_DIR:-/Users/federico/code/simple_fastfloat_benchmark}"
DATA_DIR="${CPP_DIR}/data"
REPEAT_COUNT="${1:-5}"
DATA_FILES=("canada.txt" "canada_short.txt" "mesh.txt" "contrived.txt")

SDKROOT="${SDKROOT:-$(xcrun --show-sdk-path)}"
export SDKROOT
export FPM_CFLAGS="${FPM_CFLAGS:--O3 -DNDEBUG}"

RANDOM_MODELS=(
    "uniform"
    "one_over_rand32"
    "simple_uniform32"
    "simple_int32"
    "int_e_int"
    "simple_int64"
    "bigint_int_dot_int"
    "big_ints"
)

# Build C++ benchmarks if needed
CPP_BENCH="${CPP_DIR}/build/benchmarks/benchmark"
CPP_BENCH32="${CPP_DIR}/build/benchmarks/benchmark32"
if [[ ! -x "${CPP_BENCH}" ]] || [[ ! -x "${CPP_BENCH32}" ]]; then
    echo "=== Building C++ benchmarks (benchmark + benchmark32) ==="
    cmake -S "${CPP_DIR}" -B "${CPP_DIR}/build" -DCMAKE_BUILD_TYPE=Release
    cmake --build "${CPP_DIR}/build" --target benchmark benchmark32 --parallel
fi

# Build the Fortran benchmark
cd "${ROOT_DIR}"
echo "=== Building Fortran benchmark ==="
fpm build --profile release

# ================================================================
#  Section 1: File-based (C++ + Fortran side by side)
# ================================================================
echo ""
echo "################################################################"
echo "  Section 1: File-based benchmarks (C++ + Fortran)"
echo "################################################################"

for f in "${DATA_FILES[@]}"; do
    filepath="${DATA_DIR}/${f}"
    [[ -f "${filepath}" ]] || { echo "WARNING: ${filepath} not found, skipping"; continue; }

    echo ""
    echo "================================================================"
    echo "  ${f}"
    echo "================================================================"

    echo "--- C++ (fast_float / strtod / abseil / ffc) ---"
    "${CPP_BENCH}" -f "${filepath}"

    echo ""
    echo "--- Fortran (fast_float / stdlib / str2real / ffc.h via C) ---"
    fpm test --profile release --target benchmark_compare -- \
        -f "${filepath}" -r "${REPEAT_COUNT}"
done

# ================================================================
#  Section 2: Malformed input (C++ only)
# ================================================================
echo ""
echo "################################################################"
echo "  Section 2: Malformed input (C++ only)"
echo "################################################################"

malformed="${DATA_DIR}/malformed_numbers.txt"
if [[ -f "${malformed}" ]]; then
    echo "--- C++ malformed_numbers.txt (expecting errors) ---"
    "${CPP_BENCH}" -f "${malformed}" -e
else
    echo "WARNING: ${malformed} not found, skipping"
fi

# ================================================================
#  Section 3: Random models (C++ only)
# ================================================================
echo ""
echo "################################################################"
echo "  Section 3: Random models (C++ only)"
echo "################################################################"

for model in "${RANDOM_MODELS[@]}"; do
    echo ""
    echo "================================================================"
    echo "  Random model: ${model}"
    echo "================================================================"

    "${CPP_BENCH}" -m "${model}"

    echo ""
    echo "--- concise ---"
    "${CPP_BENCH}" -m "${model}" -c
done

# ================================================================
#  Section 4: Float32 (C++ only, benchmark32)
# ================================================================
echo ""
echo "################################################################"
echo "  Section 4: Float32 file-based (C++ benchmark32)"
echo "################################################################"

for f in "${DATA_FILES[@]}"; do
    filepath="${DATA_DIR}/${f}"
    [[ -f "${filepath}" ]] || { echo "WARNING: ${filepath} not found, skipping"; continue; }

    echo ""
    echo "================================================================"
    echo "  float32 — ${f}"
    echo "================================================================"

    "${CPP_BENCH32}" -f "${filepath}"
done

echo ""
echo "################################################################"
echo "  Section 4b: Float32 random models (C++ benchmark32)"
echo "################################################################"

for model in "${RANDOM_MODELS[@]}"; do
    echo ""
    echo "================================================================"
    echo "  float32 random model: ${model}"
    echo "================================================================"

    "${CPP_BENCH32}" -m "${model}"

    echo ""
    echo "--- concise ---"
    "${CPP_BENCH32}" -m "${model}" -c
done

echo ""
echo "=== All benchmarks complete ==="
