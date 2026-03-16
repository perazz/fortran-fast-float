#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPEAT_COUNT="${1:-5}"
DATA_FILES=("canada_short.txt" "canada.txt" "mesh.txt")

# Locate or clone the C++ benchmark repo
if [[ -n "${CPP_BENCH_DIR:-}" ]]; then
    CPP_DIR="${CPP_BENCH_DIR}"
elif [[ -d "${ROOT_DIR}/build/simple_fastfloat_benchmark" ]]; then
    CPP_DIR="${ROOT_DIR}/build/simple_fastfloat_benchmark"
else
    echo "=== Cloning simple_fastfloat_benchmark ==="
    CPP_DIR="${ROOT_DIR}/build/simple_fastfloat_benchmark"
    git clone --depth 1 https://github.com/lemire/simple_fastfloat_benchmark.git "${CPP_DIR}"
fi
DATA_DIR="${CPP_DIR}/data"

# macOS SDK (needed by fpm C compilation)
if command -v xcrun &>/dev/null; then
    SDKROOT="${SDKROOT:-$(xcrun --show-sdk-path)}"
    export SDKROOT
fi
export FPM_CFLAGS="${FPM_CFLAGS:--O3 -DNDEBUG}"

# Build C++ benchmarks if needed
CPP_BENCH="${CPP_DIR}/build/benchmarks/benchmark"
if [[ ! -x "${CPP_BENCH}" ]]; then
    echo "=== Building C++ benchmarks ==="
    cmake -S "${CPP_DIR}" -B "${CPP_DIR}/build" -DCMAKE_BUILD_TYPE=Release 2>&1 | tail -1
    cmake --build "${CPP_DIR}/build" --target benchmark --parallel 2>&1 | tail -1
fi

# Build the Fortran benchmark
cd "${ROOT_DIR}"
echo "=== Building Fortran benchmark ==="
fpm build --profile release 2>&1 | tail -1
echo ""

# extract_results: keep only lines matching "label : NNN MB/s"
extract_results() {
    grep -E '^\S.+:\s+[0-9]+(\.[0-9]+)?\s+MB/s'
}

# extract_cpp_results: cut everything from "UTF-16" onward, then extract
extract_cpp_results() {
    sed '/UTF-16/,$d' | extract_results
}

# ---- Random uniform [0,1) benchmark ----
echo "================================================================"
echo "  Random uniform [0,1)  (100000 floats)"
echo "================================================================"

cpp_out=$("${CPP_BENCH}" 2>&1 || true)
cpp_results=$(echo "${cpp_out}" | extract_cpp_results || true)

fort_out=$(fpm run --profile release --target benchmark_compare -- \
    -m uniform -r "${REPEAT_COUNT}" 2>&1 || true)
fort_results=$(echo "${fort_out}" | extract_results || true)

echo "${cpp_results}"
echo "${fort_results}"
echo ""

# ---- File-based benchmarks ----
for f in "${DATA_FILES[@]}"; do
    filepath="${DATA_DIR}/${f}"
    [[ -f "${filepath}" ]] || { echo "WARNING: ${filepath} not found, skipping"; continue; }

    # Get file metadata
    nlines=$(wc -l < "${filepath}" | tr -d ' ')
    volume=$(wc -c < "${filepath}" | tr -d ' ')
    volume_mb=$(python3 -c "print(f'{${volume}/1048576:.4f}')")

    echo "================================================================"
    echo "  ${f}  (${nlines} lines, ${volume_mb} MB)"
    echo "================================================================"

    # Run C++ benchmark, capture result lines
    cpp_out=$("${CPP_BENCH}" -f "${filepath}" 2>&1 || true)
    cpp_results=$(echo "${cpp_out}" | extract_cpp_results || true)

    # Run Fortran benchmark, capture result lines
    fort_out=$(fpm run --profile release --target benchmark_compare -- \
        -f "${filepath}" -r "${REPEAT_COUNT}" 2>&1 || true)
    fort_results=$(echo "${fort_out}" | extract_results || true)

    # Print unified table
    echo "${cpp_results}"
    echo "${fort_results}"
    echo ""
done

echo "=== All benchmarks complete ==="
