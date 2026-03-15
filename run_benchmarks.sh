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
# also skip UTF-16 results and fpm build chatter
extract_results() {
    grep -E '^\S.+:\s+[0-9]+(\.[0-9]+)?\s+MB/s'
}

# extract_cpp_results: cut everything from "UTF-16" onward, then extract
extract_cpp_results() {
    sed '/UTF-16/,$d' | extract_results
}

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
    fort_out=$(fpm test --profile release --target benchmark_compare -- \
        -f "${filepath}" -r "${REPEAT_COUNT}" 2>&1 || true)
    fort_results=$(echo "${fort_out}" | extract_results || true)

    # Print unified table
    echo "${cpp_results}"
    echo "${fort_results}"
    echo ""
done

echo "=== All benchmarks complete ==="
