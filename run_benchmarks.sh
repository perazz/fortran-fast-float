#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPEAT_COUNT="${1:-10}"
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

# Build C++ benchmarks if needed
CPP_BENCH="${CPP_DIR}/build/benchmarks/benchmark"
if [[ ! -x "${CPP_BENCH}" ]]; then
    echo "=== Building C++ benchmarks ==="
    cmake -S "${CPP_DIR}" -B "${CPP_DIR}/build" -DCMAKE_BUILD_TYPE=Release 2>&1 | tail -1
    cmake --build "${CPP_DIR}/build" --target benchmark --parallel 2>&1 | tail -1
fi

# ---- Manual PGO build for maximum performance ----
# fpm's -fPIC and build-hash system prevent PGO from working optimally,
# so we build the benchmark binary manually with full PGO.

BUILD_DIR="${ROOT_DIR}/build/bench_pgo"
PROF_DIR="${BUILD_DIR}/prof"
FC="${FC:-gfortran}"
CC="${CC:-gcc}"

# LTO + aggressive inlining flags
INLINE_FLAGS="--param max-inline-insns-auto=2000 --param max-inline-insns-single=4000 --param large-function-growth=400"
BASE_FFLAGS="-O3 -cpp -flto -fno-range-check ${INLINE_FLAGS}"
BASE_CFLAGS="-O3 -DNDEBUG -flto"
BASE_LDFLAGS="-flto -O3 ${INLINE_FLAGS}"

# Dependency sources
STDLIB_KINDS="${ROOT_DIR}/build/dependencies/stdlib/src/stdlib_kinds.f90"
STDLIB_STR2NUM="${ROOT_DIR}/build/dependencies/stdlib/src/stdlib_str2num.f90"
STR2REAL="${ROOT_DIR}/build/dependencies/str2real/src/str2real_m.f90"

# Ensure fpm dependencies are available
if [[ ! -f "${STDLIB_KINDS}" ]]; then
    echo "=== Fetching fpm dependencies ==="
    cd "${ROOT_DIR}"
    fpm build --profile release 2>&1 | tail -1
fi

compile_all() {
    local extra_flags="$1"
    local fflags="${BASE_FFLAGS} ${extra_flags} -J${BUILD_DIR}"
    local cflags="${BASE_CFLAGS} ${extra_flags} -I${ROOT_DIR}/benchmark"
    local ldflags="${BASE_LDFLAGS} ${extra_flags}"

    $FC $fflags -c "${STDLIB_KINDS}"  -o "${BUILD_DIR}/stdlib_kinds.o"
    $FC $fflags -c "${STDLIB_STR2NUM}" -o "${BUILD_DIR}/stdlib_str2num.o"
    $FC $fflags -c "${STR2REAL}"       -o "${BUILD_DIR}/str2real_m.o"
    $FC $fflags -c "${ROOT_DIR}/src/fast_float_module.F90" -o "${BUILD_DIR}/fast_float_module.o"
    $CC $cflags -c "${ROOT_DIR}/benchmark/ffc_impl.c"      -o "${BUILD_DIR}/ffc_impl.o"
    $CC $cflags -c "${ROOT_DIR}/benchmark/ffc_benchmark.c"  -o "${BUILD_DIR}/ffc_benchmark.o"
    $FC $fflags -c "${ROOT_DIR}/benchmark/ffc_c_bridge.f90" -o "${BUILD_DIR}/ffc_c_bridge.o"
    $FC $fflags -c "${ROOT_DIR}/benchmark/benchmark_compare.f90" -o "${BUILD_DIR}/benchmark_compare.o"
    $FC $ldflags "${BUILD_DIR}"/*.o -o "${BUILD_DIR}/benchmark_compare" 2>&1 | grep -v "warning\|note" || true
}

FORT_BENCH="${BUILD_DIR}/benchmark_compare"

# Rebuild if sources are newer than binary
needs_rebuild=false
if [[ ! -x "${FORT_BENCH}" ]]; then
    needs_rebuild=true
elif [[ "${ROOT_DIR}/src/fast_float_module.F90" -nt "${FORT_BENCH}" ]] || \
     [[ "${ROOT_DIR}/benchmark/benchmark_compare.f90" -nt "${FORT_BENCH}" ]] || \
     [[ "${ROOT_DIR}/benchmark/ffc_benchmark.c" -nt "${FORT_BENCH}" ]] || \
     [[ "${ROOT_DIR}/benchmark/ffc_c_bridge.f90" -nt "${FORT_BENCH}" ]]; then
    needs_rebuild=true
fi

if [[ "${needs_rebuild}" == "true" ]]; then
    mkdir -p "${BUILD_DIR}" "${PROF_DIR}"

    echo "=== Building Fortran benchmark (PGO step 1: instrument) ==="
    compile_all "-fprofile-generate=${PROF_DIR}"

    echo "=== PGO step 2: collecting profile data ==="
    # Train on uniform data
    "${FORT_BENCH}" -m uniform -r 3 >/dev/null 2>&1 || true
    # Train on file data (if available)
    for f in "${DATA_FILES[@]}"; do
        filepath="${DATA_DIR}/${f}"
        [[ -f "${filepath}" ]] && "${FORT_BENCH}" -f "${filepath}" -r 3 >/dev/null 2>&1 || true
    done

    echo "=== PGO step 3: optimized rebuild ==="
    compile_all "-fprofile-use=${PROF_DIR} -fprofile-correction"

    echo ""
fi

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

fort_out=$("${FORT_BENCH}" -m uniform -r "${REPEAT_COUNT}" 2>&1 || true)
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
    fort_out=$("${FORT_BENCH}" -f "${filepath}" -r "${REPEAT_COUNT}" 2>&1 || true)
    fort_results=$(echo "${fort_out}" | extract_results || true)

    # Print unified table
    echo "${cpp_results}"
    echo "${fort_results}"
    echo ""
done

echo "=== All benchmarks complete ==="
