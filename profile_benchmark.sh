#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DATA_DIR="${DATA_DIR:-/Users/federico/code/simple_fastfloat_benchmark/data}"
REPEAT_COUNT="${REPEAT_COUNT:-10}"
MODE="${1:-xctrace}"
TRACE_OUT="${TRACE_OUT:-${ROOT_DIR}/benchmark_profile.trace}"
SAMPLE_OUT="${SAMPLE_OUT:-${ROOT_DIR}/benchmark_sample.txt}"

SDKROOT="${SDKROOT:-$(xcrun --show-sdk-path)}"
export SDKROOT
export FPM_CFLAGS="${FPM_CFLAGS:--O3 -DNDEBUG}"

# Keep optimization on, but emit symbols and preserve frame pointers for profiling.
FFLAGS="${FFLAGS:--O3 -g -fno-omit-frame-pointer -fno-stack-arrays}"

cd "${ROOT_DIR}"

fpm run --profile release --target benchmark_compare --flag "${FFLAGS}" -- -h >/dev/null

BENCH_EXE="$(find build -type f -path '*/app/benchmark_compare' | sort | tail -n 1)"
if [[ -z "${BENCH_EXE}" ]]; then
  echo "failed to locate benchmark_compare executable under build/" >&2
  exit 1
fi

ARGS=(
  -f "${DATA_DIR}/canada.txt"
  -f "${DATA_DIR}/canada_short.txt"
  -f "${DATA_DIR}/mesh.txt"
  -r "${REPEAT_COUNT}"
)

case "${MODE}" in
  xctrace)
    rm -rf "${TRACE_OUT}"
    xcrun xctrace record \
      --template "Time Profiler" \
      --output "${TRACE_OUT}" \
      --launch -- "${BENCH_EXE}" "${ARGS[@]}"
    echo "trace written to ${TRACE_OUT}"
    ;;
  sample)
    "${BENCH_EXE}" "${ARGS[@]}" >/tmp/benchmark_profile_stdout.txt &
    BENCH_PID=$!
    sample "${BENCH_PID}" 5 -mayDie -file "${SAMPLE_OUT}" >/dev/null 2>&1 || true
    wait "${BENCH_PID}"
    echo "sample written to ${SAMPLE_OUT}"
    ;;
  *)
    echo "usage: $0 [xctrace|sample]" >&2
    exit 1
    ;;
esac
