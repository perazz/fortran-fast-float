#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DATA_DIR="/Users/federico/code/simple_fastfloat_benchmark/data"
REPEAT_COUNT="${1:-3}"

SDKROOT="${SDKROOT:-$(xcrun --show-sdk-path)}"
export SDKROOT
export FPM_CFLAGS="${FPM_CFLAGS:--O3 -DNDEBUG}"

cd "${ROOT_DIR}"

fpm test --profile release --target benchmark_compare -- \
  -f "${DATA_DIR}/canada.txt" \
  -f "${DATA_DIR}/canada_short.txt" \
  -f "${DATA_DIR}/mesh.txt" \
  -r "${REPEAT_COUNT}"
