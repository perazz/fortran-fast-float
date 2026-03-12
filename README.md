# fortran-fast-float

## Profiling

Use `profile_benchmark.sh` to profile the `benchmark_compare` test target with macOS tools while keeping optimization enabled and debug symbols available.

Time Profiler trace:

```bash
./profile_benchmark.sh xctrace
```

CLI sampling report:

```bash
./profile_benchmark.sh sample
```

Useful environment overrides:

```bash
REPEAT_COUNT=30 ./profile_benchmark.sh xctrace
DATA_DIR=/path/to/data ./profile_benchmark.sh sample
TRACE_OUT=/tmp/bench.trace ./profile_benchmark.sh xctrace
```

The script builds the benchmark with `-O3 -g -fno-omit-frame-pointer -fno-stack-arrays`, locates the hashed `fpm` test executable under `build/`, and runs it against the three benchmark datasets.
