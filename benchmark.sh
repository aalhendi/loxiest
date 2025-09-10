#!/bin/bash

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --benchmark-dir)
            BENCHMARK_DIR="$(realpath "$2")"
            shift 2
            ;;
        --results-dir)
            RESULTS_DIR="$(realpath -m "$2")"
            shift 2
            ;;
        --base-binary)
            BASE_BINARY="$(realpath "$2")"
            shift 2
            ;;
        --other-binary)
            OTHER_BINARY="$(realpath "$2")"
            shift 2
            ;;
        --base-name)
            BASE_NAME="$2"
            shift 2
            ;;
        --other-name)
            OTHER_NAME="$2"
            shift 2
            ;;
        --warmup)
            WARMUP="$2"
            shift 2
            ;;
        --runs)
            RUNS="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 --benchmark-dir DIR --results-dir DIR --base-binary PATH --other-binary PATH [OPTIONS]"
            echo "Required:"
            echo "  --benchmark-dir DIR    Directory containing .lox files"
            echo "  --results-dir DIR      Output directory for results"
            echo "  --base-binary PATH     Path to base binary"
            echo "  --other-binary PATH    Path to comparison binary"
            echo "Optional:"
            echo "  --base-name NAME       Name for base binary in results (default: base)"
            echo "  --other-name NAME      Name for other binary in results (default: other)"
            echo "  --warmup N             Number of warmup runs (default: 3)"
            echo "  --runs N               Number of benchmark runs (default: 10)"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Check required arguments
if [[ -z "$BENCHMARK_DIR" || -z "$RESULTS_DIR" || -z "$BASE_BINARY" || -z "$OTHER_BINARY" ]]; then
    echo "Error: Missing required arguments"
    echo "Use --help for usage information"
    exit 1
fi

# Set defaults for optional arguments
BASE_NAME=${BASE_NAME:-"base"}
OTHER_NAME=${OTHER_NAME:-"other"}
WARMUP=${WARMUP:-3}
RUNS=${RUNS:-10}

mkdir -p "$RESULTS_DIR"

echo "# Benchmark Results Summary" > "$RESULTS_DIR/summary.md"
echo "" >> "$RESULTS_DIR/summary.md"

for file in "$BENCHMARK_DIR"/*.lox; do
    filename=$(basename "$file" .lox)
    echo "Benchmarking $filename..."
    
    hyperfine --warmup "$WARMUP" --runs "$RUNS" \
        --export-json "$RESULTS_DIR/${filename}.json" \
        --export-markdown "$RESULTS_DIR/${filename}.md" \
        -n "$BASE_NAME" "$BASE_BINARY $file" \
        -n "$OTHER_NAME" "$OTHER_BINARY $file"
    
    echo "## $filename" >> "$RESULTS_DIR/summary.md"
    tail -n +2 "$RESULTS_DIR/${filename}.md" >> "$RESULTS_DIR/summary.md"
    echo "" >> "$RESULTS_DIR/summary.md"
done

echo "All benchmarks complete! Check $RESULTS_DIR/summary.md for results."