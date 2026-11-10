#!/usr/bin/env bash
# Master script: runs the full Einav & Yariv (2006) replication, start to finish.
# Usage (in terminal, but executed from anywhere):  bash run_all.sh
# Each R script uses paths relative to code/, so we cd there before running them.

set -euo pipefail

# Folder this script lives in the main project folder, regardless of where it is called from
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ROOT/code"

run() {
	echo ""
	echo "=============================================="
	echo "Running $1"
	echo "=============================================="
	Rscript "$1"
}

# 1. Convert the original .dta files to cleaned CSVs (data/125621-V1 -> data/raw)
run dta_to_csv.R

# 2. Figures 1 & 4 and Tables 1 & 4 (output/)
run replication.R

# 3. Table 5 (output/)
run replication_extra.R

echo ""
echo "Full project has been executed; Output is stored in $ROOT/output"
