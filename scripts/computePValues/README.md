# Recompute the pValues for CoMap

## What it does

Recompute the pValues for CoMap using different bins (either Gamma or balanced).

## Inputs

* 1st argument: number of bins (default is 10 in CoMap)

> nBins = int(sys.argv[1])

* 2nd argument: folder where the analyses files are located: e.g.: "/home/meyerx/Projets/CoevBenchmarks/Simulations"

> simFolder = sys.argv[2]

* File names (to change inside the python script). Default file names -> Change to match your analyses.

> POSITION_FILE_NAME="out_map_infos.txt"

> PAIR_FILE_NAME="out_stats.txt"

> SIMU_FILE_NAME="out_stats_null.txt"


## Output

New file created with three new columns at the end:
1. pValue with uniformly distributed bins for Nmin  (equivalent to CoMap),
2. pValue with gamma distributed bins for Nmin
3.pValues with balanced bins for Nmin (i.e., same amount of simulation per bins).

> NEW_PAIR_FILE_NAME="out_stats_recomp.txt"

**The balanced variant is recommended.**
