# cluster_estimation

Arguments:
-f or --file: file that contains data (default: "~/resources/bezdekIris.data.txt"), first and last columns aren't used for distance calculation;
-r or --radius: the radius defining a heighborhood; data points outside this radius have little influence on the potential (default: 1.5).

Examples of using:
lein run -f "full/path/to/file" -r 0.9
lein run --radius 1.5
