# Spatial-Density-Scoring
Extract spatial density score for a set of points based on a 2-dimensional spatial density / heat map.


If you are simply looking to create heatmaps, there is a simpler method for doing so at : https://github.com/LMcDiffett/DensityHeatMap .
The raw data behind this density is sourced from the FARS database, maintained by NHTSA at : https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars .  The data being imported into this particular code has already been cleaned and transformed.

This particular code will allow you to actually extract the value of the density at a given point instead of simply graphing the density.  If you wish to create a composite density from multiple distributions, this can be done using a few extra lines of code (identified in line 65 of the R script).

Output:
![Spatial Scoring](https://raw.githubusercontent.com/LMcDiffett/Spatial-Density-Scoring/master/Spatial_Density_Scoring.png)
