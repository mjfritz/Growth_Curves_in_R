# Growth_Curves_in_R

An R script, plate template, and tutorial with example data to help process growth curve data (specifically coming off of VersaMax and Tecan Spark plate readers, saved as .xlsx files.)

## The tutorial

Downloading the entire repository and unzipping it allows you to run through the tutorial in an .Rmd notebook. It is helpful if you used Rstudio to create a new project with the folder you unzipped the repository to.

You can also download and open the .html file to read the tutorial and see the output with out running anything yourself.

The tutorial covers how to:
1. Use the plate reader output parsing scripts with the `plate_template.xlsx` file.
2. Plot individual and mean growth curves.
3. Calculate area under the curve.
4. Fit growth curves (either easy linear or Gompertz models via the `growthrates` package) and extract fitted parameters: v<sub>max</sub> `mumax`, lag `lambda`, carrying capacity `K`, along with goodness of fits.
5. Run analysis of variance on the parameters and AUC values.
6. Perform pairwise means tests under combinations of conditions, and plot:
	+ Data points for the measures with means and CLDs of significance patterns.
	+ A pairwise heatmap of means and signifcant differences.

## Examples of plots

The extracted parameters of each individual well as data points with means, confidence intervals, and CLDs:
![Measure Plot](measurePlot.png?raw=true "Measure Plot")

Heatmap of significant pairwise comparisons
![Pairwise Heatmap](pairwiseHeatMap.png?raw=TRUE "Pairwise Heatmap")

## Just want to parse reader output?

You only need `parse_96wellGrowthCurves.R` and the `plate_template.xlsx` file. The main functions you will need are

* `parse_growth_Spark` for the Tecan Spark reader
* `parse_growth_VersaMax_xlsx` for the VersaMax reader (with output saved as .xlsx)