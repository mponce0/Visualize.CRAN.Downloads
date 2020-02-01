# Visualize.CRAN.Downloads


## Introduction
This package allows you to visualize the number of downloads for an specific
package in the CRAN repository.

The user can specify different ways to display the information: in a classic
(static) plot, an interactive representation, and/or a combined figure
comparing multiple packages.


## Features
### Automatic date specification and selection
The user can specify the range of dates to be processed, however the main
function of the package will run a couple of checks and adjustments on these:

1) if no dates are specified it will assume the current date as the end of the
period and a year before as the starting date, ie. a period of a year since today;

2) given a range of dates, it will reset the range to the first reported download
within the specified dates, so that dates previous to any reported download from
the CRAN logs are not shown, in this way the package can generate a cleaner and
more meaningful visualization.

### Displaying "moving" statistical estimators
In order to show a closer trend to the time series data of downloads, the package will also display moving avera
ges and moving intervals of confidence. The confidence interval will be shaded in the main plot.

Both features can be turned off, using the corresponding flags in the options: `"noMovAvg"` and `"noConfBands"`.

The moving estimators (average and confidence intervarls) are comoputed using a default of 10 windows to be considered over the indicated period of time, i.e. in a given period of time the algorithm will select 10 windows resulting in an effective size for the moving window of the time range divived by 10.
The confidence interval is determined using the "moving" standard deviation, ie. the standard deviation computed on the moving window. The upper limit of the confidence band is determined by +half standard deviation and the lower band by -half standard deviation in the corresponding window.


## Implementation
Visualize.CRAN.Downloads utilizes the `cranlogs` package for accessing the data of
the downloads and the `plotly` package for generating interactive visualizations.
The basic (static) plots are generated employing R basic capabilities.
The basic plots are saved in the current directory in a PDF file named
*"DWNLDS_**packageName**.pdf"*, where ***'packageName'*** is the actual name of the
package analyzed.
The interactive plots are saved in the current directory in an HTML file named
*"Interactive_DWNLDS_**packageName**.html"*, where ***'packageName'*** is the actual
name of the package analyzed.


## Usage
It is possible to specify several packages at the same time, and indicate
the type of outcome to be produced.

By default the package will generate the static and interactive representations,
this can be turned off by indicating the `"nostatic"` and/or `"nointeractive"` as
options in the arguments of the main function.

### Static Plots
The static plot actually includes 4 different plots: a histogram of downloads vs time,
a histogram of number of downloads, a pulse plot and a download vs time plot.
The default style is to generate these 4 plots in the same figure, but it can be switch
to generate one plot per figure by utilizing the `"nocombined"` option.
In each of the plot a dahsed line is added representing the total average over time.
In the "pulse" plot (third subplot), we added also a shaded region defined by the
total average plus/minus the total standard deviation.
Additionally, moving averages and moving standard deviations computations are
displayed in dotted and dased-dotted lines.
The main plot also displays the total average and the shaded region corresponds to
the confidence interval defined by the moving average plus/minus the moving standard
deviation computed using a window of 1/10 the length of the period of time.
The display of the moving estimators can be turned off, including the `"noMovAvg"` flag;
and the shaded regions can be avoided using the `"noConfBand"` flag.

Two more "fixed" averages are presented in the main plot, indicating the average
number of downloads for the package in the last two "units" of time, eg last month
and last week, or last six-months and last month, etc.
The absolute maximum number of downloads within the period of time,
is also displayed as a filled dot and the actual value.


<!-- ![Example of the static plot generated for the *ehelp* package](man/figures/DWNLDS_ehelp.png) -->

<p float="left">
  <img src="man/figures/DWNLDS_ehelp.png" width="49.50%" />
  <img src="man/figures/DWNLDS_ggplot2.png" width="49.50%" />
</p>



### Comparison Plot
A comparison plot between multiple package should be explicity requested using
the `"compare"` option in the list of arguments.

For using this feature more than one package should be indicated!

The comparison plot will be saved into a PDF file named *"DWNLDS_**packageNames**.pdf"*,
where **packageNames** is the combination of all the packages indicated to process.
When the `"compare"` option is indicated, it will also check for the `"nocombined"`
option to either generate the comparison plot combining all packages in the same
plot or in separated ones, but always within the same file.
Similarly, the `"noMovAvg"` and `"noConfBand"` flags can be used for turning
off the moving averages indicators and overall average ones.

Additionally, when the `"compare"` option is indicated the `processPckg` function
will return a nested list containing in each element a list with the information
of each the packages, ie. date-downloads-package.name.

<!--
![Example of the combined plot generated for the *ggplot2-plotly-gplots-lattice-scatterplot3d-rgl* package](man/figures/DWNLDS_ggplot2-plotly-gplots-lattice-scatterplot3d-rgl.png)
![Example of the combined plot generated for the *ggplot2-plotly-gplots* package](man/figures/DWNLDS_ggplot2-plotly-gplots.png)
-->

<p float="left">
<!--  <object data="https://github.com/mponce0/Visualize.CRAN.Downloads/blob/master/man/figures/DWNLDS_ggplot2-plotly-gplots-lattice-sp3d-rgl.pdf" type="application/pdf" width="700px" height="700px"> -->
  <img src="man/figures/DWNLDS_ggplot2-plotly-gplots-lattice-sp3d-rgl.png" width="49.50%" />
  <!--</object> -->
  <img src="man/figures/DWNLDS_ggplot2-plotly-gplots-lattice.png" width="49.50%" />
</p>



### Interactive Plots
Interactive plots are generated using the `plotly` package and combine two plots in one single html file.
A live example of this can be seen in https://mponce0.github.io/Visualize.CRAN.Downloads/
The left plot will highlight the last month of data, and the plot on the right uses colour and symbols size to represent the respective downloads. The size of the symbols is rescaled with respect to the maximum number of downloads within the given time period, so it actuallty represents relative values.

<aside style="background-color:#000000"> 
<iframe id="igraph" scrolling="no" style="border:none" seamless="seamless" allowtransparency="true" style="background: #000000;"  src="man/figures/Interactive_DWNLDS_ehelp.html" height="525" width="100%"></iframe>
</aside>                                                                                                                                               


### Summary of options

option             | action
------             | -----------
`"nostatic"`       | disables static plots
`"nointeractive"`  | disables interactive plots
`"nocombined"`     | disables combination of static plots, ie. each plot will be a separated figure
`"noConfBand"`    | disables the shading of "confidence bands (regions)"
`"noMovAvg"`   | disables the display of "moving" estimators
`"compare"`     | generates a plot comparing the downloads of multiple packages


---


## Installation

For using the "Visualize.CRAN.Downloads" package, first you will need to
install it.
"Visualize.CRAN.Downloads" requires the `cranlogs` and `plotly` packages,
check to have these already installed before installing `Visualize.CRAN.Downloads`.

The stable version can be downloaded from the CRAN repository:

```
install.packages("Visualize.CRAN.Downloads")
```

To obtain the development version you can get it from the github repository, i.e.

```
# need devtools for installing from the github repo
install.packages("devtools")

# install Visualize.CRAN.Downloads
devtools::install_github("mponce0/Visualize.CRAN.Downloads")

# load Visualize.CRAN.Downloads
library(Visualize.CRAN.Downloads)
```



## Examples
```
processPckg("ehelp")

processPckg(c("ehelp","plotly","ggplot"), "2001-01-01")

processPckg(c("ehelp","plotly","ggplot"), "2001-01-01", opts="nostatic")

processPckg(c("ehelp","plotly","ggplot"), "2001-01-01", opts=c("nointeractive","nocombined"))

pckg.data <- processPckg(c('ggplot2','plotly','gplots','lattice'), '2017-01-01',
opts=c('nointeractive','compare','noConfBand'))

pckg.data <- processPckg(c('plotly','gplots','lattice','scatterplot3d','rgl'), '2017-01-01',
opts=c('nointeractive','compare','noMovAvg','noConfBand'))
```



## Applications
One useful application this package offers is the chance to automatically generate figures reporting the statistics of your favorite package. For such, you can create a `cron` job using the following script.

```
# load library
library(Visualize.CRAN.Downloads)

# query fav. package
processPckg("ehelp")
```
