# Visualize.CRAN.Downloads


## Introduction
This package allows you to visualize the number of downloads for an specific
package in the CRAN repository.



## Features
### Automatic date specification
The user can specify the range of dates to be processed, however the main
function of the package will run a couple of chekcs on these:

1) if no dates are specified it will assume the current date as the end of the
period and a year before as the starting date

2) will reset the range to the first download within the speicied dates, so
that dates previous to any reported download from the CRAN logs are not shown 


### Implementation
It utilizes the `cranlogs` package for accessing the data of the downloads and
the `plotly` package for generating interactive visualizations.

The user can specify different ways to display the information: in a classic
(static) plot, an interactive representation, and/or a combined figure
comparing different packages.
It is possible to specify several packages at the same time, and select the
type of out come to be produced.


## Examples
```
processPckg("ehelp")

processPckg(c("ehelp","plotly","ggplot"), "2001-01-01")

processPckg(c("ehelp","plotly","ggplot"), "2001-01-01", opts="nostatic")

processPckg(c("ehelp","plotly","ggplot"), "2001-01-01", opts=c("nostatic","nocombined","nointeractive"))
```
