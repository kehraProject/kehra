kehra: an R package to collect, assemble and model air pollution, weather and health data
=======

[![DOI](https://zenodo.org/badge/9118/cvitolo/r_kehra.svg)](https://zenodo.org/badge/55284/9118/cvitolo/r_kehra)
[![CRAN Status Badge](http://www.r-pkg.org/badges/version/kehra)](http://cran.r-project.org/web/packages/kehra)
[![CRAN Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/kehra)](http://cran.rstudio.com/web/packages/kehra/index.html)
[![CRAN Monthly Downloads](http://cranlogs.r-pkg.org/badges/kehra)](http://cran.rstudio.com/web/packages/kehra/index.html)

Collect, assemble and model air pollution, weather and health data is a Big Data problem. Here are some utility functions developed as part of the British Council's funded KEHRA project.

**To cite this software:** 

Vitolo C., Tucker A. and Russell A. (2016). kehra: Collect, assemble and model air pollution, weather and health data. R package version 0.1.  https://CRAN.R-project.org/package=kehra. DOI: http://dx.doi.org/10.5281/zenodo.55284

# Dependencies
The kehra package is dependent on a number of CRAN packages. Install them first:

```R
install.packages(c("Hmisc", "raster", "reshape2", "stringr", "sp", "xts", "zoo"))
library(devtools)
```


# Installation
The stable version of the **kehra** package is available from CRAN:

```R
install.packages("kehra")
```

This github repository contains the development version (latest version but potentially unstable), which can be installed via devtools:

```R
install_github("cvitolo/r_kehra", subdir = "kehra")
```

Now, load the kehra package:

```R
library(kehra)
```

# Leave your feedback
I would greatly appreciate if you could leave your feedbacks via email (cvitolodev@gmail.com).
