# Eyelinker: a package for reading EyeLink data

[![CRAN Version](http://www.r-pkg.org/badges/version/eyelinker)](https://cran.rstudio.com/web/packages/eyelinker)

Turns horrible EyeLink .asc files into less horrible R data structures.

**NOTE**: This branch is currently under development, and the internal ASC-reading logic has been
rewritten entirely to improve compatibility and speed. Here is a short-list of the changes:

 - `read.asc` is now 4 to 5x faster than before
 - Rows with missing eye data now retain non-missing values instead of replacing all with NA
 - ASC files with INPUT/BUTTON data are now supported
 - Is able to handle ASC files with malformed START/END blocks
 - The `$info` table now contains date of recording, tracker model, tracker mount type, display
   resolution, and sample rate information.

However, this new version has only been tested with a handful of different .ASC files, and it is
possible that this version breaks support for certain files workflows. If you encounter any issues
with this version, please let me know and I'll do my best to fix them.

To install this version of Eyelinker, you can use the `devtools` package:

```r
require(devtools)
devtools::install_github("a-hurst/eyelinker@simplified")
```

### Usage

```r
library(eyelinker)

# Example file from SR research that ships with the package
fpath <- system.file("extdata/mono500.asc.gz", package = "eyelinker")
dat <- read.asc(fpath)
plot(dat$raw$time, dat$raw$xp,xlab = "Time (ms)", ylab = "Eye position along x-axis (pix)")

# For more info:
vignette("basics", package = "eyelinker")
```

Author: Simon Barthelmé, CNRS, Gipsa-lab. See also: [cili](https://github.com/beOn/cili).
