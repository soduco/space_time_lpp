
# spacetimeLPP R package

## Space-time point pattern analysis on planar network

spacetimeLPP is an R package facilitating the analysis of point patterns
on planar networks in time. In input, spatial data are considered in
`sf` format and outputs are in `tidy` formats to simplify the use of the
package. However, main calculations, e.g. distance matrices between
points on network, are performed using the `spatstat` package for its
computational speed.

## Installing

Install package from github:

``` r
library(remotes)
install_github(repo = "soduco/space_time_lpp")
```
