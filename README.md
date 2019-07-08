EconomicComputations
=============

Collection of algorithms used to analyze financial data from stock markets and related instruments. This package implements functionality to calculate various types of returns and test for simmetry.

![Captura1](img/symmetry_plot.png?raw=true "Captura 1")

The resulting analyses and algorithms may be consulted in the file `Applications/Symmetry_measurement_in_time.nb` or a HTML visualization at `Applications/Doc/Symmetry_measurement_in_time.htm`.

The definition of returns may be viewed in the `EconomicComputations.wl` package at the root of the project.

For the symmetry test an additional package is included at `TnSymmetryStatistic` that needs to be manually compiled.

## Algorithms
### Trend Returns
Trend returns are calculated by first obtaining the trend duration, that is, the length in days of each elemental trend, which is obtained by grouping same sign returns.
![trendDuration](img/td.png?raw=true "Trend Duration")
With the durations, trend returns are calculated as the logarithmic difference of the prices at their start and end points.
![trendReturns](img/tr.png?raw=true "Trend Returns")

### Velocity Trend Returns
TVReturns are calculated as the division between trend return and its corresponding trend duration.
![velocityTR](img/vtr.png?raw=true "Velocity")

Requirements
========

* [Mathematica](https://www.wolfram.com/mathematica/) (Versión 10 or greater)
* A Unix-like enviroment to compile the TnSymmetryStatistic package.

