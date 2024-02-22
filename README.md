<!-- badges: start -->
[![build](https://github.com/JGCRI/helios/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/JGCRI/helios/actions/workflows/build.yml)
[![test_coverage](https://github.com/JGCRI/helios/actions/workflows/test_coverage.yml/badge.svg?branch=main)](https://github.com/JGCRI/helios/actions/workflows/test_coverage.yml)
[![docs](https://github.com/JGCRI/helios/actions/workflows/docs.yaml/badge.svg?branch=main)](https://github.com/JGCRI/helios/actions/workflows/docs.yaml)
[![codecov](https://codecov.io/gh/JGCRI/helios/branch/main/graph/badge.svg?token=XQ913U4IYM)](https://codecov.io/gh/JGCRI/helios) 
[![Github All Releases](https://img.shields.io/github/downloads/JGCRI/helios/total.svg)]()
[![DOI](https://joss.theoj.org/papers/10.21105/joss.06033/status.svg)](https://doi.org/10.21105/joss.06033)
[![DOI](https://zenodo.org/badge/429143355.svg)](https://zenodo.org/doi/10.5281/zenodo.8170310)
<!-- badges: end -->


<!-- ------------------------>
<!-- ------------------------>
# <a name="Introduction"></a>Introduction
<!-- ------------------------>
<!-- ------------------------>

`helios` is an R package to process heating and cooling degrees for the Global Change Analysis Model (GCAM) from various sources such as WRF and CMIP.

<br>

<p align="center">
<a href="https://jgcri.github.io/helios/" target="_blank"><img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_user_guide.PNG?raw=true" 
alt="https://jgcri.github.io/helios/" height="60"/></a>
<img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_divider.PNG?raw=true" height="40"/>
</p>

<!-- ------------------------>
<!-- ------------------------>
# <a name="Citation"></a>Citation
<!-- ------------------------>
<!-- ------------------------>

Zhao, M., Khan, Z., Dorheim, K., Vernon, C., (2024). helios: An R package to process heating and cooling degrees for GCAM. Journal of Open Source Software, 9(94), 6033, https://doi.org/10.21105/joss.06033

<!-- ------------------------>
<!-- ------------------------>
# <a name="InstallGuide"></a>Installation Guide
<!-- ------------------------>
<!-- ------------------------>

1. Download and install:
    - R (https://www.r-project.org/)
    - R studio (https://www.rstudio.com/)  
    
    
2. Open R studio:

```r
install.packages("devtools")
devtools::install_github("JGCRI/helios")
```
or
```r
install.packages("remotes")
remotes::install_github("JGCRI/helios")
```

Additional steps for UBUNTU from a terminal
```
sudo add-apt-repository ppa:ubuntugis/ppa
sudo apt-get update
sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev libmagick++-dev
```

Additional steps for MACOSX from a terminal
```
brew install pkg-config
brew install gdal
```
<br>

<!-- ------------------------>
<!-- ------------------------>
# <a name="GettingStarted"></a>Getting Started
<!-- ------------------------>
<!-- ------------------------>

`helios` is an R package that calculates heating and cooling degrees using high temporal and spatial resolution of climate data and population data. `helios` currently supports WRF data and ISIMIP-CMIP data, or same format with any of them. For more details:

* [GCAM-Regions Tutorial](https://jgcri.github.io/helios/articles/vignette_gcam-regions.html) introduces how to use `helios` to calculate heating and cooling degree-days for 32 global regions in GCAM.
* [GCAM-USA Tutorial](https://jgcri.github.io/helios/articles/vignette_gcam-usa.html) introduces how to use `helios` to calculate heating and cooling degree-hours for CONUS in GCAM-USA.

<!-- ------------------------>
<!-- ------------------------>
# <a name="Publications"></a>Related Publications
<!-- ------------------------>
<!-- ------------------------>

- Khan, Z., Iyer, G., Patel, P., Kim, S., Hejazi, M., Burleyson, C. and Wise, M., 2021. Impacts of long-term temperature change and variability on electricity investments. Nature communications, 12(1), pp.1-12.


<!-- ------------------------>
<!-- ------------------------>
# <a name="Contributing"></a>Contributing
<!-- ------------------------>
<!-- ------------------------>

Whether you find a typo in the documentation, find a bug, or want to develop functionality that you think will make `helios` more robust, you are welcome to contribute! The [contributing](https://github.com/JGCRI/helios/blob/main/CONTRIBUTING.md) page will walk you through processes to contribute to `helios`.
