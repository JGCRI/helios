<!-- badges: start -->
[![build](https://github.com/JGCRI/helios/actions/workflows/build.yml/badge.svg?branch=main)](https://github.com/JGCRI/helios/actions/workflows/build.yml)
[![test_coverage](https://github.com/JGCRI/helios/actions/workflows/test_coverage.yml/badge.svg?branch=main)](https://github.com/JGCRI/helios/actions/workflows/test_coverage.yml)
[![docs](https://github.com/JGCRI/helios/actions/workflows/docs.yaml/badge.svg?branch=main)](https://github.com/JGCRI/helios/actions/workflows/docs.yaml)
[![codecov](https://codecov.io/gh/JGCRI/helios/branch/main/graph/badge.svg?token=XQ913U4IYM)](https://codecov.io/gh/JGCRI/helios) 
[![Github All Releases](https://img.shields.io/github/downloads/JGCRI/helios/total.svg)]()
<!-- badges: end -->


<!-- ------------------------>
<!-- ------------------------>
# <a name="Introduction"></a>Introduction
<!-- ------------------------>
<!-- ------------------------>

`helios` is an R package to process heating and cooling degrees for GCAM from various sources such as WRF and CMIP.

<br>

<p align="center">
<a href="https://jgcri.github.io/helios/articles/vignette_helios.html" target="_blank"><img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_user_guide.PNG?raw=true" alt="https://jgcri.github.io/helios/articles/vignette_helios.html" height="60"/></a>
<img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_divider.PNG?raw=true" height="40"/>
</p>

<!-- ------------------------>
<!-- ------------------------>
# <a name="Citation"></a>Citation
<!-- ------------------------>
<!-- ------------------------>

Zhao, M., Khan, Z., Dorheim, K., Vernon, C., 2022. helios - An R package to process heating and cooling degrees for GCAM. (In progress) Journal of Open Source Software, DOI: XXXX

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

Additional steps for UBUNTU from a terminal
```
sudo add-apt-repository ppa:ubuntugis/ppa
sudo apt-get update
sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev
```

Additional steps for MACOSX from a terminal
```
brew install pkg-config
brew install gdal
```

<!-- ------------------------>
<!-- ------------------------>
# <a name="Publications"></a>Related Publications
<!-- ------------------------>
<!-- ------------------------>

- Khan, Z., Iyer, G., Patel, P., Kim, S., Hejazi, M., Burleyson, C. and Wise, M., 2021. Impacts of long-term temperature change and variability on electricity investments. Nature communications, 12(1), pp.1-12.
