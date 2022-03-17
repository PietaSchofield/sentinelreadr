# sentinelreadr

This is a currently a very simple package with few functions to enable the extraction of xml data embedded in RSNTL file which are files with a mix of binary metadata and xml. 

# Installation

```
install.packages("remotes")
remotes::install_github("PietaSchofield/sentinelreadr")
library(sentinelreadr)
```

If you install with build_vignettes=T it will build a vignette with an example of use

```
install.packages("remotes")
remotes::install_github("PietaSchofield/sentinelreadr", build_vignettes=T)
library(sentinelreadr)
vignette("Sentinel-Readr")
```

