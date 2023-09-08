![](SF_logo.png)

# A Horizon 2020 smartfish study: Birch et al. (2023) Fisheries Research

> Birch, Samantha F., Stephen D. Gregory, David L. Maxwell, Marieke Desender, and Thomas L. Catchpole. 2023. How an Illuminated Headline Affects Catches and Species Separation in a Celtic Sea Mixed Demersal Trawl Fishery. Fisheries Research 268:106832. doi: [10.1016/j.fishres.2023.106832](https://doi.org/10.1016/j.fishres.2023.106832).

This repo holds the manuscript and analysis code for a lights selectivity experiment done as part of the Horizon 2020 SMARTFISH project [https://smartfishh2020.eu/](https://smartfishh2020.eu/).

## Compiling the manuscript

The manuscript can be compiled using Quarto as per this website: [https://quarto.org/docs/get-started/hello/rstudio.html](https://quarto.org/docs/get-started/hello/rstudio.html)

## Re-running analyses

Open the file "h2020-smartfish-ms.qmd", navigate to the R chunk labelled "analysis", set the analysis parts under "choose analyses" you wish to run to 1, e.g., set `do__orgdata <- 0` to run the script to organise the data.

**Note** The complete analysis takes some time to complete (depending on the computer resources and software versions, etc.) - it is suggested to start with individual scripts rather than simply setting analysis parts under "choose analyses" to 1.

## Future improvements

-   Tweedie distribution
-   R-INLA?
