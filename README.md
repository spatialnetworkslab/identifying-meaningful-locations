
# Identifying meaningful locations in human mobility data: an open-source R package as a framework for comparison and reproducibility

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/spatialnetworkslab/identifying-meaningful-locations/master?urlpath=rstudio)

This repository contains the data and code for our paper:

> Poorthuis, A and Chen, QQ (2020). *Identifying meaningful locations in
> human mobility data: an open-source R package as a framework for
> comparison and reproducibility*. International Journal of Geographical
> Information Science <https://doi.org/xxx/xxx>

<!-- Our pre-print is online here: -->

<!-- > Authors, (YYYY). _Identifying meaningful locations in human mobility data: an open-source R package as a framework for comparison and reproducibility_. Name of journal/book, Accessed 21 Jul 2020. Online at <https://doi.org/xxx/xxx> -->

This repository contains all the data and code needed to reproduce the
results and figures in our paper. Although we are not able to share the
raw Twitter data publicly, the aggregated and de-identified Twitter
dataset is found in `analysis/data/derived_data/`. The steps taken to
produce this aggregated dataset can be found in:

  - [analysis/00a-de-identification.Rmd](analysis/00a-de-identification.md):
    Workflow of de-identification approach

The steps needed to recreate the figures found in the paper can be found
in:

  - [analysis/01-identifying-home-locations.Rmd](analysis/01-identifying-home-locations.md):
    Identifying home locations for users by using four built-in
    ‘recipes’ in
    [homelocator](https://github.com/spatialnetworkslab/homelocator)
    package

  - [analysis/01-figures-spatial-comparison-of-four-approaches.Rmd](analysis/01-figures-spatial-comparison-of-four-approaches.md):
    Geographical distributions of inferred home locations by four
    different approaches

  - [analysis/02-figures-spatial-view-of-same-hm-users.Rmd](analysis/02-figures-spatial-view-of-same-hm-users.md):
    Geographical distributions of tweets, unique users, inferred homes
    and actual residents in Singapore (2015)

  - [analysis/03-figures-correlation.Rmd](analysis/03-correlation.md):
    The correlation between the normalized number of inferred residents
    and the normalized number of actual residents in Singapore (2015)

### How to download or install

You can download the compendium as a zip from from [this
URL](https://github.com/spatialnetworkslab/identifying-meaningful-locations/archive/master.zip).

Or you can install this compendium as an R package,
identifyingmeaningfullocations, from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("spatialnetworkslab/identifying-meaningful-locations")
```

### Licenses

Text + figures and data:
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

Code: See the [DESCRIPTION](DESCRIPTION) file
