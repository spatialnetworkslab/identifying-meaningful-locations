
# Identifying meaningful locations in human mobility data: an open-source R package as a framework for comparison and reproducibility

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/spatialnetworkslab/identifying-meaningful-locations/master?urlpath=rstudio)

This repository contains the data and code for our paper:

> Poorthuis, A and Chen, QQ (2020). *Identifying meaningful locations in
> human mobility data: an open-source R package as a framework for
> comparison and reproducibility*. International Journal of Geographical
> Information Science <https://doi.org/xxx/xxx>

<!-- Our pre-print is online here: -->

<!-- > Authors, (YYYY). _Identifying meaningful locations in human mobility data: an open-source R package as a framework for comparison and reproducibility_. Name of journal/book, Accessed 20 Jul 2020. Online at <https://doi.org/xxx/xxx> -->

This repository contains all the data and code needed to reproduce the
results and figures in our paper. Although we are not able to share the
raw Twitter data publicly, the aggregated and de-identified Twitter
dataset is found in `analysis/data/derived_data/`. The steps taken to
produce this aggregated dataset can be found in:

  - [analysis/00a-remove-users.Rmd](analysis/00a-remove-users.md):
    Remove users with too few or too many data points.
  - [analysis/00b-add-noise.Rmd](analysis/00b-add-noise.md): Add a small
    acount of noise to each user.
  - [analysis/00c-geomasking.Rmd](analysis/00c-geomasking.md): Offset
    locations by a random distance (\< 100 meters)
  - [analysis/00d-temporal-masking.Rmd](analysis/00d-temporal-masking.md):
    Shift timestamps by a random number of minutes and swap the day of
    the week witn a similar day (weekday or weekend)
  - [analysis/00e-aggregate-to-hex.Rmd](analysis/00e-aggregate-to-hex.md):
    Aggregate locations to 750m hexagonal grid cells
  - [analysis/00f-remove-grid-user.Rmd](analysis/00f-remove-grid-user.md):
    Remove grids visited by fewer than 5 people or users with inferred
    homes at grid(s) that have fewer than 5 inferred residential
    locations.

The steps needed to recreate the figures found in the paper can be found
in:

  - [analysis/01-figures-spatial-comparison-of-four-approaches.Rmd](analysis/01-figures-spatial-comparison-of-four-approaches.md):
    Geographical distributions of inferred home locations by four
    different approaches
  - [analysis/02-figures-spatial-view-of-shared-users.Rmd](analysis/02-figures-spatial-view-of-shared-users.md):
    Geographical distributions of tweets, unique users, inferred homes
    for shared users and actual residents in Singapore (2015)
  - [analysis/03-figures-correlation.Rmd](analysis/03-correlation.md):
    The correlation between the normalized number of inferred residents
    and the normalized number of actual residents in Singapore (2015).

### How to download or install

You can download the compendium as a zip from from [this URL]().

Or you can install this compendium as an R package, geographyoffashion,
from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("spatialnetworkslab/identifying-meaningful-locations")
```

### Licenses

Text + figures and data:
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

Code: See the [DESCRIPTION](DESCRIPTION) file
