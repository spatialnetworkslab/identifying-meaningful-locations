
# Identifying home locations in human mobility data: an open-source R package for comparison and reproducibility

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/spatialnetworkslab/identifying-meaningful-locations/master?urlpath=rstudio)

This repository contains the data and code for our paper:

> *Identifying home locations in human mobility data: an open-source R
> package for comparison and reproducibilityy*. International Journal of
> Geographical Information Science (IJGIS)

This repository contains all the data and code needed to reproduce the
results and figures in our paper. Although we are not able to share the
raw Twitter data publicly, the aggregated and de-identified Twitter
dataset is found in `analysis/data/derived_data/`. The steps taken to
produce this aggregated dataset can be found in:

  - [analysis/00a-de-identification.Rmd](analysis/00a-de-identification.md):
    Workflow of de-identification approach

The steps needed to identify home locations for users can be found in:

  - [analysis/00b-identifying-home-locations.Rmd](analysis/00b-identifying-home-locations.md):
    Identifying home locations for users by using four built-in
    ‘recipes’ in `homelocator` package

The steps needed to recreate the figures found in the paper can be found
in:

  - [analysis/01-figures-spatial-comparison-of-four-approaches.Rmd](analysis/01-figures-spatial-comparison-of-four-approaches.md):
    Geographical distributions of inferred home locations by four
    different approaches
  - [analysis/02-figures-spatial-view-of-same-hm-users.Rmd](analysis/02-figures-spatial-view-of-same-hm-users.md):
    Geographical distributions of tweets, unique users, inferred homes
    and actual residents in Singapore (2015)
  - [analysis/03-figures-correlation.Rmd](analysis/03-figures-correlation.md):
    The correlation between the normalized number of inferred residents
    and the normalized number of actual residents in Singapore (2015)

Additionally, this repository also includes supplementary documents for
spatial sensitivity test and comparison between raw and de-identified
Twitter datasets. As we are not able to share the raw Twitter dataset,
we only show the detailed analysis procedures and results.

The steps needed to conduct spatial sensitivity test can be found in:

  - [analysis/04-appendix-sensitivity-analysis-on-grid-sizes.Rmd](analysis/04-appendix-sensitivity-analysis-on-grid-sizes.md):
    Spatial sensitivity test on different grid cell sizes

The steps needed to compare raw and de-identified Twitter datasets can
be found in:

  - [analysis/05-appendix-comparison-between-raw-and-deidentified-datasets.Rmd](analysis/05-appendix-comparison-between-raw-and-deidentified-datasets.md):
    Comparison between raw and de-identified Twitter datasets

### Licenses

Text + figures and data:
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

Code: See the [DESCRIPTION](DESCRIPTION) file
