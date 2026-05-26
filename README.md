
<!-- README.md is generated from README.Rmd. Please edit that file -->

# musr

<!-- badges: start -->

<!-- badges: end -->

The goal of musr is to analyze and compare musical scores using
polyphonic modeling. It provides tools for extracting pitch-based
features such as roughness, harmonicity, contour, and diatonic
deviation, along with visualization functions for each.

## Installation

You can install the development version of musr like so:

``` r
# not available for installation yet
```

## Example

``` r
library(musr)

# read file
score <- xml2::read_xml("path/to/file.xml")

# parse score
notes_df <- parse_xml(score)
notes_df <- expand_notes_df(notes_df, score, "example_score")

# build time data
time_df <- time_slice(notes_df)
time_df <- time_metrics(time_df)
```
