
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inspector: Validation of Arguments and Objects in User-Defined Functions

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/pedro-teles-fonseca/inspector.svg?branch=master)](https://travis-ci.com/pedro-teles-fonseca/inspector)
[![R build
status](https://github.com/pedro-teles-fonseca/inspector/workflows/R-CMD-check/badge.svg)](https://github.com/pedro-teles-fonseca/inspector/actions)
![pkgdown](https://github.com/pedro-teles-fonseca/inspector/workflows/pkgdown/badge.svg)
[![codecov](https://codecov.io/gh/pedro-teles-fonseca/inspector/branch/master/graph/badge.svg)](https://codecov.io/gh/pedro-teles-fonseca/inspector)
[![MIT
license](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://lbesson.mit-license.org/)
<!-- badges: end -->

## Overview

The `inspector` package provides utility functions that implement and
automate common sets of validation tasks, namely:

  - `inspect_prob()` checks if an object is a numeric vector of valid
    probability values.

  - `inspect_bfactor()` checks if an object is a numeric vector of valid
    Bayes factors values.

  - `inspect_bfactor_log()` checks if an object is a numeric vector of
    valid logarithmic Bayes factors values.

  - `inspect_log_base()` checks if an object is a valid logarithmic
    base.

  - `inspect_true_or_false()` checks if an object is a non-missing
    logical value.

  - `inspect_scale` check if an object is a valid Bayes factor
    interpretation scale.

  - `inspect_categories()` validates factor levels.

  - `inspect_character()` validates character vectors.

  - `inspect_character_match()` validates character values with
    predefined allowed values.

  - `inspect_data_dichotomous()` validates dichotomous data

  - `inspect_data_categorical()` and `inspect_data_multinom_as_bern()`
    validate categorical data.

  - `inspect_par_bernoulli()` validates parameters for the Bernoulli and
    Binomial distributions.

  - `inspect_par_multinomial()` validates parameters for the Multinomial
    distribution.

  - `inspect_par_beta()` validates parameters for the Beta distribution.

  - `inspect_par_dirichlet()` validates parameters for the Dirichlet
    distribution.

  - `inspect_par_haldane()` validates parameters for the Haldane
    distribution.

These functions are particularly useful to validate inputs, intermediate
objects and output values in user-defined functions, resulting in tidier
and less verbose functions.

## Installation

The development version of `inspector` can be installed from
[GitHub](https://github.com/) using the `devtools` package:

``` r
# install.packages("devtools")
devtools::install_github("pedro-teles-fonseca/inspector")
```

## Usage

## Getting Help

If you find a bug, please file an issue with a minimal reproducible
example on [GitHub](https://github.com/pedro-teles-fonseca/inspector).
Feature requests are also welcome. You can contact me at
<pedro.teles.fonseca@phd.iseg.ulisboa.pt>.
