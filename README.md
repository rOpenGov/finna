# finna

[![rOG-badge](https://ropengov.github.io/rogtemplate/reference/figures/ropengov-badge.svg)](https://ropengov.org/)
[![issues](https://img.shields.io/github/issues/rOpenGov/finna)](https://github.com/rOpenGov/finna/issues)
[![pulls](https://img.shields.io/github/issues-pr/rOpenGov/finna)](https://github.com/rOpenGov/finna/pulls)
[![R-CMD-check](https://github.com/rOpenGov/finna/workflows/rworkflows/badge.svg)](https://github.com/rOpenGov/finna/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/rOpenGov/finna/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/rOpenGov/finna)
[![codefactor](https://www.codefactor.io/repository/github/rOpenGov/finna/badge)](https://www.codefactor.io/repository/github/rOpenGov/finna)

The goal of finna is to retrieve data from Finna API 

## Installation instructions
The devel version of finna can be installed from GitHub as follows:

``` r
# Install finna if not already installed
if (!requireNamespace("finna", quietly = TRUE)) {
  remotes::install_github("rOpenGov/finna")
}
```

``` r
remotes::install_github("rOpenGov/finna")
```

## Example
The basic functionality of finna can be explored as follows:

``` r
# Load the package
library(finna)
# Perform a simple search and print a table

record <- search_finna("sibelius")
head(record)
```
| Title                                                                                        | Author           | Year | Language | Formats           | Subjects          | Library             | Series |
|:---------------------------------------------------------------------------------------------|:-----------------|:-----|:---------|:------------------|:------------------|:--------------------|:-------|
| Sibelius favourites: Sibelius collection                                                     | Sibelius         | 2001 | N/A      | Äänite (audio)    | Orkesterimusiikki  | Lapin               | N/A    |
| SIBELIUS                                                                                     | TAWASTSTJERNA    | 1997 | Finnish  | Kirja (book)      | SIBELIUS           | Anders Chydenius    | N/A    |
| Sibelius                                                                                     | TAWASTSTJERNA    | 1997 | Finnish  | Kirja (book)      | Sibelius           | Anders Chydenius    | N/A    |
| Sibelius                                                                                     | Lampila          | 1984 | Finnish  | Kirja (book)      | Sibelius           | Helka-arkisto       | N/A    |
| Sibelius                                                                                     | TAWASTSTJERNA    | 2003 | Finnish  | Kirja (book)      | Sibelius           | Kansalliskirjasto   | N/A    |
| Sibelius                                                                                     | Ringbom          | 1948 | Finnish  | Kirja (book)      | Sibelius           | Kirkes              | N/A    |

To search all related in descending order
``` r
record <- search_finna("sibelius", sort = "main_date_str des")
head(record)
```
| Title                                                                                  | Author                    | Year | Language | Formats         | Subjects      | Library         | Series   |
|:---------------------------------------------------------------------------------------|:--------------------------|:-----|:---------|:----------------|:--------------|:----------------|:---------|
| He selvisivät sodasta                                                                  | Kirves, Jenni, Werner Söderström | 2024 | Finnish  | Kirja (book)    | Sotilaat       | Anders Chydenius | N/A      |
| Yli-ihmisiä ja traagisia kuolevaisia: esseitä ja kirjoituksia 1901-1945                | Frosterus, Sigurd, Sarje Maaria | 2024 | Finnish  | Kirja (book)    | Wagner, etc.   | Helka-arkisto    | N/A      |
| Eero Järnefelt                                                                         | Järnefelt, Eero, Selkokari | 2024 | Finnish  | Kirja (book)    | Järnefelt      | Anders Chydenius | Ateneum  |
| Eero Järnefelt                                                                         | Järnefelt, Eero, Selkokari | 2024 | Swedish  | Kirja (book)    | Järnefelt      | Helle-kirjastot  | Ateneum  |
| Eero Järnefelt                                                                         | Järnefelt, Eero, Selkokari | 2024 | English  | Kirja (book)    | Järnefelt      | Helmet-kirjasto  | Ateneum  |
| Solace                                                                                 | N/A                        | 2024 | No language (zxx) | Äänite (audio)  | N/A            | Anders Chydenius | N/A      |



## Code of Conduct
Please note that the finna project is released with a
[Contributor Code of Conduct](Link).
By contributing to this project, you agree to abide by its terms. Contributions
are welcome in the form of feedback, issues and pull requests. You can find the
contributor guidelines of the finna
[here]().

## Acknowledgements
Please note that finna was only made possible thanks to many other R and
rOpenGov software authors, which are cited in the vignettes describing
this package.

This package was developed using the following resources:

- [_usethis_](https://cran.r-project.org/web/packages/usethis/) to generate an
  initial template.
- Continuous code testing is performed on
  [GitHub actions](https://github.com/features/actions) and include R CMD check,
- Code coverage assessment is possible thanks to
  [codecov](https://app.codecov.io/gh/).
- The documentation website is automatically updated thanks to
  [_pkgdown_](https://cran.r-project.org/web/packages/pkgdown/).
- The documentation is formatted thanks to
  [_devtools_](https://cran.r-project.org/web/packages/devtools/) and
  [_roxygen2_](https://cran.r-project.org/web/packages/roxygen2/).

### Disclaimer

This package is in no way officially related to or endorsed by Finna.

When using data retrieved from Finna database in your work, please
indicate that the data source is Finna. If your re-use involves some
kind of modification to data or text, please state this clearly to the
end user. See Finna policy on [copyright and free re-use of
data](https://www.finna.fi/Content/terms?lng=en-gb) for more
detailed information and certain exceptions.
