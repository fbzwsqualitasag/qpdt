
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qpdt

<!-- badges: start -->
<!-- badges: end -->

The goal of qpdt is to provide a package with generic pedigree tools
used at Qualitas AG.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fbzwsqualitasag/qpdt")
```

## Example

This is a basic example which shows you how basic properties of a
pedigree can be checked:

``` r
library(qpdt)
## basic example code
s_pedigree <- system.file('extdata',
                          'PopReport_SN_ohne_20210115.csv_adaptfin2.csv',
                          package = 'qprppedigree')
check_pedig_parent(ps_pedig_path = s_pedigree)
#> Warning: `guess_max` is a very large value, setting to `21474836` to avoid
#> exhausting memory
#> Parsed with column specification:
#> cols(
#>   `#IDTier` = col_double(),
#>   IDVater = col_double(),
#>   IDMutter = col_double(),
#>   Birthdate = col_date(format = ""),
#>   Geschlecht = col_character(),
#>   PLZ = col_double(),
#>   introg = col_double(),
#>   inb_gen = col_logical(),
#>   cryo = col_double()
#> )
#> $PedFile
#> [1] "/Library/Frameworks/R.framework/Versions/4.0/Resources/library/qprppedigree/extdata/PopReport_SN_ohne_20210115.csv_adaptfin2.csv"
#> 
#> $NrMissingSire
#> [1] 1211
#> 
#> $NrMissingDam
#> [1] 462
#> 
#> $NrSireNotAnimal
#> [1] 4243
#> 
#> $NrDamNotAnimal
#> [1] 6468
#> 
#> $TblSireBdate
#> # A tibble: 9 x 4
#>    `#IDTier`    IDVater Birthdate.Tier Birthdate.Vater
#>        <dbl>      <dbl> <date>         <date>         
#> 1 1000810031 1000050624 2013-03-09     2013-05-02     
#> 2  999870937 1000161006 2013-03-20     2013-10-08     
#> 3 1000810027 1000050624 2013-03-25     2013-05-02     
#> 4 1000813707 1000050624 2013-03-26     2013-05-02     
#> 5 1000810038 1000050624 2013-03-26     2013-05-02     
#> 6 1000810020 1000050624 2013-04-15     2013-05-02     
#> 7 1000711443 1000050624 2013-04-15     2013-05-02     
#> 8 1000456126 1005609568 2013-11-26     2016-02-02     
#> 9 1004653188 1005845648 2015-03-07     2016-09-29     
#> 
#> $TblDamBdate
#> # A tibble: 2 x 4
#>    `#IDTier`   IDMutter Birthdate.Tier Birthdate.Mutter
#>        <dbl>      <dbl> <date>         <date>          
#> 1      99768      99765 1983-03-20     1985-02-26      
#> 2 1000878464 1004910666 2015-03-05     2015-10-12      
#> 
#> $TblSireEqID
#> # A tibble: 0 x 9
#> # … with 9 variables: `#IDTier` <dbl>, IDVater <dbl>, IDMutter <dbl>,
#> #   Birthdate <date>, Geschlecht <chr>, PLZ <dbl>, introg <dbl>, inb_gen <lgl>,
#> #   cryo <dbl>
#> 
#> $TblDamEqID
#> # A tibble: 1 x 9
#>    `#IDTier`  IDVater  IDMutter Birthdate  Geschlecht   PLZ introg inb_gen  cryo
#>        <dbl>    <dbl>     <dbl> <date>     <chr>      <dbl>  <dbl> <lgl>   <dbl>
#> 1 1004866450   9.99e8    1.00e9 2011-03-27 F           7411      0 NA          0
```

If you want to find whether a certain pedigree contains cycles the
following statements provides an anser.

``` r
s_pedi_path <- system.file('extdata','data_sample2.csv', package = 'qprppedigree')
check_cycle_pedigree(ps_pedig_path = s_pedi_path)
#> Parsed with column specification:
#> cols(
#>   `#animal` = col_double(),
#>   sire = col_double(),
#>   dam = col_double(),
#>   birth_date = col_date(format = ""),
#>   sex = col_character(),
#>   plz = col_double(),
#>   introg = col_double(),
#>   inb_gen = col_logical(),
#>   cryo = col_logical()
#> )
#> $PedFile
#> [1] "/Library/Frameworks/R.framework/Versions/4.0/Resources/library/qprppedigree/extdata/data_sample2.csv"
#> 
#> $HasCycle
#> [1] FALSE
```

------------------------------------------------------------------------

*Latest Changes: 2021-03-03 17:27:32 (pvr)*
