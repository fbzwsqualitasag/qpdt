
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qpdt

<!-- badges: start -->
<!-- badges: end -->

The website of qpdt is available under
<https://fbzwsqualitasag.github.io/qpdt/>

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
                          package = 'qpdt')
check_pedig_parent(ps_pedig_path = s_pedigree)
#> $PedFile
#> [1] "/Library/Frameworks/R.framework/Versions/4.0/Resources/library/qpdt/extdata/PopReport_SN_ohne_20210115.csv_adaptfin2.csv"
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
#>    `#IDTier`    IDVater `Birthdate.#IDTier` Birthdate.IDVater
#>        <dbl>      <dbl> <date>              <date>           
#> 1 1000810031 1000050624 2013-03-09          2013-05-02       
#> 2  999870937 1000161006 2013-03-20          2013-10-08       
#> 3 1000810027 1000050624 2013-03-25          2013-05-02       
#> 4 1000813707 1000050624 2013-03-26          2013-05-02       
#> 5 1000810038 1000050624 2013-03-26          2013-05-02       
#> 6 1000810020 1000050624 2013-04-15          2013-05-02       
#> 7 1000711443 1000050624 2013-04-15          2013-05-02       
#> 8 1000456126 1005609568 2013-11-26          2016-02-02       
#> 9 1004653188 1005845648 2015-03-07          2016-09-29       
#> 
#> $TblDamBdate
#> # A tibble: 2 x 4
#>    `#IDTier`   IDMutter `Birthdate.#IDTier` Birthdate.IDMutter
#>        <dbl>      <dbl> <date>              <date>            
#> 1      99768      99765 1983-03-20          1985-02-26        
#> 2 1000878464 1004910666 2015-03-05          2015-10-12        
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
#> 
#> $TblSireWrongSex
#> # A tibble: 3 x 2
#>      IDVater Geschlecht
#>        <dbl> <chr>     
#> 1 1007737726 F         
#> 2 1006000916 F         
#> 3 1004668293 F         
#> 
#> $TblDamWrongSex
#> # A tibble: 0 x 2
#> # … with 2 variables: IDMutter <dbl>, Geschlecht <chr>
```

If you want to find whether a certain pedigree contains cycles the
following statements provides an anser.

``` r
s_pedi_path <- system.file('extdata','data_sample_cycle.csv', package = 'qpdt')
check_cycle_pedigree(ps_pedig_path = s_pedi_path)
#> $PedFile
#> [1] "/Library/Frameworks/R.framework/Versions/4.0/Resources/library/qpdt/extdata/data_sample_cycle.csv"
#> 
#> $HasCycle
#> [1] TRUE
```

------------------------------------------------------------------------

*Latest Changes: 2021-04-01 08:24:00 (pvr)*
