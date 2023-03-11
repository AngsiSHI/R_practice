File\_Import\_practice
================

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.2

    ## Warning: package 'ggplot2' was built under R version 4.1.2

    ## Warning: package 'tidyr' was built under R version 4.1.2

    ## Warning: package 'readr' was built under R version 4.1.2

    ## Warning: package 'purrr' was built under R version 4.1.2

    ## Warning: package 'dplyr' was built under R version 4.1.2

    ## Warning: package 'stringr' was built under R version 4.1.2

    ## Warning: package 'forcats' was built under R version 4.1.2

    ## Warning: package 'lubridate' was built under R version 4.1.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.2.0
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

Set Up

## Read in some data

``` r
litters_df = read_csv("./data/FAS_litters.csv")
```

    ## Rows: 49 Columns: 8── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df = janitor::clean_names(litters_df)
```

## Different ways to look at the data

``` r
litters_df
```

    ## # A tibble: 49 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_…¹ pups_…² pups_…³ pups_…⁴
    ##    <chr> <chr>                <dbl>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Con7  #85                   19.7        34.7       20       3       4       3
    ##  2 Con7  #1/2/95/2             27          42         19       8       0       7
    ##  3 Con7  #5/5/3/83/3-3         26          41.4       19       6       0       5
    ##  4 Con7  #5/4/2/95/2           28.5        44.1       19       5       1       4
    ##  5 Con7  #4/2/95/3-3           NA          NA         20       6       0       6
    ##  6 Con7  #2/2/95/3-2           NA          NA         20       6       0       4
    ##  7 Con7  #1/5/3/83/3-3/2       NA          NA         20       9       0       9
    ##  8 Con8  #3/83/3-3             NA          NA         20       9       1       8
    ##  9 Con8  #2/95/3               NA          NA         20       8       0       8
    ## 10 Con8  #3/5/2/2/95           28.5        NA         20       8       0       8
    ## # … with 39 more rows, and abbreviated variable names ¹​gd_of_birth,
    ## #   ²​pups_born_alive, ³​pups_dead_birth, ⁴​pups_survive

view the front and the back of the data use the following functions:

``` r
head(litters_df)
```

    ## # A tibble: 6 × 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_…¹ pups_…² pups_…³
    ##   <chr> <chr>              <dbl>       <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Con7  #85                 19.7        34.7          20       3       4       3
    ## 2 Con7  #1/2/95/2           27          42            19       8       0       7
    ## 3 Con7  #5/5/3/83/3-3       26          41.4          19       6       0       5
    ## 4 Con7  #5/4/2/95/2         28.5        44.1          19       5       1       4
    ## 5 Con7  #4/2/95/3-3         NA          NA            20       6       0       6
    ## 6 Con7  #2/2/95/3-2         NA          NA            20       6       0       4
    ## # … with abbreviated variable names ¹​pups_born_alive, ²​pups_dead_birth,
    ## #   ³​pups_survive

``` r
tail(litters_df)
```

    ## # A tibble: 6 × 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_…¹ pups_…² pups_…³
    ##   <chr> <chr>              <dbl>       <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 Low8  #79                 25.4        43.8          19       8       0       7
    ## 2 Low8  #100                20          39.2          20       8       0       7
    ## 3 Low8  #4/84               21.8        35.2          20       4       0       4
    ## 4 Low8  #108                25.6        47.5          20       8       0       7
    ## 5 Low8  #99                 23.5        39            20       6       0       5
    ## 6 Low8  #110                25.5        42.7          20       7       0       6
    ## # … with abbreviated variable names ¹​pups_born_alive, ²​pups_dead_birth,
    ## #   ³​pups_survive

Skim the relevant data summary

``` r
skimr::skim(litters_df)
```

|                                                  |             |
|:-------------------------------------------------|:------------|
| Name                                             | litters\_df |
| Number of rows                                   | 49          |
| Number of columns                                | 8           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| character                                        | 2           |
| numeric                                          | 6           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| group          |          0 |              1 |   4 |   4 |     0 |         6 |          0 |
| litter\_number |          0 |              1 |   3 |  15 |     0 |        49 |          0 |

**Variable type: numeric**

| skim\_variable    | n\_missing | complete\_rate |  mean |   sd |   p0 |   p25 |   p50 |   p75 | p100 | hist  |
|:------------------|-----------:|---------------:|------:|-----:|-----:|------:|------:|------:|-----:|:------|
| gd0\_weight       |         15 |           0.69 | 24.38 | 3.28 | 17.0 | 22.30 | 24.10 | 26.67 | 33.4 | ▃▇▇▆▁ |
| gd18\_weight      |         17 |           0.65 | 41.52 | 4.05 | 33.4 | 38.88 | 42.25 | 43.80 | 52.7 | ▃▃▇▂▁ |
| gd\_of\_birth     |          0 |           1.00 | 19.65 | 0.48 | 19.0 | 19.00 | 20.00 | 20.00 | 20.0 | ▅▁▁▁▇ |
| pups\_born\_alive |          0 |           1.00 |  7.35 | 1.76 |  3.0 |  6.00 |  8.00 |  8.00 | 11.0 | ▁▃▂▇▁ |
| pups\_dead\_birth |          0 |           1.00 |  0.33 | 0.75 |  0.0 |  0.00 |  0.00 |  0.00 |  4.0 | ▇▂▁▁▁ |
| pups\_survive     |          0 |           1.00 |  6.41 | 2.05 |  1.0 |  5.00 |  7.00 |  8.00 |  9.0 | ▁▃▂▇▇ |

We can also use view() function in the shell to see the full data

## Options to read\_csv

read\_csv("", skip = 10, col\_names = FALSE) skip the first 10 obs and
rename the col to x1-xn

read\_csv("“, na = c(”“,”NA“,”.")) N/A processing

check our ‘?read\_csv()’ for more info

## Other file format

### Read in exacl file

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.1.2

``` r
mlb_df = read_excel("./data/mlb11.xlsx")
mlb_df
```

    ## # A tibble: 30 × 12
    ##    team         runs at_bats  hits homer…¹ bat_avg strik…² stole…³  wins new_o…⁴
    ##    <chr>       <dbl>   <dbl> <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl>   <dbl>
    ##  1 Texas Rang…   855    5659  1599     210   0.283     930     143    96   0.34 
    ##  2 Boston Red…   875    5710  1600     203   0.28     1108     102    90   0.349
    ##  3 Detroit Ti…   787    5563  1540     169   0.277    1143      49    95   0.34 
    ##  4 Kansas Cit…   730    5672  1560     129   0.275    1006     153    71   0.329
    ##  5 St. Louis …   762    5532  1513     162   0.273     978      57    90   0.341
    ##  6 New York M…   718    5600  1477     108   0.264    1085     130    77   0.335
    ##  7 New York Y…   867    5518  1452     222   0.263    1138     147    97   0.343
    ##  8 Milwaukee …   721    5447  1422     185   0.261    1083      94    96   0.325
    ##  9 Colorado R…   735    5544  1429     163   0.258    1201     118    73   0.329
    ## 10 Houston As…   615    5598  1442      95   0.258    1164     118    56   0.311
    ## # … with 20 more rows, 2 more variables: new_slug <dbl>, new_obs <dbl>, and
    ## #   abbreviated variable names ¹​homeruns, ²​strikeouts, ³​stolen_bases,
    ## #   ⁴​new_onbase

## Options to read\_excel

range = “A1:E7”

Sheet = ""

### Read in SAS

``` r
library(haven)
```

    ## Warning: package 'haven' was built under R version 4.1.2

``` r
pulse_df = read_sas("./data/public_pulse_data.sas7bdat")
pulse_df
```

    ## # A tibble: 1,087 × 7
    ##       ID   age Sex    BDIScore_BL BDIScore_01m BDIScore_06m BDIScore_12m
    ##    <dbl> <dbl> <chr>        <dbl>        <dbl>        <dbl>        <dbl>
    ##  1 10003  48.0 male             7            1            2            0
    ##  2 10015  72.5 male             6           NA           NA           NA
    ##  3 10022  58.5 male            14            3            8           NA
    ##  4 10026  72.7 male            20            6           18           16
    ##  5 10035  60.4 male             4            0            1            2
    ##  6 10050  84.7 male             2           10           12            8
    ##  7 10078  31.3 male             4            0           NA           NA
    ##  8 10088  56.9 male             5           NA            0            2
    ##  9 10091  76.0 male             0            3            4            0
    ## 10 10092  74.2 female          10            2           11            6
    ## # … with 1,077 more rows

## Comparison with Base R

read.csv vs read\_csv

NEVER USE .csv

## Export data

write\_csv(mlb,df, “path”)
