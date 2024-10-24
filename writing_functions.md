writing_functions
================
Pradeeti Mainali
2024-10-24

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(readxl)

set.seed(1)
```

## writing my first function !!

as an example, here is a z-score computation:

``` r
x_vec = rnorm(n = 25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

not i’ll write a function to do this

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 5) {
    stop("you need at least five numbers to compute the z score")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

() will have the function and {} will hold what it should do

does this always work?

``` r
z_scores(x = 3)
```

    ## Error in z_scores(x = 3): you need at least five numbers to compute the z score

``` r
# you cant compute the sd with just one numbers
z_scores(x = c("my", "name", "is", "Jeff"))
```

    ## Error in z_scores(x = c("my", "name", "is", "Jeff")): x needs to be numeric

``` r
# no because these are characters..
```

## A new function!

``` r
mean_and_sd = function(x) {
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
      )
  
  return(out_df)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.6  3.33

## Check stuff using a simulation:

``` r
sim_df = 
  tibble(
    x = rnorm(30, 10, 5)
  )

sim_df |>
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.2  3.71

Simulation function to check sample mean and sd

``` r
sim_mean_sd = function(samp_size, true_mean, true_sd) {
  
  sim_df = 
  tibble(
    x = rnorm(samp_size, true_mean, true_sd)
  )

out_df = 
  sim_df |>
  summarise(
    mean = mean(x),
    sd = sd(x)
  )

return(out_df)
}

sim_mean_sd(samp_size = 3000, true_mean = 4, true_sd = .12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.00 0.125

``` r
# order doesnt matter in there

sim_mean_sd(30, 16, 2)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  16.8  2.15

``` r
# this will follow the order as above

#you can also set default values up on the first line of this chunk. If you do this, the following will work (because it will assume the true mean and sd:

sim_mean_sd(30)
```

    ## Error: argument "true_mean" is missing, with no default

## revisit LoTR words

``` r
fellowship_df = 
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship") |>
  janitor::clean_names()

two_towers_df = 
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers") |>
  janitor::clean_names()

return_king_df = 
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king") |>
  janitor::clean_names()

lotr_tidy = bind_rows(fellowship_df, two_towers_df, return_king_df) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") |> 
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything()) 
```

``` r
# write something that can be used to import either one of the previous datasets:

lotr_import = function(cell_range, movie_title) {
  
  movie_df =
    read_excel("data/LotR_Words.xlsx", range = cell_range) |>
    mutate(movie = movie_title) |>
    janitor::clean_names() |>
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words") |>
    select(movie, everything())
  
  return(movie_df)
  
}

lotr_df = 
  bind_rows(
    lotr_import("B3:D6", "fellowship_ring"),
    lotr_import("F3:H6", "two_towers"),
    lotr_import("J3:L6", "return_king"))
```

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

marj_table = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  mutate(drug = "marj")

cocaine_table = 
  nsduh_html |> 
  html_table() |> 
  nth(4) |>
  slice(-1) |> 
  mutate(drug = "cocaine")

heroin_table = 
  nsduh_html |> 
  html_table() |> 
  nth(5) |>
  slice(-1) |> 
  mutate(drug = "heroin")


nsduh_table_format = function(html, table_num, table_name) {
 
  out_table = 
  html |> 
  html_table() |> 
  nth(table_num) |>
  slice(-1) |> 
  mutate(drug = table_name)
  
  return(out_table)
}

bind_rows(
nsduh_table_format(html = nsduh_html, 1, "marj"),
nsduh_table_format(html = nsduh_html, 4, "cocaine"),
nsduh_table_format(html = nsduh_html, 5, "heroin")
)
```

    ## # A tibble: 168 × 17
    ##    State     `12+(2013-2014)` `12+(2014-2015)` `12+(P Value)` `12-17(2013-2014)`
    ##    <chr>     <chr>            <chr>            <chr>          <chr>             
    ##  1 Total U.… 12.90a           13.36            0.002          13.28b            
    ##  2 Northeast 13.88a           14.66            0.005          13.98             
    ##  3 Midwest   12.40b           12.76            0.082          12.45             
    ##  4 South     11.24a           11.64            0.029          12.02             
    ##  5 West      15.27            15.62            0.262          15.53a            
    ##  6 Alabama   9.98             9.60             0.426          9.90              
    ##  7 Alaska    19.60a           21.92            0.010          17.30             
    ##  8 Arizona   13.69            13.12            0.364          15.12             
    ##  9 Arkansas  11.37            11.59            0.678          12.79             
    ## 10 Californ… 14.49            15.25            0.103          15.03             
    ## # ℹ 158 more rows
    ## # ℹ 12 more variables: `12-17(2014-2015)` <chr>, `12-17(P Value)` <chr>,
    ## #   `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>, `18-25(P Value)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `26+(P Value)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>, `18+(P Value)` <chr>,
    ## #   drug <chr>
