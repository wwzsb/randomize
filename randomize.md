Randomization tutorial
================

## Generating a population

``` r
install.packages("tidyverse")
```

    ## Installing package into '/home/ron/R/x86_64-pc-linux-gnu-library/4.2'
    ## (as 'lib' is unspecified)

``` r
install.packages("randomNames")
```

    ## Installing package into '/home/ron/R/x86_64-pc-linux-gnu-library/4.2'
    ## (as 'lib' is unspecified)

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   1.4.0     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
population_size <- 10e6
sample_sizes <- c(10, 100, 1000, 1000000)
num_simulations <- 10

ethnicities <- c("Indian", "Asian", "Black", "Hispanic", "White", "Arabic")
genders <- c("Female", "Male")

population <- randomNames::randomNames(population_size,
                                       return.complete.data = TRUE) %>%
  mutate(gender = map_chr(gender, ~ genders[.x + 1]),
         ethnicity = map_chr(ethnicity, ~ ethnicities[.x])) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(income = round(rnorm(population_size, 10e4, 10e3)))

summary(population)
```

    ##     gender           ethnicity             first_name         last_name      
    ##  Female:4998958   Arabic  :1667172   Michael    :  68794   Nguyen  :  76388  
    ##  Male  :5001042   Asian   :1666775   Joshua     :  63237   Martinez:  74054  
    ##                   Black   :1668474   Brandon    :  58064   Johnson :  60689  
    ##                   Hispanic:1665491   Christopher:  51559   Smith   :  58225  
    ##                   Indian  :1665573   Jordan     :  49612   Garcia  :  52915  
    ##                   White   :1666515   Tyler      :  47260   Williams:  45188  
    ##                                      (Other)    :9661474   (Other) :9632541  
    ##      income      
    ##  Min.   : 47908  
    ##  1st Qu.: 93252  
    ##  Median : 99999  
    ##  Mean   : 99999  
    ##  3rd Qu.:106746  
    ##  Max.   :154543  
    ## 

``` r
population %>%
  ggplot(aes(ethnicity, fill = ethnicity)) +
  geom_bar()
```

![](randomize_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
population %>%
  ggplot(aes(gender, fill = gender)) +
  geom_bar()
```

![](randomize_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
population %>%
  ggplot(aes(income)) +
  geom_density()
```

![](randomize_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

## Random Sampling

``` r
map(1:num_simulations, function(x) {
  
  map_dfr(sample_sizes, ~ population %>% sample_n(.x) %>%
            mutate(size = .x)) %>%
    # mutate(sim = sim_num) %>%
    ggplot(aes(gender, fill = gender)) +
    geom_bar() +
    facet_wrap(~ size, scales = "free_y") +
    theme(legend.position = "none")
})
```

    ## [[1]]

![](randomize_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

    ## 
    ## [[2]]

![](randomize_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

    ## 
    ## [[3]]

![](randomize_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

    ## 
    ## [[4]]

![](randomize_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

    ## 
    ## [[5]]

![](randomize_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

    ## 
    ## [[6]]

![](randomize_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->

    ## 
    ## [[7]]

![](randomize_files/figure-gfm/unnamed-chunk-2-7.png)<!-- -->

    ## 
    ## [[8]]

![](randomize_files/figure-gfm/unnamed-chunk-2-8.png)<!-- -->

    ## 
    ## [[9]]

![](randomize_files/figure-gfm/unnamed-chunk-2-9.png)<!-- -->

    ## 
    ## [[10]]

![](randomize_files/figure-gfm/unnamed-chunk-2-10.png)<!-- -->

``` r
map(1:num_simulations, function(x) {
  sample_sizes <- c(10, 100, 1000, 1000000)
  
  map_dfr(sample_sizes, ~ population %>% sample_n(.x) %>%
            mutate(size = .x)) %>%
    # mutate(sim = sim_num) %>%
    ggplot(aes(ethnicity, fill = ethnicity)) +
    geom_bar() +
    facet_wrap(~ size, scales = "free_y") +
    theme(legend.position = "none")
})
```

    ## [[1]]

![](randomize_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

    ## 
    ## [[2]]

![](randomize_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

    ## 
    ## [[3]]

![](randomize_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

    ## 
    ## [[4]]

![](randomize_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

    ## 
    ## [[5]]

![](randomize_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

    ## 
    ## [[6]]

![](randomize_files/figure-gfm/unnamed-chunk-3-6.png)<!-- -->

    ## 
    ## [[7]]

![](randomize_files/figure-gfm/unnamed-chunk-3-7.png)<!-- -->

    ## 
    ## [[8]]

![](randomize_files/figure-gfm/unnamed-chunk-3-8.png)<!-- -->

    ## 
    ## [[9]]

![](randomize_files/figure-gfm/unnamed-chunk-3-9.png)<!-- -->

    ## 
    ## [[10]]

![](randomize_files/figure-gfm/unnamed-chunk-3-10.png)<!-- -->

``` r
map(1:num_simulations, function(x) {
  sample_sizes <- c(10, 100, 1000, 1000000)
  
  map_dfr(sample_sizes, ~ population %>% sample_n(.x) %>%
            mutate(size = .x)) %>%
    ggplot(aes(income)) +
    geom_histogram() +
    facet_wrap(~ size, scales = "free_y") +
    theme(legend.position = "none")
})
```

    ## [[1]]

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](randomize_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

    ## 
    ## [[2]]

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](randomize_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

    ## 
    ## [[3]]

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](randomize_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

    ## 
    ## [[4]]

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](randomize_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

    ## 
    ## [[5]]

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](randomize_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

    ## 
    ## [[6]]

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](randomize_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->

    ## 
    ## [[7]]

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](randomize_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->

    ## 
    ## [[8]]

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](randomize_files/figure-gfm/unnamed-chunk-4-8.png)<!-- -->

    ## 
    ## [[9]]

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](randomize_files/figure-gfm/unnamed-chunk-4-9.png)<!-- -->

    ## 
    ## [[10]]

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](randomize_files/figure-gfm/unnamed-chunk-4-10.png)<!-- --> \##
Random Assignment

### Covariate balance

``` r
map(1:num_simulations, function(x) {
  sample_sizes <- c(10, 100, 1000, 1000000)
  
  map_dfr(sample_sizes, ~ population %>% sample_n(.x) %>%
            mutate(size = .x) %>%
            mutate(group = sample(c("treatment", "control"), .x, replace = TRUE))) %>%
    ggplot(aes(income, color = group, fill = group)) +
    geom_density(alpha = 0.2) +
    facet_wrap(~ size, scales = "free_y")
})
```

    ## [[1]]

![](randomize_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    ## 
    ## [[2]]

![](randomize_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

    ## 
    ## [[3]]

![](randomize_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

    ## 
    ## [[4]]

![](randomize_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

    ## 
    ## [[5]]

![](randomize_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->

    ## 
    ## [[6]]

![](randomize_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->

    ## 
    ## [[7]]

![](randomize_files/figure-gfm/unnamed-chunk-5-7.png)<!-- -->

    ## 
    ## [[8]]

![](randomize_files/figure-gfm/unnamed-chunk-5-8.png)<!-- -->

    ## 
    ## [[9]]

![](randomize_files/figure-gfm/unnamed-chunk-5-9.png)<!-- -->

    ## 
    ## [[10]]

![](randomize_files/figure-gfm/unnamed-chunk-5-10.png)<!-- -->

### Treatment effects

``` r
sample_sizes <- c(10, 100, 1000)

rct_sims <- map(1:100, function(x) {
 
  map_dfr(sample_sizes, ~ population %>% sample_n(.x) %>%
            mutate(size = .x) %>%
            mutate(group = sample(c("Treatment", "Control"), .x, replace = TRUE))) %>%
    mutate(outcome = if_else(gender == "Male", income + 10000, income),
           outcome = if_else(group == "Treatment", outcome + 5000, outcome)) %>%
    group_by(size) %>%
    group_map(~ lm(outcome ~ group, data = .x))
})

map(rct_sims, function(.x) {
  map(.x, function(.y) {
    .y %>% summary() %>% pluck("coefficients") %>% .["groupTreatment","Pr(>|t|)"]
  })
}) %>%
  unlist() %>%
  matrix(ncol = length(sample_sizes), byrow = TRUE) %>%
  as_tibble() %>%
  set_names(as.character(sample_sizes)) %>%
  pivot_longer(everything()) %>%
  # mutate(value = value < 0.1) %>%
  ggplot(aes(value, fill = value)) +
  # geom_bar() +
  # geom_histogram(binwidth = 0.01) +
  geom_density () +
  facet_wrap(~ name, scales = "free_y", nrow = 3) +
  geom_vline(xintercept = c(0.01, 0.05, 0.1), alpha = 0.2, color = "red")
```

    ## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.

![](randomize_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
