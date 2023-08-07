
# analyses_RC

The goal of analyses_RC is to produce same outputs between Reunion and Mayotte 
islands reef check surveys, transparent and reproducible results.

## Installation

1. Go to [GitHub](https://github.com/clemdelrevo/analyses_RC)

2. Create a new git version control project

3. Enter this URL: https://github.com/clemdelrevo/analyses_RC.git

4. Synchronize the library with 

``` r
renv::restore()

```

## Run targets

All the targets are define in analyses/pipeline

For example, if you want to look coral cover timeline in Mayotte, run:

``` r
targets::tar_read(final_cc_timeline_may)

```

If you want to load target, run :

``` r
targets::tar_load(line_may)

```
After that, you can work with the data:

``` r
# look the data
line_may

# select site in fringing reef
fringing <- line_may[line_may$reef_type == "fringing", ]

```