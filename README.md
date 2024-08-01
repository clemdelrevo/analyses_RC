
# analyses_RC

The goal of analyses_RC is to standardize results of Reek Check survey through years
on Mayotte island. We have adopted a transparent and reproducible approach by creating 
an analysis workflow.

## Installation

1. Go to [GitHub](https://github.com/clemdelrevo/analyses_RC)

2. Create a new git version control project

3. Enter this URL: https://github.com/clemdelrevo/analyses_RC.git

4. Synchronize the library with 

``` r
renv::restore()

```

5. Create data and outputs folders with


``` r
dir.create(data)
dir.create(outputs)

```

## Run the pipeline

Run

``` r
targets::tar_config_set(
store = "outputs/pipeline/",
script = "analyses/pipeline.R"
)

targets::tar_make()

```

It will take just few minutes...

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