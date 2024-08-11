
# analyses_RC

The goal of analyses_RC is to standardize results of Reek Check survey through years
on Mayotte island. We have adopted a transparent and reproducible approach by creating 
an analysis workflow.

# Data accessibility

Raw data are the property of the data producers, with the exception of data obtained with public funding. Data can be requested from <service.plongee.scientifique@gmail.com>

## Installation


1. Go to [GitHub](https://github.com/clemdelrevo/analyses_RC)

2. Create a new git version control project in RStudio

3. Enter this URL: https://github.com/clemdelrevo/analyses_RC.git

4. Set up your environment with the make.R file place at the root of the project

``` r
dir.create("data")
dir.create("outputs")
file.create("rc_report.qmd")

# renv
renv::install() ; renv::snapshot(prompt = FALSE) ; renv::status()

# targets
dir.create("outputs/pipeline")
targets::tar_config_set(
store = "outputs/pipeline/",
script = "analyses/pipeline.R"
)
```

## Run the pipeline

Run
``` r
targets::tar_make()
```

It will take just few minutes...

## Run targets

All the targets are define in analyses/pipeline

For example, if you want to look the evolution of the coral coverss in Mayotte, run:

``` r
targets::tar_read(final_cc_timeline_may)

```

If you want to load target, run :

``` r
targets::tar_load(data_pit_may)

```
After that, you can work with the data:

``` r
# Have a look to the data
head(data_pit_may)

# select site in fringing reef
fringing <- data_pit_may[data_pit_may$reef_type == "fringing", ]
```