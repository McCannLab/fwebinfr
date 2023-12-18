# _targets.R file
library(targets)
source("R/read_data.R")
source("R/plot_data.R")

tar_option_set(packages = c("readr", "dplyr", "ggplot2"))
list(
    tarchetypes::tar_download(
        "commercial_data",
        urls = "http://www.glfc.org/commercial/commercial.xlsx",
        paths = "data/commercial.xlsx"
    ),
    tar_target(file, "data/commercial.xlsx", format = "file"),
    tar_target(data, read_data(file)),
    tar_target(plot, plot_data(data))
)