plot_data <- function(data) {
    spc <- c("White Perch", "Yellow Perch", "Lake Trout")
    ggplot(data[[1]] |> dplyr::filter(Species %in% spc)) +
        geom_line() +
        facet_grid(rows = "Species")
}
read_data <- function(file) {
    sht <- readxl::excel_sheets(file)
    sht |>
        lapply(\(x) readxl::read_xlsx(file, sheet = x))
}