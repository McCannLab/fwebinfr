read_data <- function(file) {
    sht <- readxl::excel_sheets(file)
    sht |>
        lapply(\(x) readxl::read_xlsx(file, sheet = x))
}