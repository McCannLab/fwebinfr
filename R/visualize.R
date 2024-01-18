#' Internal
#' @noRd
#' 
# quick_ode_plot(mod_lv_fr1, rep(10, 5), seq(1, 50, 0.1), pars = fw_gen_parms_exper2())
quick_ode_plot <- function(fun, init, times, pars) {
    out <- deSolve::ode(init, times, fun, pars)
    out |>
        dplyr::as_tibble() |>
        dplyr::mutate_all(as.numeric) |>
        tidyr::pivot_longer(
            -time,
            names_to = "variable", values_to = "value"
        ) |>
        ggplot2::ggplot() +
        ggplot2::aes(x = time, y = value, color = variable) +
        ggplot2::geom_line()
}


