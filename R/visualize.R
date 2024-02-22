#' Simple ODE plot
#'
#' @param x an object of class `fw_model`.
#' @param y an object of class `fw_predict`.
#' @param init initial values (see [deSolve::ode()]).
#' @param times time steps (see [deSolve::ode()]).
#' @param include_stability a logical. Should the distribution of eigen values
#' be included?
#'
#' @export
#' @examples
#' fw_ode_plot(fw_example_2species(), rep(0.3, 2), seq(1, 500, 0.1))
fw_ode_plot <- function(x, init, times) {
    stopifnot(inherits(x, "fw_model"))

    out <- deSolve::ode(init, times, fun = x$model, parms = x)
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


#' @export
#' @rdname fw_ode_plot
fw_range_plot <- function(y, include_stability = TRUE) {
    stopifnot(inherits(y, "fw_predicted"))
    y <- y$prediction

    y_long <- y |>
        tidyr::pivot_longer(
            tidyr::everything(),
            values_to = "val", names_to = "var"
        )

    p1 <- y_long |>
        dplyr::filter(var != "leading_ev") |>
        ggplot2::ggplot() +
        ggplot2::geom_boxplot(ggplot2::aes(x = var, y = val)) +
        ggplot2::scale_y_continuous(trans = "log10") +
        ggplot2::ggtitle("Interaction strengths") +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90, vjust = 0.5, hjust = 1
            )
        )


    if (include_stability) {
        p2 <- y_long |>
            dplyr::filter(var == "leading_ev") |>
            ggplot2::ggplot() +
            ggplot2::geom_boxplot(ggplot2::aes(x = var, y = val)) +
            ggplot2::ggtitle("Leading eigen value")

        p1 + p2 + plot_layout(widths = c(2, 1))
    } else {
        p1
    }
}