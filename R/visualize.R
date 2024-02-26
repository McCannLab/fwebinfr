#' Simple ODE plot
#'
#' @param x an object of class `fw_model`.
#' @param y an object of class `fw_predict`.
#' @param init initial values (see [deSolve::ode()]).
#' @param times time steps (see [deSolve::ode()]).
#' @param show_lev a logical. Should the distribution of eigen values
#' be included?
#' @param show_points a logical. Should points be added on top of box. Note
#' that points will be colored according to the value of the leading eigen
#' vector.
#' @param prob the quantile used to split parameters sets.
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
fw_range_plot <- function(y, show_points = FALSE, show_lev = TRUE) {
    stopifnot(inherits(y, "fw_predicted"))

    y_long <- y$prediction |>
        tidyr::pivot_longer(
            tidyr::starts_with("a_"),
            values_to = "val", names_to = "var"
        )

    p1 <- y_long |>
        ggplot2::ggplot(ggplot2::aes(x = var, y = val)) +
        ggplot2::geom_boxplot() +
        ggplot2::scale_y_continuous(trans = "log10") +
        ggplot2::ggtitle("Interaction strengths") +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90, vjust = 0.5, hjust = 1
            )
        ) +
        ggplot2::xlab("Interactions") +
        ggplot2::xlab("Interaction strengths")
    if (show_points) {
        p1 <- p1 +
            ggplot2::geom_jitter(
                ggplot2::aes(color = leading_ev),
                size = 0.2,
                alpha = 0.4
            ) +
            ggplot2::scale_colour_gradientn(colours = grDevices::terrain.colors(10))
    }

    if (show_lev) {
        p2 <- y$prediction |>
            tidyr::pivot_longer(
                tidyr::contains("leading_"),
                values_to = "val", names_to = "var"
            ) |>
            ggplot2::ggplot() +
            ggplot2::geom_boxplot(ggplot2::aes(x = var, y = val)) +
            ggplot2::ggtitle("Stability") +
            ggplot2::xlab("") +
            ggplot2::ylab("Leading eigen value")
        p1 + p2 + plot_layout(widths = c(3, 1))
    } else {
        p1
    }
}


#' @export
#' @rdname fw_ode_plot
fw_range_compare_plot <- function(y, prob = 0.25, show_lev = TRUE) {
    stopifnot(exprs = {
        inherits(y, "fw_predicted")
        length(prob) == 1
    })

    y_tmp <- y$prediction |>
        dplyr::mutate(
            most_stable = leading_ev <= stats::quantile(leading_ev, prob)
        )

    y_long <- y_tmp |>
        tidyr::pivot_longer(
            tidyr::contains("a_"),
            values_to = "val", names_to = "var"
        )

    p1 <- y_long |>
        ggplot2::ggplot(ggplot2::aes(x = var, y = val, fill = most_stable)) +
        ggplot2::geom_boxplot() +
        ggplot2::scale_y_continuous(trans = "log10") +
        ggplot2::ggtitle("Interaction strengths") +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = 90, vjust = 0.5, hjust = 1
            )
        ) +
        ggplot2::xlab("Interactions") +
        ggplot2::xlab("Interaction strengths")

    if (show_lev) {
        p2 <- y_tmp |>
            tidyr::pivot_longer(
                tidyr::contains("leading_"),
                values_to = "val", names_to = "var"
            ) |>
            ggplot2::ggplot() +
            ggplot2::geom_boxplot(
                ggplot2::aes(x = var, y = val, fill = most_stable),
                show.legend = FALSE
            ) +
            ggplot2::ggtitle("Stability") +
            ggplot2::xlab("") +
            ggplot2::ylab("Leading eigen value")
        p1 + p2 + plot_layout(widths = c(3, 1))
    } else {
        p1
    }
}