#' Numerical experiments
#' 
#' @importFrom grDevices dev.off png
#' @importFrom graphics abline boxplot legend lines par
#' @export

fw_list_experim <- function() {
    val <- c("run_experim01", "get_fig_exp_1", "run_experim02")
    ul  <- cli::cli_ul()
    cli::cli_li()
    cli::cli_end(ul)
}


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



#' @describeIn fw_list_experim First experiment 
#' @param eff_max maximum transfer efficiency.
#' @export
run_experim01 <- function(eff_max = 1) {
    a_21 <- seq(0.055, 0.15, 0.005)
    res_A <- res_B <- res_xsample <- res_stab <- list()

    cli::cli_progress_bar("a_21 increases", total = length(a_21), 
        type = "iterator")
    for (i in seq(a_21)) {
        A <- matrix(0, 2, 2)
        A[1, 1] <- -0.1
        A[1, 2] <- -0.2
        A[2, 1] <- a_21[i]
        R <- c(0.1, -0.05)
        B <- limSolve::lsei(E = A, F = -R)$X
        res_A[[i]] <- A
        res_B[[i]] <- B
        res_stab[[i]] <- get_leading_ev(get_jacobian(A, B, R, mod_lv_fr1))
        res_xsample[[i]] <- wrap_xsample(A, B, R, eff_max = eff_max)
        cli::cli_progress_update()
    }
    cli::cli_progress_done()

    biom_eq <- do.call(rbind, res_B) |> as.data.frame()
    names(biom_eq) <- paste0("biom_eq_", 1:2)

    return(list(
        dat = cbind(biom_eq, a_21 = a_21, stab = unlist(res_stab)),
        x_sample = res_xsample
    ))
}


#' @describeIn fw_list_experim First experiment with three different eff_max
#' @param output_dir output directory
#' @export 
get_fig_exp_1 <- function(output_dir = ".") {
    ttl <- c("eff_max_1.png", "eff_max_2.png", "eff_max_0_75.png")

    ls_exp <- list(
        run_experim01(),
        run_experim01(2),
        run_experim01(0.75)
    )
    for (i in 1:3) {
        hh <- ls_exp[[i]]
        xr <- c(0.055, 0.15)
        bw <- 0.005
        png(
            file.path(output_dir, ttl[i]),
            width = 9, height = 8, units = "in", res = 200
        )
        par(mfrow = c(2, 2))
        plot(xr, c(0, 1), type = "n", xlab = "a_21")
        lines(hh$dat[, 3], hh$dat[, 1], type = "l")
        lines(hh$dat[, 3], hh$dat[, 2], type = "l", col = "steelblue")
        legend("topright", legend = c("Ressource", "Consumer"), col = c("black", "steelblue"), bty = "n", lty = 1)

        plot(xr, c(0, 0.2), type = "n", xlab = "a_21", main = "a_11")
        for (i in seq_along(hh$x_sample)) {
            boxplot(at = hh$dat[i, 3], hh$x_sample[[i]][, 1], add = TRUE, boxwex = bw, axes = FALSE, ylab = "Interaction Strength")
        }
        abline(a = 0.1, b = 0, lty = 2, col = "grey60")

        plot(xr, c(0, 1), type = "n", xlab = "a_21", main = "a_12")
        for (i in seq_along(hh$x_sample)) {
            boxplot(at = hh$dat[i, 3], hh$x_sample[[i]][, 3], add = TRUE, boxwex = bw, axes = FALSE, ylab = "Interaction Strength")
        }
        abline(a = 0.2, b = 0, lty = 2, col = "grey60")
        plot(xr, c(-0.04, 0.005), type = "n", xlab = "a_21", main = "Leading Eigen value")
        for (i in seq_along(hh$x_sample)) {
            boxplot(at = hh$dat[i, 3], hh$x_sample[[i]][, 4], add = TRUE, boxwex = bw, axes = FALSE, ylab = "Re(Eigen)")
        }
        lines(hh$dat[, 3], hh$dat[, 4], type = "b", pch = 19, col = "orange")
        dev.off()
    }
}


#--------- Figure 3 species

run_experim02 <- function(eff_max = 1) {

    n  <- 10
    a_13 <- -seq(0.01, 0.1, length.out = n)
    a_31 <- -0.5*a_13
    a_35 <- -seq(0.01, 0.1, length.out = n)
    a_53 <- -0.5 * a_35
    par0  <- fw_gen_parms_exper2()
    res_A <- res_B <- res_xsample <- list()

    cli::cli_progress_bar("increases one channel",
        total = n,
        type = "iterator"
    )
    for (i in seq_len(10)) {
        A <- par0$A
        A[1, 3] <- a_13[i]
        A[3, 1] <- a_31[i]
        A[3, 5] <- a_35[i]
        A[5, 3] <- a_53[i]
        B <- limSolve::lsei(E = A, F = -par0$R)$X
        res_A[[i]] <- A
        res_B[[i]] <- B
        res_xsample[[i]] <- fw_infer(A, B, par0$R, eff_max = eff_max)
        cli::cli_progress_update()
    }
    cli::cli_progress_done()

    biom_eq <- do.call(rbind, res_B) |> as.data.frame()
    names(biom_eq) <- paste0("biom_eq_", 1:2)

    return(list(
        dat = cbind(biom_eq, a_21 = a_21, stab = unlist(res_stab)),
        x_sample = res_xsample
    ))
}