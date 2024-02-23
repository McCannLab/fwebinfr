#' Create a foodweb model object
#'
#' Create an object of class `fw_model`.
#'
#' @param A interaction matrix. A square matrix describing interactions of the
#' system (see details).
#' @param R reproduction/mortality vector.
#' @param B biomass vector.
#'
#' @details
#' So far there is only one model supported, a general Lotka-Volterra with
#' a linear response.
#'
#' @return
#' An object of class `fw_problem` which is a named list of 4 elements:
#' * A: interaction matrix
#' * B: biomass vector
#' * R: mortality rate vector
#' * model: the underlying model
#'
#' @export
#'
#' @examples
#' A <- rbind(c(-1, -1), c(1, 0))
#' R <- c(0.1, -0.05)
#' B <- c(0.5, 0.25)
#' fw_model(A, B, R)
fw_model <- function(A, B, R) {
    x <- new_fw_model(A, B, R)
    validate_fw_model(x)
    x$leading_ev <- get_leading_ev(
        get_jacobian(x$A, x$B, x$R, mod = x$model)
    )
    x
}

#' Create models
#' @describeIn fw_model get model
#' @param model model to be used.
#' @export

fw_get_model <- function(model = c("lv_fr1")) {
    model <- match.arg(model)
    switch(model,
        lv_fr1 = mod_lv_fr1
    )
}


## Compute model with functional response type I
mod_lv_fr1 <- function(t, y, pars) {
    return(list((pars$A %*% y + pars$R) * y))
}


# builder
new_fw_model <- function(A, B, R) {
    structure(
        list(
            A = A,
            B = B,
            R = R,
            model = fw_get_model(),
            leading_ev = NA
        ),
        class = "fw_model"
    )
}

validate_fw_model <- function(x) {
    A <- x$A
    B <- x$B
    R <- x$R
    stopifnot(exprs = {
        inherits(A, "matrix")
        NROW(A) == NCOL(A)
        inherits(B, "numeric")
        inherits(R, "numeric")
        NROW(A) == length(R)
        length(B) == length(R)
    })
    x
}