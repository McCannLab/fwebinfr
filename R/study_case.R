#' Study case
#'
#' Helper function that includes study cases.
#'
#' @param add_sdB a logical. Should biomass variation be added?
#' @export
#'
#' @examples
#' fw_example_2species()
fw_example_2species <- function() {
    A <- rbind(c(-0.1, -0.2), c(0.1, 0))
    R <- c(0.1, -0.05)
    B <- limSolve::lsei(E = A, F = -R)$X
    return(fw_model(A, B, R))
}

#' @rdname fw_example_2species
#' @export
fw_example_5species_2chains <- function(add_sdB = FALSE) {
    A <- matrix(0, 5, 5)
    A[1, 1] <- -1
    A[2, 2] <- -1

    A[1, 3] <- -1
    A[2, 4] <- -1

    A[3, 1] <- 1
    A[3, 5] <- -1

    A[4, 2] <- 1
    A[4, 5] <- -1

    A[5, 3] <- 1
    A[5, 4] <- 1

    R <- c(500, 100, -500, -100, -1000)
    B <- c(10000, 1000, 200, 100, 20)
    if (add_sdB) {
        sdB <- list(B / 20)
    } else {
        sdB <- NULL
    }
    return(fw_problem(A, B, R, sdB = sdB))
}

fw_gen_3sp_01 <- function() {
    A <- matrix(0, 3, 3)
    A[1, 1] <- -0.1
    A[1, 2] <- -0.2
    A[2, 1] <- 0.1
    A[2, 3] <- -0.05
    A[3, 2] <- 0.05

    R <- c(0.1, -0.05, -0.01)
    B <- limSolve::lsei(E = A, F = -R)$X
    return(list(A = (A > 0) - (A < 0), B = B, R = R))
}

fw_gen_parms_exper2 <- function() {
    A <- matrix(0, 5, 5)
    A[1, 1] <- -0.1
    A[1, 3] <- -0.01

    A[2, 2] <- -0.1
    A[2, 4] <- -0.01

    A[3, 1] <- 0.005
    A[3, 5] <- -0.01

    A[4, 2] <- 0.005
    A[4, 5] <- -0.01

    A[5, 3] <- 0.005
    A[5, 4] <- 0.005

    R <- c(1000, 1000, -47, -47, -10)
    B <- limSolve::lsei(E = A, F = -R)$X
    list(A = A, B = B, R = R)
}
