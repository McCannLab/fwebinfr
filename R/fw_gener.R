#' Generate random food webs
#'
#' @export
#'
#' @examples
#' fw_gen_ex1()
fw_gen_ex1 <- function() {
    A <- matrix(0, 2, 2)
    A[1, 1] <- -0.1
    A[1, 2] <- -0.2
    A[2, 1] <- 0.1
    R <- c(0.1, -0.05)
    B <- limSolve::lsei(E = A, F = -R)$X
    return(list(A = A, B = B, R = R))
}

# val <- fw_gen_5sp_2chains_01(add_sdB = FALSE)
# res <- val |> do.call(what = "fw_infer")
fw_gen_5sp_2chains_01 <- function(add_sdB = TRUE) {
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
    out <- list(A = A, B = B, R = R)
    if (add_sdB) out <- c(out, sdB = list(B / 20))
    return(out)
}

# val <- fw_gen_3sp_01() 
# res <- val |> do.call(what = "fw_infer")
# get_B_from_res(res, )
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

fw_gen_5sp_2chains <- function() {
    A <- matrix(0, 5, 5)
    A[1, 1] <- -0.1
    A[1, 2] <- -0.2
    A[2, 1] <- 0.1
    A[2, 3] <- -0.05
    A[3, 2] <- 0.05

    R <- c(0.1, -0.05, -0.01)
    B <- limSolve::lsei(E = A, F = -R)$X
    return(list(A = (A > 0) - (A < 0), B = B, R = R))
}