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

# fw_gen_5sp_2chains_01() |> do.call(what = "fw_infer")
fw_gen_5sp_2chains_01  <- function(add_sdB = TRUE) {
        A <- matrix(0, 5, 5)
        A[1, 3] <- -1
        A[2, 4] <- -1

        A[3, 1] <- 1
        A[3, 5] <- -1

        A[4, 2] <- 1
        A[4, 5] <- -1

        A[5, 3] <- 1
        A[5, 4] <- 1

        # R <- c(500, 100, -50, -200, -1000)
        # B <- c(1000, 500, 500, 200, 1)
        # R <- c(500, 100, -500, -100, -1000)
        # B <- c(10000, 1000, 200, 100, 1)
        R <- c(500, 100, -500, -100, -1000)
        B <- c(10000, 1000, 200, 100, 20)
        out  <- list(A = A, B = B, R = R)
        if (add_sdB) out  <- c(out, sdB = list(B/20))
        return(out)
}

fw_gen_5sp_2chains <- function() {
    A <- matrix(0, 5, 5)
    A[1, 1] <- -0.01
    A[1, 3] <- -1
    A[2, 4] <- -2
    A[2, 2] <- -0.01
    
    A[3, 1] <- 0.5
    A[3, 5] <- -0.001

    A[4, 2] <- 1
    A[4, 5] <- -0.005
    
    A[5, 3] <- 0.005
    A[5, 4] <- 0.001
    
    R <- c(10, 1, -0.1, -0.1, -0.06)
    B <- limSolve::lsei(E = A, F = -R)$X
    return(list(A = A, B = B, R = R))
}