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