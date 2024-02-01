#' Create Foodweb model object
#'
#' Create a object of class `fwmod` that can be used for interaction inference.
#'
#' @param A interaction matrix. A square matrix describing interactions of the
#' system (see details).
#' @param R reproduction/mortality vector.
#' @param B biomass vector.
#' @param S a square matrix describing the sign structure of the system. It
#' must have the same dimension of A and only includes 0, -1 and 1. If `NULL`, S
#' is derived from A.
#' @param U unknow matrix. A two-columns matrix that includes the position of unknown interaction.
#' If `NULL`, U is derived from A.
#' @param sdB vector of standard deviation biomass (see [limSolve::xsample()]).
#'
#' @details
#' In most cases, only `A` needs to be defined and should include:
#'  * 1 when interactions are present and positve,
#'  * -1 when interactions are present and negative,
#'  * 0 otherwise.
#' Matrix `A` will be used to generate `S` and `U`.
#' `A` may also includes known interactions that need not be infered. In such 
#' case U must be defined accordingly. Similarly, if A only includes 
#' interaction without their sign, S must be defined properly to perform the 
#' correct inference. 
#' TODO: S may not be required is U has signed info, or U may not be required if
#' S points to only unknown info.
#'
#' @return
#' An object of class `fwmod` which is a named list of 7 elements:
#' * A: interaction matrix
#' * B: biomass vector
#' * R: mortality rate vector
#' * U: unknown interaction
#' * S: signed structure matrix
#' * SdB: biomass variation
#' * model: the underlying model
#' @export
#' @examples 
#' A <- rbind(c(-1, -1), c(1, 0))
#' R <- c(0.1, -0.05)
#' B <- c(0.5, 0.25)
#' fw_create(A, B, R)
#' 
fw_create <- function(A, B, R, S = NULL, U = NULL, sdB = NULL) {
    stopifnot(exprs = {
        inherits(A, "matrix")
        NROW(A) == NCOL(A)
        inherits(B, "numeric")
        inherits(R, "numeric")
        NROW(A) == length(R)
        length(B) == length(R)
    })

    structure(
        list(
            A = A,
            B = B,
            R = R,
            S = check_S(S, A),
            U = check_U(U, A),
            sdB = sdB,
            model = fw_model()
        ),
        class = "fwmod"
    )
}

get_S <- function(A) {
    (A > 0) + (A < 0) * -1
}

check_S <- function(S, A) {
    if (is.null(S)) {
        S <- get_S(A)
    }
    stopifnot(exprs = {
        inherits(S, "matrix")
        identical(dim(S), dim(A))
        all(S %in% c(-1, 0, 1))
    })
    S
}


# edge list: matrix p x 2 where p is is the number of non-0 interactions,
# it goes column by column therefore if {1,2} and {2,1} are both not-null,
# {2,1} would be indexed before {1,2}.
get_U <- function(A) {
    return(which(A != 0, arr.ind = TRUE))
}

check_U <- function(U, A) {
    if (is.null(U)) {
        U <- get_U(A)
    }
    stopifnot(exprs = {
        inherits(U, "matrix")
        nrow(U) > 0
        ncol(U) == 2
    })
    colnames(U) <- c("row", "col")
    stopifnot(exprs = {
        all(U[, 1L] %in% seq_len(nrow(A)))
        all(U[, 2L] %in% seq_len(ncol(A)))
    })
    U
}