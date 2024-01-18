#' Create Foodweb model object
#'
#' @param A interaction matrix. Either a signed matrix (0/-1/1) or a matrix
#' with coefficients. In both cases the matrix is currently treated as a signed
#' one.
#' @param R reproduction/mortality vector.
#' @param B biomass vector.
#' @param U unknow matrix. A two-columns matrix that includes unknowm column.
#' @param sdB vector of standard deviation biomass (see [limSolve::xsample()]).
#' 
#' @return 
#' An object of class `class` which is a named list of 6 elements: 
#' * A 
#' * B 
#' * R 
#' * U 
#' * SdB
#' * model: the underlying model
#' @export
#' 
fw_create <- function(A, B, R, U = NULL, sdB = NULL) {
    
    stopifnot(exprs = {
        inherits(A, "matrix")
        inherits(B, "numeric")
        inherits(R, "numeric")
        NROW(A) == NCOL(A)
        NROW(A) == length(R)
        length(B) == length(R)
    })

    structure(
        list(
            A = A,
            B = B,
            R = R,
            U = check_U(U, A),
            sdB = sdB,
            model = fw_model()
        ),
        class = "fwmod"
    )
}
