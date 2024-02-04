#' Create Foodweb model object
#'
#' Create an object of class `fw_problem` that can be used for interaction
#' inference.
#'
#' @param A interaction matrix. A square matrix describing interactions of the
#' system (see details).
#' @param R reproduction/mortality vector.
#' @param B biomass vector.
#' @param U unknow interactions. A three-columns data frame that includes the
#' position of unknown/known interactions. If `NULL`, U is derived from A.
#' @param sdB vector of standard deviation biomass (see [limSolve::xsample()]).
#'
#' @details
#' In most cases, only `A` needs to be defined and should include:
#'  * 1 when interactions are present and positve,
#'  * -1 when interactions are present and negative,
#'  * 0 otherwise.
#' `U` will be generated according to this these values. `A` may however
#' includes known interactions, in such case, `U` needs to be defined and 
#' must includes all known interactions (unknown interactions will be 
#' identified using `A`).
#' TODO: S may not be required if U has signed info, or U may not be required if
#' S points to only unknown info. I think ultimately using an Edge list + Node
#' info is the way to go.
#'
#' @return
#' An object of class `fw_problem` which is a named list of 7 elements:
#' * A: interaction matrix
#' * B: biomass vector
#' * R: mortality rate vector
#' * U: unknown interaction(s)
#' * S: square matrix representing the signed structure (only 1/-1 and 0)
#' * SdB: biomass variation
#' * model: the underlying model
#' 
#' @export
#' 
#' @examples
#' A <- rbind(c(-1, -1), c(1, 0))
#' R <- c(0.1, -0.05)
#' B <- c(0.5, 0.25)
#' fw_problem(A, B, R)
#'
fw_problem <- function(A, B, R, U = NULL, sdB = NULL) {
    x  <- new_fw_problem(A, B, R, U, sdB)
    validate_fw_problem(x)
}

#' @describeIn fw_problem Coerce an object to an object of class `fw_problem`.
#' @param x an R object.
#' @param ... extra arguments (ignored so far, might be use to custom xsample 
#' at some point).
#' @export 
fw_as_problem  <- function(x, ...) {
    UseMethod("fw_as_problem")
}

#' @export
fw_as_problem.fw_problem <- function(x, ...) {
    validate_fw_problem(x)
}

#' @export
fw_as_problem.fw_model <- function(x, ...) {
    validate_fw_model(x)
    new_fw_problem(A = x$A, B = x$B, R = x$R, U = NULL, sdB = NULL)  |>
        validate_fw_problem()
}


get_S_from_A <- function(A) {
    (A > 0) + (A < 0) * -1
}


# edge list: matrix p x 2 where p is is the number of non-0 interactions,
# it goes column by column therefore if {1,2} and {2,1} are both not-null,
# {2,1} would be indexed before {1,2}.
get_U_from_A <- function(A) {
    return(which(A != 0, arr.ind = TRUE))
}

create_U <- function(U, A) {
    if (is.null(U)) {
        U <- get_U_from_A(A) |> as.data.frame()
        colnames(U) <- c("row", "col")
        U$unknown <- TRUE
    } else {
        U  <- validate_U(U, A)
        # check whether they are additionnal unknown interactions
        U2 <- dplyr::setdiff(
            get_U_from_A(A) |> as.data.frame(),
            U[c("col", "row")]
        )
        if (nrow(U2)) {
            U2$unknown <- TRUE
            U <- rbind(U, U2)
        }
    }
    U
}


# builder
new_fw_problem <- function(A, B, R, U, sdB) {
    mod <- new_fw_model(A, B, R)
    tmp_U <- create_U(U, A)
    # interaction not mentioned
    structure(
        list(
            A = mod$A,
            B = mod$B,
            R = mod$R,
            S = get_S_from_A(A),
            U = tmp_U,
            sdB = sdB,
            model = mod$model
        ),
        class = "fw_problem"
    )
}

validate_fw_problem <- function(x) {
    validate_U(x$U, x$A)
    validate_sdB(x$sdB, x$B)
    validate_fw_model(new_fw_model(x$A, x$B, x$R))
    x
}

validate_U  <- function(U, A) {
    stopifnot(exprs = {
        inherits(U, "data.frame")
        nrow(U) > 0
        all(U$row %in% seq_len(nrow(A)))
        all(U$col %in% seq_len(ncol(A)))
        identical(colnames(U), c("row", "col", "unknown"))
    })
    U
}

validate_sdB <- function(sdB, B) {
     if (!is.null(sdB)) {
         stopifnot(length(sdB) == length(B))
     }
     sdB
}