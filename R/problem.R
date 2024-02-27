#' Create Foodweb model object
#'
#' Create an object of class `fw_problem` that can be used for interaction
#' inference.
#'
#' @param A interaction matrix. A square matrix describing interactions of the
#' system (see details). If `NULL`, `A` is derived from `U`.
#' @param R reproduction/mortality vector.
#' @param B biomass vector.
#' @param U unknown interactions. A data frame that includes the
#' position of unknown/known interactions. If `NULL`, `U` is derived from `A`.
#' @param sdB vector of standard deviation biomass (see [limSolve::xsample()]).
#'
#' @details
#' In most cases, only `A` needs to be defined and should include:
#'  * 1 when interactions are present and positive,
#'  * -1 when interactions are present and negative,
#'  * 0 otherwise.
#' `U` will be generated according to these values. `A` may however
#' includes known interactions, in such case, `U` needs to be defined and
#' must includes all known interactions (unknown interactions will be
#' identified using `A`).
#' It is also possible to only declare `U`, in this case the dimension of A and
#' its coefficients will be derived from `U`. `U` has a strict format, namely
#' a data frame with 5 columns:
#' * `name`: interaction name, when `U` is created from `A` name is `a_i_j`
#' where `i` is the row number and `j` the column number,
#' * `row`: row number,
#' * `col`: column number,
#' * `unknown`: a logical vector that identifies unknown and known interactions,
#' * `value`: interaction value, only the sign matter for unknown interactions,
#' The order does not matter as the columns are ordered during the validation
#' step.
#'
#' @return
#' An object of class `fw_problem` which is a named list of 7 elements:
#' * A: interaction matrix
#' * B: biomass vector
#' * R: mortality rate vector
#' * U: unknown interaction(s)
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
    x <- new_fw_problem(A, B, R, U, sdB)
    validate_fw_problem(x)
}

#' @describeIn fw_problem Coerce an object to an object of class `fw_problem`.
#' @param x an R object.
#' @param ... extra arguments (ignored so far, might be use to custom xsample
#' at some point).
#' @export
fw_as_problem <- function(x, ...) {
    UseMethod("fw_as_problem")
}

#' @export
fw_as_problem.fw_problem <- function(x, ...) {
    validate_fw_problem(x)
}

#' @export
fw_as_problem.fw_model <- function(x, ...) {
    validate_fw_model(x)
    new_fw_problem(A = x$A, B = x$B, R = x$R, U = NULL, sdB = NULL) |>
        validate_fw_problem()
}


create_U_from_A <- function(A) {
    U <- which(A != 0, arr.ind = TRUE) |> as.data.frame()
    colnames(U) <- c("row", "col")
    U$unknown <- TRUE
    U$value <- A[U[, c("row", "col")] |> as.matrix()]
    U$name <- paste0(
        "a_", apply(U[, c("row", "col")], 1, paste, collapse = "_")
    )
    return(U)
}

create_A_from_U <- function(U) {
    nsp <- max(c(U$row, U$col))
    A <- matrix(0, nrow = nsp, ncol = nsp)
    A[get_matrix_index(U)] <- (U$value[U$unknown] > 0) * 2 - 1
    if (any(!U$unknown)) {
        A[get_matrix_index(U, FALSE)] <- U$value[!U$unknown]
    }
    A
}

# useful when there are some info in U and other in A
create_U_with_A <- function(U, A) {
     #   browser()
    tmp_U <- create_U_from_A(A)
    # check whether they are additional unknown interactions
    id <- paste0(tmp_U$row, "_", tmp_U$col) %in% paste0(U$row, "_", U$col)
    if (sum(!id)) {
        U <- rbind(U, tmp_U[!id, ])
    }

    # need to have all unknown interactions first because the row number is
    # use to identify unknown variables
    U[
        order(U$unknown, decreasing = TRUE),
        c("name", "row", "col", "unknown", "value")
    ]
}


create_A <- function(A, U) {
    out <- (A > 0) + (A < 0) * -1
    UU <- U[!U$unknown, c("row", "col")] |> as.matrix()
    if (nrow(UU)) {
        out[UU] <- A[UU]
        out
    } else {
        out
    }
}


# builder
new_fw_problem <- function(A, B, R, U, sdB) {
    stopifnot(!is.null(A) || !is.null(U))
    if (is.null(A)) {
        tmp_U <- validate_U(U)
        tmp_A <- create_A_from_U(U)
    } else {
        tmp_A <- validate_A(A)
        if (is.null(U)) {
            tmp_U <- create_U_from_A(A)
        } else {
            tmp_U <- validate_U(U)
            tmp_U <- create_U_with_A(tmp_U, A)
        }
    }
    mod <- new_fw_model(A, B, R)
    row.names(tmp_U) <- NULL

    structure(
        list(
            A = tmp_A,
            B = mod$B,
            R = mod$R,
            U = tmp_U,
            sdB = sdB,
            model = mod$model
        ),
        class = "fw_problem"
    )
}

validate_fw_problem <- function(x) {
    stopifnot(exprs = {
        all(x$U$row %in% seq_len(nrow(x$A)))
        all(x$U$col %in% seq_len(nrow(x$A)))
        any(x$U$unknown)
        !any(duplicated(x$U$name))
    })
    validate_sdB(x$sdB, x$B)
    validate_fw_model(new_fw_model(x$A, x$B, x$R))
    x
}

validate_U <- function(U) {
    stopifnot(exprs = {
        inherits(U, "data.frame")
        nrow(U) > 0
        identical(
            colnames(U) |> sort(),
            c("name", "row", "col", "unknown", "value") |> sort()
        )
        inherits(U$value, "numeric")
        all(!is.na(U$value))
        # at least one interaction should be unknown
        inherits(U$unknown, "logical")
    })
    U[
        order(U$unknown, decreasing = TRUE),
        c("name", "row", "col", "unknown", "value")
    ]
}

validate_A <- function(A) {
    stopifnot(exprs = {
        inherits(A, "matrix")
        ncol(A) > 0
        ncol(A) == nrow(A)
        inherits(A |> as.vector(), "numeric")
    })
    A
}

validate_sdB <- function(sdB, B) {
    if (!is.null(sdB)) {
        stopifnot(length(sdB) == length(B))
    }
    sdB
}


# used U as a matrix for indexing A
get_matrix_index <- function(U, unknown = TRUE, both = FALSE) {
    if (both) {
        U[, c("row", "col")] |> as.matrix()
    } else {
        if (unknown) {
            U[U$unknown, c("row", "col")] |> as.matrix()
        } else {
            U[!U$unknown, c("row", "col")] |> as.matrix()
        }
    }
}
