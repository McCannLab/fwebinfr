#' Estimate interaction strengths
#'
#' Function to estimate interaction strengths using LIM.
#'
#' @param x an object of class `fw_problem`.
#' @param eff_max max efficiency.
#' @param ... further arguments passed to [limSolve::xsample()]).
#'
#' @details
#' LIM based on generalized linear Lotka-Volterra model with the following
#' interaction matrix:
#' \deqn{\frac{1/X}{X'} = A * X + R }{1/X X' = A * X + R}
#' See [limSolve::xsample()] for the meaning of matrices E, F, G and H.
#'
#' @return
#' Return a list of two elements:
#'  * `prediction`: a data frame with one column per interaction strength
#' estimated. The number of row is given by the number of interaction sets
#' estimated by `xsample()` (see parameter `iter` in `xsample()`).
#'
#' @references
#' * Gellner G, McCann K, Hastings A. 2023. Stable diverse food webs become
#' more common when interactions are more biologically constrained. Proceedings
#' of the National Academy of Sciences 120:e2212061120. DOI: 10.1073/pnas.2212061120.
#'
#' @export
#'
#' @examples
#' A <- rbind(c(-1, -1), c(1, 0))
#' R <- c(0.1, -0.05)
#' B <- c(0.5, 0.25)
#' res <- fw_problem(A, B, R) |> fw_infer()
#'
fw_infer <- function(x, eff_max = 1, ...) {
    stopifnot(inherits(x, "fw_problem"))
    out <- wrap_xsample(x, eff_max = eff_max, ...)
    return(
        structure(
            list(prediction = out, problem = x),
            class = c("fw_predicted")
        )
    )
}

# get interaction matrix from one result
#' @describeIn fw_infer returns predicted A.
#' @param y an object of class `fw_predicted`
#' @param index index of the prediction to be used.
#' @export
fw_predict_A <- function(y, index = 1) {
    stopifnot(inherits(y, "fw_predicted"))
    x <- y$problem
    UU <- get_matrix_index(x$U)
    y <- y$prediction[index, ] |>
        dplyr::select(!leading_ev)
    stopifnot(length(y) == nrow(UU))

    out <- x$A
    for (i in seq_len(nrow(UU))) {
        out[UU[i, 1], UU[i, 2]] <- as.numeric(y[i]) * x$A[UU[i, 1], UU[i, 2]]
    }
    out
}

# get Bodymass vector from one result
#' @describeIn fw_infer returns predicted B.
#' @export
fw_predict_B <- function(y, index = 1) {
    limSolve::lsei(E = fw_predict_A(y, index), F = -y$problem$R)$X
}


# equalities
get_E_F <- function(A, B, R, U) {
    nr <- nrow(A)
    ni <- nrow(U[U$unknown, ])

    AU <- get_unknown_interactions(A, U)
    AK <- get_known_interactions(A, U)

    # food web interaction gives a first set of constrains
    E_fw <- matrix(0, nr, ni)
    F_fw <- -R
    for (i in seq_len(nr)) {
        # A is either -1, 0 or 1 for unknown interaction
        tmpU <- AU[i, ] * B
        # for known interactions we use this quantity sum it and add to F
        tmpK <- AK[i, ] * B
        for (j in seq_len(nr)) {
            if (tmpU[j] != 0) {
                B
                # LHS
                ind_ij <- which(U$row == i & U$col == j)
                E_fw[i, ind_ij] <- tmpU[j]
            }
        }
        # all known interactions
        F_fw[i] <- F_fw[i] - sum(tmpK)
    }

    return(list(E = E_fw, F = F_fw))
}


# inequalities
get_G_H <- function(A, U, eff_max = 1) {
    UU <- U[U$unknown, ]
    ni <- nrow(UU) # number of interactions to infer
    nr <- NROW(A) # number of species
    # derive constraints using eff_max * a_{j,i} >= a_{i,j}
    # eff_max * a_{j,i} - a_{i,j} >= 0
    H_sym <- G_sym <- NULL
    k <- 0
    tmpG <- tmpH <- list()
    for (i in seq_len(nr - 1)) {
        # skip interactions on diagonal
        for (j in seq(i + 1, nr)) {
            ind_ij <- which(U$row == i & U$col == j)
            ind_ji <- which(U$row == j & U$col == i)
            # remember unknown interactions first, known second
            if (length(ind_ij) && length(ind_ji)) {
                if (!U$unknown[ind_ij] && !U$unknown[ind_ji]) {
                    next
                }
                k <- k + 1
                tmpG[[k]] <- rep(0, ni)
                if (U$unknown[ind_ij] && U$unknown[ind_ji]) {
                    # both are unknown
                    if (A[j, i] > 0) {
                        tmpG[[k]][ind_ij] <- eff_max
                        tmpG[[k]][ind_ji] <- -1
                    } else {
                        tmpG[[k]][ind_ji] <- eff_max
                        tmpG[[k]][ind_ij] <- -1
                    }
                    tmpH[[k]] <- 0
                } else if (!U$unknown[ind_ij]) {
                    # A[i, j] is known, so variable ind_ji is unknown
                    # one less unknown, but still the inequalities remain
                    if (A[j, i] > 0) {
                        tmpG[[k]][ind_ji] <- eff_max
                        tmpH[[k]] <- A[i, j]
                    } else {
                        tmpG[[k]][ind_ji] <- -eff_max * A[i, j]
                        tmpH[[k]] <- -1
                    }
                } else {
                    # A[j, i] is known
                    if (A[j, i] > 0) {
                        tmpG[[k]][ind_ij] <- eff_max
                        tmpH[[k]] <- A[j, i]
                    } else {
                        tmpG[[k]][ind_ij] <- -eff_max * A[j, i]
                        tmpH[[k]] <- -1
                    }
                }
            }
        }
    }

    if (k > 0) {
        G_sym <- do.call(rbind, tmpG)
        H_sym <- matrix(unlist(tmpH), ncol = 1)
    }

    # All interactions are positive
    G_pos <- diag(1, ni)
    H_pos <- matrix(0, ncol = 1, nrow = ni)

    return(list(G = rbind(G_sym, G_pos), H = rbind(H_sym, H_pos)))
}

# sampling
wrap_xsample <- function(x, eff_max = 1, ...) {
    # tryCatch
    tmp <- do.call(
        limSolve::xsample,
        c(
            get_E_F(A = x$A, B = x$B, R = x$R, U = x$U),
            get_G_H(x$A, x$U, eff_max),
            ...
        )
    )

    if (is.atomic(tmp)) {
        out <- tmp |>
            t() |>
            as.data.frame()
    } else {
        out <- tmp$X |> as.data.frame()
    }
    names(out) <- paste0(
        "a_",
        apply(x$U[x$U$unknown, c("row", "col")], 1, paste, collapse = "_")
    )
    out$leading_ev <- get_xsample_stab(out, x$A, x$B, x$R, x$U, mod = x$model)
    return(out)
}

get_jacobian <- function(A, B, R, mod = mod_lv_fr1) {
    rootSolve::jacobian.full(B, mod, parms = list(A = A, R = R))
}

get_leading_ev <- function(J) {
    out <- eigen(J)
    return(out$values |> Re() |> max())
}

get_xsample_stab_unit <- function(x, A, B, R, U, mod) {
    A_sim <- predict_A(unlist(x), A, U)
    return(get_leading_ev(get_jacobian(A_sim, B, R, mod)))
}


get_xsample_stab <- function(X, A, B, R, U, mod = mod_lv_fr1) {
    stopifnot(NROW(U[U$unknown, ]) == NCOL(X))
    out <- apply(
        X,
        1,
        get_xsample_stab_unit,
        A = A,
        B = B,
        R = R,
        U = U,
        mod = mod
    )
    return(out)
}

get_known_interactions <- function(A, U) {
    if (sum(U$unknown)) {
        UU <- get_matrix_index(U)
        A[as.matrix(UU)] <- 0
    }
    return(A)
}

get_unknown_interactions <- function(A, U) {
    if (sum(!U$unknown)) {
        UU <- get_matrix_index(U, FALSE)
        A[as.matrix(UU)] <- 0
    }
    return(A)
}

predict_A <- function(y, A, U) {
    UU <- U[U$unknown, ]
    out <- A
    out[UU[, c("row", "col")] |> as.matrix()] <- as.numeric(y) * UU$value 
    out
}