#' Estimate interaction strengths
#'
#' Function to estimate interaction strengths using LIM.
#'
#' @param x an object of class `fw_problem`.
#' @param eff_max max efficience. 
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
#' res  <- fw_problem(A, B, R) |> fw_infer()
#' 
fw_infer <- function(x, eff_max = 1, ...) {
    stopifnot(inherits(x, "fw_problem"))

    if (!is.null(x$sdB)) {
        out <- wrap_xsample_AB(x, eff_max = eff_max, ...)
    } else {
        out <- wrap_xsample(x, eff_max = eff_max, ...)
    }
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
    y <- y$prediction[index, ] |>
        dplyr::select(!leading_ev)
    stopifnot(length(y) == nrow(x$U))

    out <- x$S * 0
    for (i in seq_len(nrow(x$U))) {
        out[x$U[i, 1], x$U[i, 2]] <- as.numeric(y[i]) * x$S[x$U[i, 1], x$U[i, 2]]
    }
    out
}

# get Bodymass vector from one result
#' @describeIn fw_infer returns predicted B.
#' @export
fw_predict_B <- function(y, index = 1) {
    limSolve::lsei(E = fw_predict_A(y, index), F = -y$problem$R)$X
}




get_E_F <- function(A, B, R) {
    nr <- nrow(A)
    # determine the number of unknown interactions to be searched for
    # matrix U gives the coordinates of non-null interactions in A
    U <- get_U_from_A(A)
    ni <- nrow(U)

    # foodweb interaction gives a first set of constrains
    E_fw <- matrix(0, nr, ni)
    for (i in seq_len(nr)) {
        # A is either -1, 0 or 1
        tmp <- A[i, ] * B
        for (j in seq_len(nr)) {
            if (tmp[j] != 0) {
                ind_ij <- which(U[, 1] == i & U[, 2] == j)
                E_fw[i, ind_ij] <- tmp[j]
            }
        }
    }
    #
    F_fw <- -R

    return(list(E = E_fw, F = F_fw))
}

get_G_H <- function(A, eff_max = 1) {
    U <- get_U_from_A(A)
    ni <- NROW(U) # number of interaction to infer
    nr <- NROW(A) # number of spec
    # derive constraints using eff_max * a_{j,i} >= a_{i,j}
    # eff_max * a_{j,i} - a_{i,j} >= 0
    H_sym <- G_sym <- NULL
    k <- 0
    tmp <- list()
    for (i in seq_len(nr - 1)) {
        for (j in seq(i + 1, nr)) {
            if (A[i, j] != 0 && A[j, i] != 0) {
                ind_ij <- which(U[, 1] == i & U[, 2] == j)
                ind_ji <- which(U[, 1] == j & U[, 2] == i)
                k <- k + 1
                tmp[[k]] <- rep(0, ni)
                tmp[[k]][ind_ij] <- eff_max
                tmp[[k]][ind_ji] <- -1
            }
        }
    }

    if (k > 0) {
        G_sym <- do.call(rbind, tmp)
        H_sym <- matrix(0, ncol = 1, nrow = NROW(G_sym))
    }

    # All interactions are positive
    G_pos <- diag(1, ni)
    H_pos <- matrix(0, ncol = 1, nrow = ni)

    return(list(G = rbind(G_sym, G_pos), H = rbind(H_sym, H_pos)))
}

wrap_xsample <- function(x, eff_max = 1, ...) {
    tmp <- do.call(
        limSolve::xsample,
        c(get_E_F(A = x$S, B = x$B, R = x$R), get_G_H(x$S, eff_max), ...)
    )
    out <- tmp$X |> as.data.frame()
    names(out) <- paste0(
        "a_", 
        apply(x$U[c("row", "col")], 1, paste, collapse = "_")
    )
    out$leading_ev <- get_xsample_stab(out, x$S, x$B, x$R, x$U, mod = x$model)
    return(out)
}

wrap_xsample_AB <- function(x, eff_max = 1, ...) {
    # we use A to know the real interactions but in the methods
    # A takes 1, -1 or 0
    tmp <- get_E_F(x$S, x$B, x$R)
    ls_AB <- list(A = tmp$E, B = tmp$F, sdB = x$sdB)
    tmp <- do.call(
        limSolve::xsample,
        c(ls_AB, get_G_H(x$S, eff_max), ...)
    )
    out <- tmp$X |> as.data.frame()
    names(out) <- paste0("a_", apply(x$U, 1, paste, collapse = "_"))
    out$leading_ev <- get_xsample_stab(out, x$S, x$B, x$R, x$U, mod = x$model)
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
    A_sim <- get_Asim(unlist(x), A, U)
    return(get_leading_ev(get_jacobian(A_sim, B, R, mod)))
}

get_Asim <- function(x, A, U) {
    A_sim <- matrix(0, NROW(A), NROW(A))
    for (i in seq(x)) {
        A_sim[U[i, 1], U[i, 2]] <- x[i] * A[U[i, 1], U[i, 2]]
    }
    return(A_sim)
}

get_xsample_stab <- function(X, A, B, R, U, mod = mod_lv_fr1) {
    stopifnot(NROW(U) == NCOL(X))
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
