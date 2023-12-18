#' Estimate interaction strengths
#'
#' Function to estimate interaction strengths using LIM.
#' 
#' @param A interaction matrix
#' @param R reproduction/mortality.
#' @param B biomass matrix.
#' @param eff_max maximum transfer efficiency.
#'
#' @details
#' LIM based on generalized linear Lotka-Volterra model with the following 
#' interaction matrix:
#' \deqn{\frac{1/X}{X'} = A * X + R }{1/X X' = A * X + R}
#' See [limSolve::xsample()] for the meaning of matrices E, F, G and H.
#' 
#' @references 
#' * Gellner G, McCann K, Hastings A. 2023. Stable diverse food webs become more common when interactions are more biologically constrained. Proceedings of the National Academy of Sciences 120:e2212061120. DOI: 10.1073/pnas.2212061120.
#'
#' @export
fw_infer <- function(A, R, B, eff_max = 1) {
    stopifnot(inherits(A, "matrix"))
    stopifnot(NROW(A) == NCOL(A))
    stopifnot(NROW(A) == length(R))
    stopifnot(length(B) == length(R))
    U <- get_U(A)
    stopifnot(nrow(U) > 0)
    return(wrap_xsample(A, B, R, U, eff_max = eff_max))
}

# edge list: matrix p x 2 where p is is the number of non 0 interaction
# it goes column by column so is not-null, [2,1] would be indexed before [1,2].
get_U <- function(A) {
    return(which(A != 0, arr.ind = TRUE))
}

get_E_F <- function(A, B, R) {
    nr <- NROW(A)
    # determine the number of unknown interaction to be searched for
    # matrix U describes where are non-null interactions in A
    U <- get_U(A)
    ni <- NROW(U)

    # foodweb interaction provides the first set of constrains
    E_fw <- matrix(0, nr, ni)
    for (i in seq_len(nr)) {
        tmp <- A[i, ] * B
        for (j in seq(tmp)) {
            if (tmp[j] != 0) {
                ind_ij <- which(U[, 1] == i & U[, 2] == j)
                E_fw[i, ind_ij] <- tmp[j]
            }
        }
    }
    F_fw <- -R

    return(list(E = E_fw, F = F_fw))
}

get_G_H <- function(A, eff_max = 1) {
    U <- get_U(A)
    ni <- NROW(U)
    nr <- NROW(A)
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

    # All interaction are positive
    G_pos <- diag(1, ni)
    H_pos <- matrix(0, ncol = 1, nrow = ni)

    return(list(G = rbind(G_sym, G_pos), H = rbind(H_sym, H_pos)))
}

wrap_xsample <- function(A, B, R, U, eff_max = 1, mod = mod_lv_fr1) {
    # we use A to know the real interactions but in the methods
    # A takes 1, -1 or 0
    AA <- (A > 0) - (A < 0)
    U <- get_U(A)
    tmp <- do.call(
        limSolve::xsample,
        c(get_E_F(AA, B, R), get_G_H(AA, eff_max), burninlength = 5000)
    )
    out <- tmp$X |> as.data.frame()

    out$leading_ev <- get_xsample_stab(out, AA, B, R, U, mod = mod)

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

## Compute model with functional response type I
mod_lv_fr1 <- function(t, y, pars) {
    return(list((pars$A %*% y + pars$R) * y))
}
