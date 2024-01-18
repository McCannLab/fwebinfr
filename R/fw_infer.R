#' Estimate interaction strengths
#'
#' Function to estimate interaction strengths using LIM.
#' 
#' @param A interaction matrix. Either a signed matrix (0/-1/1) or a matrix 
#' with coefficients. In both cases the matrix is currently treated as a signed 
#' one.
#' @param R reproduction/mortality vector.
#' @param B biomass vector.
#' @param U unknow matrix. A two-columns matrix that includes unknowm column.
#' @param sdB vector of standard deviation biomass (see [limSolve::xsample()]).
#' @param eff_max maximum transfer efficiency.
#' @param ... further arguments passed to [limSolve::xsample()]).
#'
#' @details
#' LIM based on generalized linear Lotka-Volterra model with the following 
#' interaction matrix:
#' \deqn{\frac{1/X}{X'} = A * X + R }{1/X X' = A * X + R}
#' See [limSolve::xsample()] for the meaning of matrices E, F, G and H.
#' 
#' @return
#' Return a data frame with one column per interaction strength estimated in 
#' A (i.e. one per 1 or -1 in A). The number of row is given by the number of 
#' interaction estimated by `xsample()` (see parameter `iter` in `xsample()`). 
#' 
#' @references 
#' * Gellner G, McCann K, Hastings A. 2023. Stable diverse food webs become 
#' more common when interactions are more biologically constrained. Proceedings 
#' of the National Academy of Sciences 120:e2212061120. DOI: 10.1073/pnas.2212061120.
#'
#' @export
fw_infer <- function(A, R, B, U = NULL, eff_max = 1, sdB = NULL, ...) {
    stopifnot(exprs = {
        inherits(A, "matrix")
        NROW(A) == NCOL(A)
        NROW(A) == length(R)
        length(B) == length(R)
    })
    U <- check_U(U, A)

    if (!is.null(sdB)) {
        out <- wrap_xsample_AB(A, B, R, U, sdB, eff_max = eff_max, ...)
    } else {
        out <- wrap_xsample(A, B, R, U, eff_max = eff_max, ...)
    }
    return(structure(out, class = c("fw_predicted_int", class(out))))
}

# get interaction matrix from one result
#' @describeIn fw_infer returns predicted A.
#' @param x an object of class `fw_predicted_int`
#' @export
fw_get_A_predicted <- function(x, A, U = NULL) {
    stopifnot(inherits(x, "fw_predicted_int"))
    U <- check_U(U, A)
    x <- x |> 
        dplyr::select(!leading_ev)
    stopifnot(length(x) == nrow(U))
    AA <- (A > 0) - (A < 0)
    out <- AA * 0
    for (i in seq_len(nrow(U))) {
        out[U[i, 1], U[i, 2]] <- as.numeric(x[i]) * AA[U[i, 1], U[i, 2]]
    }
    out
}

# get Bodymass vector from one result
#' @describeIn fw_infer returns predicted B.
#' @export
fw_get_B_predicted <- function(x, A, R, U = NULL) {
    U <- check_U(U, A)
    limSolve::lsei(E = fw_get_A_predicted(x, A, U), F = -R)$X
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

get_E_F <- function(A, B, R) {
    nr <- nrow(A)
    # determine the number of unknown interaction to be searched for
    # matrix U gives the coordinates of non-null interactions in A
    U <- get_U(A)
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
    U <- get_U(A)
    ni <- NROW(U) # number of interaction
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

wrap_xsample <- function(A, B, R, U, eff_max = 1, mod = mod_lv_fr1, ...) {
    # we use A to know the real interactions but in the methods
    # A takes 1, -1 or 0
    AA <- (A > 0) - (A < 0)
    tmp <- do.call(
        limSolve::xsample,
        c(get_E_F(A = AA, B = B, R = R), get_G_H(AA, eff_max), ...)
    )
    out <- tmp$X |> 
        as.data.frame()
    names(out) <- paste0("a_", apply(U, 1, paste, collapse = "_"))
    out$leading_ev <- get_xsample_stab(out, AA, B, R, U, mod = mod)
    return(out)
}

wrap_xsample_AB <- function(A, B, R, U, sdB, eff_max = 1, mod = mod_lv_fr1, ...) {
    # we use A to know the real interactions but in the methods
    # A takes 1, -1 or 0
    AA <- (A > 0) - (A < 0)
    tmp <- get_E_F(AA, B, R)
    ls_AB <- list(A = tmp$E, B = tmp$F, sdB = sdB)
    tmp <- do.call(
        limSolve::xsample,
        c(ls_AB, get_G_H(AA, eff_max), ...)
    )
    out <- tmp$X |> 
        as.data.frame()
    names(out) <- paste0("a_", apply(U, 1, paste, collapse = "_"))
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
