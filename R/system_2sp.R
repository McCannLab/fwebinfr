library(deSolve)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
dir.create("fig/", showWarnings = FALSE)


times <- seq(0, 1000, by = 0.01)

lvr <- function(t, y, pars) {
    with(as.list(c(y, pars)), {
        dx1 <- (-a11 * x1 - a12 * x2 + r1 + runif(1) * 0.005) * x1
        dx2 <- (a21 * x1 - r2 + runif(1) * 0.005) * x2
        return(list(c(dx1, dx2)))
    })
}

lv <- function(t, y, pars) {
    with(as.list(c(y, pars)), {
        dx1 <- (-a11 * x1 - a12 * x2 + r1) * x1
        dx2 <- (a21 * x1 - r2) * x2
        return(list(c(dx1, dx2)))
    })
}

get_jac <- function(pars, xeq) {
    jac <- rbind(
        c(
            -2 * pars["a11"] * xeq[1] - pars["a12"] * xeq[2] + pars["r1"],
            -pars["a12"] * xeq[1]
        ),
        c(
            pars["a21"] * xeq[2],
            pars["a21"] * xeq[1] - pars["r2"]
        )
    )
    Re(eigen(jac)$value[1])
}


pars <- c(a11 = 0.1, a12 = 0.2, a21 = 0.1, r1 = 0.1, r2 = 0.05)
out <- ode(
    c(x1 = 0.2, x2 = 0.2),
    times,
    lv,
    pars
)
out |>
    as_tibble() |>
    mutate_all(as.numeric) |>
    pivot_longer(-time, names_to = "variable", values_to = "value") |>
    ggplot() +
    aes(x = time, y = value, color = variable) +
    geom_line()
ggsave("fig/sys2sp.png")


out <- ode(
    c(x1 = 0.2, x2 = 0.2),
    times,
    lvr,
    pars
)
out |>
    as_tibble() |>
    mutate_all(as.numeric) |>
    pivot_longer(-time, names_to = "variable", values_to = "value") |>
    ggplot() +
    aes(x = time, y = value, color = variable) +
    geom_line()
ggsave("fig/sys2sp_noise.png")


# Equilibrium
xeq <- apply(out[nrow(out) + seq(-10000, 0), ], 2, mean)[2:3]
xeq <- c(0.5, 0.25)

get_mat_val <- function(xeq) {
    matE <- rbind(
        c(-xeq[1], -xeq[2], 0),
        c(0, 0, xeq[1])
    )
    res <- xsample(
        E = matE,
        F = matrix(c(-0.1, 0.05)),
        G = rbind(
            c(0, 1, -1), # a12 >= a21
            # c(-1, 0, 0),   # a11 <= 1/x1*(r1-x2/x1*r2)
            c(1, 0, 0), # a11 >= 0
            c(0, 1, 0), # a12 >= 0
            c(0, 0, 1) # a21 >= 0
        ),
        H = matrix(
            c(
                0,
                #-1/xeq[2]*(pars["r1"]-(xeq[2]/xeq[3])*pars["r2"]),
                0,
                0,
                0
            ),
            nrow = 4
        ),
        burninlength = 1e4,
        iter = 2000
    )
    mat_val <- res$X |> as.data.frame()
    mat_val$stab <- NA_real_
    for (i in seq_len(nrow(mat_val))) {
        jac <- rbind(
            c(-mat_val[i, 1], -mat_val[i, 2]),
            c(0, mat_val[i, 3])
        )
        mat_val$stab[i] <- get_jac(
            c(
                a11 = mat_val[i, 1],
                a12 = mat_val[i, 2],
                a21 = mat_val[i, 3],
                pars["r1"],
                pars["r2"]
            ),
            xeq
        )
    }
    mat_val
}
mat_val <-  get_mat_val(xeq)

mat_xeq <- out[50000 + sample(seq_len(50000))[1:100], 2:3]
kk <- apply(mat_xeq, 1, get_mat_val)
mat_val  <- do.call(rbind, kk)

png(filename = "fig/box_2sp2.png", height = 6, width = 12, res = 300, units = "in")
par(mfrow = c(1, 3))

boxplot(mat_val[mat_val$stab < 0, 1:3], ylim = c(0, 0.5))
points(1:3, pars[c("a11", "a12", "a21")], pch = 19, cex = 2, col = "steelblue")

boxplot(mat_val[mat_val$stab <= quantile(mat_val$stab, 0.5), 1:3], ylim = c(0, 0.5))
points(1:3, pars[c("a11", "a12", "a21")],
    pch = 19, cex = 2,
    col = "steelblue"
)

boxplot(mat_val[mat_val$stab > quantile(mat_val$stab, 0.5), 1:3], ylim = c(0, 0.5))
points(1:3, pars[c("a11", "a12", "a21")],
    pch = 19, cex = 2,
    col = "steelblue"
)

dev.off()