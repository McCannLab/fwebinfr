library(deSolve)
library(dplyr)
library(ggplot2)
library(limSolve)
library(tibble)
library(tidyr)
dir.create("fig/", showWarnings = FALSE)


times <- seq(0, 2000, by = 0.01)

lv <- function(t, y, pars) {
    with(as.list(c(y, pars)), {
        dx1 <- (-a11 * x1 - a12 * x2 + r1) * x1
        dx2 <- (a21 * x1 - a22 * x2 - a23 * x3 - r2) * x2
        dx3 <- (a32 * x2 - r3) * x3
        return(list(c(dx1, dx2, dx3)))
    })
}

lvr <- function(t, y, pars) {
    with(as.list(c(y, pars)), {
        dx1 <- (-a11 * x1 - a12 * x2 + r1 + 0.001 * runif(1)) * x1
        dx2 <- (a21 * x1 - a22 * x2 - a23 * x3 - r2 + 0.001 * runif(1)) * x2
        dx3 <- (a32 * x2 - r3 + 0.001 * runif(1)) * x3
        return(list(c(dx1, dx2, dx3)))
    })
}

get_jac <- function(pars, xeq) {
    jac <- rbind(
        c(
            -2 * pars["a11"] * xeq[1] - pars["a12"] * xeq[2] + pars["r1"],
            -pars["a12"] * xeq[1],
            0
        ),
        c(
            pars["a21"] * xeq[2],
            pars["a21"] * xeq[1] - 2 *pars["a22"] * xeq[2] - pars["a23"] * xeq[3] - pars["r2"],
            pars["a23"] * xeq[2] 
        ),
         c(
            0,
            pars["a32"] * xeq[3],
            pars["a32"] * xeq[2] - pars["r3"]
         )
    )
    Re(eigen(jac)$value[1])
}


pars <- c(
    a11 = 0.1, 
    a12 = 0.3, 
    a21 = 0.2, 
    a22 = 0.1, 
    a23 = 0.2,
    a32 = 0.1,
    r1 = 0.3, 
    r2 = 0.2, 
    r3 = 0.05)
out <- ode(
    c(x1 = 1, x2 = 1, x3 = 1),
    times,
    lv,
    pars
)
xeq <- apply(out[nrow(out) + seq(-10000, 0), ], 2, mean)[2:4]

out |>
    as_tibble() |>
    mutate_all(as.numeric) |>
    pivot_longer(-time, names_to = "variable", values_to = "value") |>
    ggplot() +
    aes(x = time, y = value, color = variable) +
    geom_line()
ggsave("fig/sys3sp.png")


out <- ode(
    c(x1 = 1, x2 = 1, x3 = 1),
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
ggsave("fig/sys3sp_noise.png")


# Equilibrium
# xeq <- apply(out[nrow(out) + seq(-10000, 0), ], 2, mean)[2:4]
xeq  <- c(1.5, 0.5, 0.25)
get_jac(pars, xeq)

get_mat_val <- function(xeq, pars) {
    matE <- rbind(
        c(-xeq[1], -xeq[2], 0, 0, 0, 0),
        c(0, 0, xeq[1], -xeq[2], -xeq[3], 0),
        c(0, 0, 0, 0, 0, xeq[2])
    )
    res <- xsample(
        E = matE,
        F = matrix(c(-pars["r1"], pars["r2"], pars["r3"])),
        G = rbind(
            c(0, 1, -1, 0, 0, 0), # a12 >= a21
            c(0, 0, 0, 0, 1, -1), # a23 >= a32out <- ode(
            c(1, 0, 0, 0, 0, 0), # a11 >= 0
            c(0, 1, 0, 0, 0, 0), # a12 >= 0
            c(0, 0, 1, 0, 0, 0), # a21 >= 0
            c(0, 0, 0, 1, 0, 0), # a22 >= 0
            c(0, 0, 0, 0, 1, 0), # a23 >= 0
            c(0, 0, 0, 0, 0, 1) # a32 >= 0
            #c(0, 0, 0, -1, 0, 0), # a22 <= 1
            #c(0, 0, 0, 0, -1, 0) # a23 <= 1
        ),
        #H = matrix(c(rep(0, 8), -1, -1), nrow = 10),
        H = matrix(rep(0, 8), nrow = 8),
        burninlength = 1e4,
        iter = 5000
    )
    mat_val <- res$X |> as.data.frame()
    mat_val$stab <- NA_real_
    for (i in seq_len(nrow(mat_val))) {
        mat_val$stab[i] <- get_jac(
            c(
                a11 = mat_val[i, 1],
                a12 = mat_val[i, 2],
                a21 = mat_val[i, 3],
                a22 = mat_val[i, 4],
                a23 = mat_val[i, 5],
                a32 = mat_val[i, 6],
                pars["r1"],
                pars["r2"],
                pars["r3"]
            ),
            xeq
        )
    }
    mat_val
}
mat_val <- get_mat_val(xeq, pars)

png(filename = "fig/box_3sp.png", height = 6, width = 18, res = 300, units = "in")
val_int <- pars[c("a11", "a12", "a21", "a22", "a23", "a32")]
par(mfrow = c(1, 4), mar = c(4, 4, 2, 0.5))

yl = c(0, 2)
boxplot(mat_val[mat_val$stab < 0, 1:6], ylim = yl, main = "all", names = names(val_int))
points(seq(val_int), val_int, pch = 19, cex = 2, col = "steelblue")

boxplot(mat_val[mat_val$stab <= quantile(mat_val$stab, 0.25), 1:6], ylim = yl, main = "50% top table", names = names(val_int))
points(seq(val_int), val_int,
    pch = 19, cex = 2,
    col = "steelblue"
)

boxplot(mat_val[mat_val$stab > quantile(mat_val$stab, 0.5), 1:6], ylim = yl, main = "50% less stable", names = names(val_int))
points(seq(val_int), val_int,
    pch = 19, cex = 2,
    col = "steelblue"
)

boxplot(mat_val[mat_val$stab <= quantile(mat_val$stab, 0.1), 1:6], ylim = yl, main = "10% top stable", names = names(val_int))
points(seq(val_int), val_int,
    pch = 19, cex = 2,
    col = "steelblue"
)

dev.off()

i = 500
i = 600
mat_val2  <- mat_val[mat_val$stab >= quantile(mat_val$stab, 0.1), 1:6]
i = 100
pars2 <- c(
    a11 = mat_val2[i, 1],
    a12 = mat_val2[i, 2],
    a21 = mat_val2[i, 3],
    a22 = mat_val2[i, 4],
    a23 = mat_val2[i, 5],
    a32 = mat_val2[i, 6],
    r1 = 0.3,
    r2 = 0.2,
    r3 = 0.05
)
out <- ode(
    c(x1 = 1, x2 = 1, x3 = 1),
    times,
    lv,
    pars2
)
out |>
    as_tibble() |>
    mutate_all(as.numeric) |>
    pivot_longer(-time, names_to = "variable", values_to = "value") |>
    ggplot() +
    aes(x = time, y = value, color = variable) +
    geom_line()

png(filename = "fig/box_3sp2_hist.png", height = 6, width = 6, res = 300, units = "in")
hist(mat_val$stab)
dev.off()

mat_val2 <- mat_val[mat_val$stab >= quantile(mat_val$stab, 0.9), 1:6]
i <- 100
pars2 <- c(
    a11 = mat_val2[i, 1],
    a12 = mat_val2[i, 2],
    a21 = mat_val2[i, 3],
    a22 = mat_val2[i, 4],
    a23 = mat_val2[i, 5],
    a32 = mat_val2[i, 6],
    r1 = 0.3,
    r2 = 0.2,
    r3 = 0.05
)
out <- ode(
    c(x1 = 1, x2 = 1, x3 = 1),
    times,
    lv,
    pars2
)
out |>
    as_tibble() |>
    mutate_all(as.numeric) |>
    pivot_longer(-time, names_to = "variable", values_to = "value") |>
    ggplot() +
    aes(x = time, y = value, color = variable) +
    geom_line()

boxplot(mat_val[mat_val[1, ] <= pars["a11"] + 0.01 & mat_val[1, ] >= pars["a11"] - 0.01, 1:6], ylim = yl)
points(seq(val_int), val_int,
    pch = 19, cex = 2,
    col = "steelblue"
)

boxplot(
    mat_val[mat_val[5, ] <= pars["a23"] + 0.05 & mat_val[5, ] >= pars["a23"] - 0.05, 1:6], 
    ylim = yl)
points(seq(val_int), val_int,
    pch = 19, cex = 2,
    col = "steelblue"
)

boxplot(
    mat_val[
        mat_val[5, ] <= pars["a23"] + 0.05 & mat_val[5, ] >= pars["a23"] - 0.05 &
        mat_val[4, ] <= pars["a22"] + 0.1 & mat_val[4, ] >= pars["a22"] - 0.1,
        1:6
    ],
    ylim = yl
)
points(seq(val_int), val_int,
    pch = 19, cex = 2,
    col = "steelblue"
)



mat_val[mat_val[1, ] <= pars["a11"] + 0.0001 & mat_val[1, ] >= pars["a11"] - 0.0001, 1:6]

mat_xeq <- out[50000 + sample(seq_len(50000))[1:100], 2:3]
kk <- apply(mat_xeq, 1, get_mat_val)
mat_val <- do.call(rbind, kk)
