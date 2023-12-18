A <- matrix(0, 2, 2)
A[1, 1] <- -1
A[1, 2] <- -1
A[2, 1] <- 1
R <- c(0.1, -0.05)
B <- c(0.5, 0.25)

test_that("get_E_F works", {
    res <- get_E_F(A, B, R)
    expect_equal(res$E, rbind(c(-0.5, 0, -0.25), c(0, 0.5, 0)))
    expect_equal(res$F, -R)
})


test_that("get_U works", {
    res <- get_U(A)
    expect_identical(
        res, 
        structure(
            c(1L, 2L, 1L, 1L, 1L, 2L), 
            dim = 3:2, 
            dimnames = list(NULL, c("row", "col"))
        )
    )
    AA  <- A
    AA[1, 2] <- 0
    res <- get_U(AA)
    expect_identical(
        res,
        structure(
            c(1L, 2L, 1L, 1L),
            dim = c(2L, 2L),
            dimnames = list(NULL, c("row", "col"))
        )
    )
})

test_that("get_G_H works", {
    res <- get_G_H(A, eff_max = 0.5)
    expect_equal(res$G, rbind(c(0, -1, 0.5), diag(1, 3)))
    expect_equal(res$H, matrix(0, nrow = 4))
})

test_that("fw_infer() format erros", {
    expect_error(fw_infer("wrong", R, B, 1))
    expect_error(fw_infer(A, R, c(B, 1), 1))
    expect_error(fw_infer(diag(0, 2), R, B, 1))
    expect_error(fw_infer(diag(3), R, B, 1))
})
