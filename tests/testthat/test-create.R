A <- matrix(0, 2, 2)
A[1, 1] <- -1
A[1, 2] <- -1
A[2, 1] <- 1
R <- c(0.1, -0.05)
B <- c(0.5, 0.25)

test_that("fw_create() works", {
    res <- fw_create(A, B, R)
    expect_identical(names(res), c("A", "B", "R", "U", "sdB", "model"))
    expect_true(is.null(res$sdB))
    expect_identical(res$A, A)
    expect_identical(res$B, B)
    expect_identical(res$U, get_U(A))
    expect_true(inherits(res, "fwmod"))
})

