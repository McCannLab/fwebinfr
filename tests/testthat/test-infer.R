A <- rbind(c(-1, -1), c(1, 0))
R <- c(0.1, -0.05)
B <- c(0.5, 0.25)

test_that("get_E_F works", {
    res <- get_E_F(A, B, R)
    expect_equal(res$E, rbind(c(-0.5, 0, -0.25), c(0, 0.5, 0)))
    expect_equal(res$F, -R)
})


test_that("get_U_from_A() works", {
    res <- get_U_from_A(A)
    expect_identical(
        res, 
        structure(
            c(1L, 2L, 1L, 1L, 1L, 2L), 
            dim = 3:2, 
            dimnames = list(NULL, c("row", "col"))
        )
    )
    AA <- A
    AA[1, 2] <- 0
    res <- get_U_from_A(AA)
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


test_that("fw_infer() works", {
    expect_error(
        fw_infer("wrong"), 
        'inherits(x, "fw_problem") is not TRUE',
        fixed = TRUE
    )
    suppressWarnings({
        res1 <- fw_problem(A, B, R) |> fw_infer()
        prb <- fw_problem(A, B, R)
        res2 <- fw_infer(prb, iter = 1000)
    })
    expect_true(inherits(res1, "fw_predicted"))
    expect_true(inherits(unclass(res1), "list"))
    expect_identical(names(res1), c("prediction", "problem"))
    expect_identical(res1$problem, prb)
    expect_identical(dim(res1$prediction), c(3000L, 4L))
    expect_identical(dim(res2$prediction), c(1000L, 4L))
    expect_true(all(res1$prediction$leading_ev < 0))
    expect_error(
        fw_predict_B(
            "wrong",
            'inherits(y, "fw_predicted") is not TRUE',
            fixed = TRUE
        )
    )
    expect_equal(fw_predict_B(res1, 1), B)
    expect_equal(fw_predict_B(res1, 100), B)
    res3 <- fw_predict_A(res1, 1)
    expect_identical((res3 > 0) * 1.0 - (res3 < 0) * 1.0, A)
})