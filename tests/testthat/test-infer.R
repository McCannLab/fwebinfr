A <- rbind(c(-1, -1), c(1, 0))
R <- c(0.1, -0.05)
B <- c(0.5, 0.25)
U <- create_U_from_A(A)


test_that("get_E_F works", {
    res <- get_E_F(A, B, R, U)
    expect_equal(res$E, rbind(c(-0.5, 0, -0.25), c(0, 0.5, 0)))
    expect_equal(res$F, -R)
})


test_that("get_G_H works", {
    res <- get_G_H(A, U, eff_max = 0.5)
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
        res3 <- fw_problem(A, B, R, U) |> fw_infer()
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
    expect_equal(fw_predict_B(res2, 2), B)
    expect_equal(fw_predict_B(res3, 2), B)
    res3 <- fw_predict_A(res1, 1)
    expect_identical((res3 > 0) * 1.0 - (res3 < 0) * 1.0, A)
})


test_that("fw_infer() works with known interactions", {
    #
    A2 <- A
    A2[1, 1] <- -0.1
    U2 <- U
    U2$unknown[1] <- FALSE
    suppressWarnings({
        res1 <- fw_problem(A2, B, R, U2) |> fw_infer()
    })
    expect_identical(nrow(res1$prediction), 1L)
    expect_equal(fw_predict_B(res1, 1), B)
    expect_error(fw_predict_B(res1, 2), "NA/NaN/Inf in foreign function call", 
        fixed = TRUE)
})

test_that("get_known_interactions() works", {
    expect_identical(get_unknown_interactions(A, U), A)
    expect_identical(get_known_interactions(A, U), A * 0)
    U2 <- U
    A2 <- A
    A3 <- A * 0
    U2$unknown[3] <- FALSE
    A3[1, 2] <- A2[1, 2]
    A2[1, 2] <- 0
    expect_identical(get_unknown_interactions(A, U2), A2)
    expect_identical(get_known_interactions(A, U2), A3)
    U2$unknown[2] <- FALSE
    A3[2, 1] <- A2[2, 1]
    A2[2, 1] <- 0
    expect_identical(get_unknown_interactions(A, U2), A2)
    expect_identical(get_known_interactions(A, U2), A3)
})
