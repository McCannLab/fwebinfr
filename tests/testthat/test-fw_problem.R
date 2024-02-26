A <- rbind(c(-1, -1), c(1, 0))
R <- c(0.1, -0.05)
B <- c(0.5, 0.25)

test_that("fw_problem() works", {
    res <- fw_problem(A, B, R)
    expect_identical(names(res), c("A", "B", "R", "U", "sdB", "model"))
    expect_true(is.null(res$sdB))
    expect_identical(res$A, A)
    expect_identical(res$B, B)
    expect_true(inherits(res, "fw_problem"))

    expect_error(fw_problem(A, B, R[1]), "NROW(A) == length(R) is not TRUE", fixed = TRUE)
    expect_error(fw_problem(A, B[1], R), "length(B) == length(R) is not TRUE", fixed = TRUE)
    expect_error(fw_problem(A, B[1], R), "length(B) == length(R) is not TRUE", fixed = TRUE)
})


test_that("create_U() works", {
    tmp <- get_U_from_A(A) |> as.data.frame()
    tmp$unknown <- TRUE
    expect_identical(create_U(tmp, A), create_U(NULL, A))
    expect_error(
        create_U("a", A),
        'inherits(U, "data.frame") is not TRUE',
        fixed = TRUE
    )
    U <- data.frame(
        row = c(1, 1),
        col = c(1, 2),
        u = rep(TRUE, 2)
    )
    expect_error(
        create_U(U, A),
        'identical(colnames(U), c("row", "col", "unknown")) is not TRUE',
        fixed = TRUE
    )
    U <- data.frame(
        row = c(0, 1),
        col = c(1, 2),
        unknown = rep(TRUE, 2)
    )
    expect_error(
        fw_problem(A, B, R, U),
        "all(U$row %in% seq_len(nrow(A))) is not TRUE",
        fixed = TRUE
    )
})
