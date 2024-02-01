A <- rbind(c(-1, -1), c(1, 0))
R <- c(0.1, -0.05)
B <- c(0.5, 0.25)

test_that("fw_create() works", {
    res <- fw_create(A, B, R)
    expect_identical(names(res), c("A", "B", "R", "S", "U", "sdB", "model"))
    expect_true(is.null(res$sdB))
    expect_identical(res$A, A)
    expect_identical(res$B, B)
    expect_identical(res$U, get_U(A))
    expect_identical(res$S, get_S(A))
    expect_true(inherits(res, "fwmod"))

    expect_error(fw_create(A, B, R[1]), "NROW(A) == length(R) is not TRUE", fixed = TRUE)
    expect_error(fw_create(A, B[1], R), "length(B) == length(R) is not TRUE", fixed = TRUE)
    expect_error(fw_create(A, B[1], R), "length(B) == length(R) is not TRUE", fixed = TRUE)
    expect_error(
        fw_create(A, B, R, matrix(c(0, 1, 0, 2), 2, 2)), 
        "all(S %in% c(-1, 0, 1)) is not TRUE", 
        fixed = TRUE
    )
})


test_that("check_U() works", {
    expect_identical(check_U(get_U(A), A), get_U(A))
    expect_error(
        check_U("a", A),
        'inherits(U, "matrix") is not TRUE',
        fixed = TRUE
    )
    expect_error(
        check_U(matrix(0, 3, 3), A),
        "ncol(U) == 2 is not TRUE",
        fixed = TRUE
    )
    U <- rbind(c(1, 2), c(1, 3))
    expect_error(
        check_U(U, A),
        "all(U[, 2L] %in% seq_len(ncol(A))) is not TRUE",
        fixed = TRUE
    )
})

test_that("check_S() works", {
    expect_identical(check_S(get_S(A), A), get_S(A))
    expect_error(
        check_S("a", A),
        'inherits(S, "matrix") is not TRUE',
        fixed = TRUE
    )
    expect_error(
        check_S(matrix(0, 3, 3), A),
        "identical(dim(S), dim(A)) is not TRUE",
        fixed = TRUE
    )
    expect_error(
        check_S(matrix(c(0, 1, 0, 2), 2, 2), A),
        "all(S %in% c(-1, 0, 1)) is not TRUE",
        fixed = TRUE
    )
})
