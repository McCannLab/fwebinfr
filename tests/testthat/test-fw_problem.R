A <- rbind(c(-1, -1), c(1, 0))
R <- c(0.1, -0.05)
B <- c(0.5, 0.25)
U <- data.frame(
    row = c(1L, 2L, 1L),
    col = c(1L, 1L, 2L),
    unknown = TRUE,
    value = c(-1, 1, -1),
    name = c("a_1_1", "a_2_1", "a_1_2")
)


test_that("create_U_from_A() works", {
    expect_identical(create_U_from_A(A), U)
})

test_that("create_A_from_U() works", {
    expect_identical(create_A_from_U(U), A)
    U2 <- U
    U$value[1] <- 10
    expect_identical(create_A_from_U(U2), A)
})

test_that("create_U_from_A() works", {
    expect_identical(create_U_with_A(U, A), validate_U(U))
    expect_identical(create_U_with_A(U[1:2, ], A), validate_U(U))
})


test_that("validate_U() works", {
    expect_error(
        validate_U("a"),
        'inherits(U, "data.frame") is not TRUE',
        fixed = TRUE
    )
    expect_error(
        validate_U(data.frame()),
        "nrow(U) > 0 is not TRUE",
        fixed = TRUE
    )
    U2 <- U
    U2$wrong <- ""
    expect_error(
        validate_U(U2),
        'identical(sort(colnames(U)), sort(c("name", "row',
        fixed = TRUE
    )
    U2 <- U
    U2$value[1] <- "a"
    expect_error(
        validate_U(U2),
        'inherits(U$value, "numeric") is not TRUE',
        fixed = TRUE
    )
    U2 <- U
    U2$value[1] <- NA
    expect_error(
        validate_U(U2),
        "all(!is.na(U$value)) is not TRUE",
        fixed = TRUE
    )
    U2 <- U
    U2$unknown[1] <- 2
    expect_error(
        validate_U(U2),
        'inherits(U$unknown, "logical") is not TRUE',
        fixed = TRUE
    )
})

test_that("validate_A() works", {
    expect_error(
        validate_A(matrix(0, 3, 2)),
        "ncol(A) == nrow(A) is not TRUE",
        fixed = TRUE
    )
    expect_error(
        validate_A(matrix("a", 2, 2)),
        'inherits(as.vector(A), "numeric") is not TRUE',
        fixed = TRUE
    )
})


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
    #
    U2 <- U
    U2$value[1] <- NA
    expect_error(
        fw_problem(A, B[1], R, U2),
        "all(!is.na(U$value)) is not TRUE",
        fixed = TRUE
    )
    #
    U2 <- U
    U2$row <- 10
    expect_error(
        fw_problem(A, B, R, U2),
        "all(x$U$row %in% seq_len(nrow(x$A))) is not TRUE",
        fixed = TRUE
    )
})
