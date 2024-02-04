test_that("fw_model() works", {
    A <- rbind(c(-1, -1), c(1, 0))
    R <- c(0.1, -0.05)
    B <- c(0.5, 0.25)
    res <- fw_model(A, B, R)

    expect_identical(names(res), c("A", "B", "R", "model"))
    expect_identical(res$A, A)

    expect_error(fw_model(A, B, R[1]), "NROW(A) == length(R) is not TRUE", fixed = TRUE)
    expect_error(fw_model(A, B[1], R), "length(B) == length(R) is not TRUE", fixed = TRUE)
    expect_error(fw_model(A, B[1], R), "length(B) == length(R) is not TRUE", fixed = TRUE)
})


test_that("fw_model() works", {
    expect_identical(fw_get_model(), mod_lv_fr1)
})
