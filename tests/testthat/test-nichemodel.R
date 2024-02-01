test_that("webFromNicheModel() works", {
    expect_identical(fw_niche_model(10, 0.2) |> dim(), c(10L, 10L))
    expect_true(
        fw_niche_model(10, 0.2, TRUE, TRUE) |>
            apply(1, sum) |>
            all()
    )
})