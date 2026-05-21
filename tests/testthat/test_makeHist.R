test_that("makeHist returns a histogram object", {
    df <- mtcars
    var <- "mpg"
    result <- makeHist(df = df, var = var)

    expect_true(inherits(result, "ggplot"))
    expect_equal(unique(ggplot2::get_layer_data(result)$y), c(2, 1))
})


test_that("makeHist handles NA values correctly", {
    df <- mtcars
    df$mpg[1:3] <- NA
    var <- "mpg"
    result <- makeHist(df = df, var = var)
    expect_true(inherits(result, "ggplot"))

    expect_warning(
      expect_equal(
        unique(ggplot2::get_layer_data(result)$y),
        c(2, 1)
      )
    )
})
