test_that("makeHist returns a histogram object", {
    df <- mtcars
    var <- "mpg"
    result <- makeHist(df = df, var = var)
    layer_data <- ggplot2::get_layer_data(result)

    expect_true(inherits(result, "ggplot"))
    # antallet er enten 1 eller 2
    expect_equal(unique(layer_data$y), c(2, 1))
    expect_equal(nrow(layer_data), 25)
    labs <- ggplot2::get_labs(result)
    expect_equal(labs$x, "mpg")
    expect_equal(labs$y, "Antall")
    expect_equal(labs$title, "Fordeling av mpg")
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
