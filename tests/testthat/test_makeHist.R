test_that("makeHist returns a histogram object", {
    df <- mtcars
    var <- "mpg"
    bins <- 5
    result <- makeHist(df = df, var = var, bins = bins, makeTable = FALSE)

    expect_true(inherits(result, "histogram"))
    expect_equal(result$breaks, c(10.4, 15.1, 19.8, 24.5, 29.2, 33.9))
})

test_that("makeHist returns a data frame when makeTable is TRUE", {
    df <- mtcars
    var <- "mpg"
    bins <- 5
    result <- makeHist(df = df, var = var, bins = bins, makeTable = TRUE)

    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 3)
    expect_named(result, c("GruppeMin", "GruppeMax", "Antall"))
})

test_that("makeHist handles NA values correctly", {
    df <- mtcars
    df$mpg[1:3] <- NA
    var <- "mpg"
    bins <- 5
    result <- makeHist(df = df, var = var, bins = bins, makeTable = TRUE)

    expect_true(all(!is.na(result$Antall)))
})

test_that("makeHist produces correct binning", {
    df <- mtcars
    var <- "mpg"
    bins <- 4
    result <- makeHist(df = df, var = var, bins = bins, makeTable = TRUE)

    expect_equal(nrow(result), bins)
    expect_true(all(result$GruppeMin < result$GruppeMax))
})
