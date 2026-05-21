test_that("makeGGdata returns correct data for 'kval' type_plot", {
    # Mock configuration
    mock_config <- list(
        kvalind = list(
            behandlingsplan = list(tittel = "Behandlingsplan Title"),
            kriseplan = list(tittel = "Kriseplan Title"),
            utbytte = list(tittel = "Utbytte Title"),
            gjensidig = list(tittel = "Gjensidig Title"),
            default = list(tittel = "Default Title")
        )
    )
    
    local_mocked_bindings(
      get_config = function(...) mock_config
    )
    
    # Test cases
    result <- makeGGdata("behandlingsplan", "kval")
    expect_equal(result$title, "Behandlingsplan Title")
    expect_equal(result$xlab, "Andel pasienter")
    
    result <- makeGGdata("kriseplan", "kval")
    expect_equal(result$title, "Kriseplan Title")
    
    result <- makeGGdata("utbytte", "kval")
    expect_equal(result$title, "Utbytte Title")
    
    result <- makeGGdata("gjensidig", "kval")
    expect_equal(result$title, "Gjensidig Title")
    
    result <- makeGGdata("unknown", "kval")
    expect_equal(result$title, "Default Title")

    result <- makeGGdata("unknown", "unknow")
    expect_equal(result, data.frame(title = "", xlab = ""))

})
