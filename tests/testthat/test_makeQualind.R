test_that("kval_count works as expected", {
    # Create a mock dataset
    mock_data <- data.frame(
        Sykehus = c("A", "A", "B", "B", "C"),
        behandlingsstatus = c("aktiv", "avsluttet med gjensidig avtale", "aktiv", "avbrutt", "aktiv"),
        behandlingsplan = c("ja", "nei", "ja", "nei", "ja"),
        kriseplan = c("ja", "nei", "nei", "ja", "ja"),
        eval_utbytte = c(4, 5, 3, 4, 5),
        PasientGUID = c("p1", "p2", "p3", "p4", "p5"),
        dato_mp_beh = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05"))
    )

    # Test for "behandlingsplan"
    result <- kval_count(mock_data, "behandlingsplan")
    expect_equal(nrow(result), length(unique(mock_data$Sykehus)))
    expect_equal(result$andel_per_syk, c(50, 50, 100))
    expect_equal(result$Sykehus, unique(result$Sykehus))

    # Test for "kriseplan"
    result <- kval_count(mock_data, "kriseplan")
    expect_equal(result$andel_per_syk, c(50, 0, 100))
    expect_equal(result$Sykehus, unique(result$Sykehus))
    expect_equal(nrow(result), length(unique(mock_data$Sykehus)))

    # Test for "utbytte"
    result <- kval_count(mock_data, "utbytte")
    expect_equal(result$andel_per_syk, c(50, 0, 100))
    expect_equal(nrow(result), length(unique(mock_data$Sykehus)))

    # Test for "gjensidig"
    result <- kval_count(mock_data, "gjensidig")
    expect_equal(result$andel_per_syk, c(50, 0, 0))
    expect_equal(nrow(result), length(unique(mock_data$Sykehus)))

    # Test for random navn
    result <- kval_count(mock_data, "qwerty")
    expect_equal(result$antall_kval_syk, c(0, 0, 0))
    expect_equal(result$andel_per_syk, c(0, 0, 0))
    expect_equal(nrow(result), length(unique(mock_data$Sykehus)))
})

test_that("annotations function works as expected", {
    # Test for "behandlingsplan"
    result <- annotations("behandlingsplan")
    expect_equal(result$xmin, 80)
    expect_equal(result$xmax, 100)
    expect_equal(result$xmin_moderate, 60)
    expect_equal(result$xmax_moderate, 80)

    # Test for "kriseplan"
    result <- annotations("kriseplan")
    expect_equal(result$xmin, 60)
    expect_equal(result$xmax, 100)
    expect_equal(result$xmin_moderate, 40)
    expect_equal(result$xmax_moderate, 60)

    # Test for "utbytte"
    result <- annotations("utbytte")
    expect_equal(result$xmin, 80)
    expect_equal(result$xmax, 100)
    expect_equal(result$xmin_moderate, 60)
    expect_equal(result$xmax_moderate, 80)

    # Test for "gjensidig"
    result <- annotations("gjensidig")
    expect_equal(result$xmin, 80)
    expect_equal(result$xmax, 100)
    expect_equal(result$xmin_moderate, 60)
    expect_equal(result$xmax_moderate, 80)

    # Test for random navn
    result <- annotations("qwerty")
    expect_equal(result$xmax, 100)
    expect_true(is.na(result$xmin))
    expect_true(is.na(result$xmin_moderate))
    expect_true(is.na(result$xmax_moderate))
})



test_that("explanation_kvalind works as expected", {
    # Mock configuration
    mock_config <- list(
        kvalind = list(
            behandlingsplan = list(forklaring = "Explanation for behandlingsplan"),
            kriseplan = list(forklaring = "Explanation for kriseplan"),
            utbytte = list(forklaring = "Explanation for utbytte"),
            gjensidig = list(forklaring = "Explanation for gjensidig"),
            default = list(forklaring = "Default explanation")
        )
    )

    local_mocked_bindings(
      get_config = function(...) mock_config
    )

    # Test for "behandlingsplan"
    result <- explanation_kvalind("behandlingsplan")
    expect_equal(result$header, "Behandlingsplan på plass tidlig i forløpet")
    expect_equal(result$text, "Explanation for behandlingsplan")

    # Test for "kriseplan"
    result <- explanation_kvalind("kriseplan")
    expect_equal(result$header, "Kriseplan på plass tidlig i forløpet")
    expect_equal(result$text, "Explanation for kriseplan")

    # Test for "utbytte"
    result <- explanation_kvalind("utbytte")
    expect_equal(result$header, "Stort utbytte av behandlingen")
    expect_equal(result$text, "Explanation for utbytte")

    # Test for "gjensidig"
    result <- explanation_kvalind("gjensidig")
    expect_equal(result$header, "Gjensidig avslutning av behandlingen")
    expect_equal(result$text, "Explanation for gjensidig")

    # Test for random navn
    result <- explanation_kvalind("qwerty")
    expect_true(is.na(result$header))
    expect_equal(result$text, "Default explanation")
})
