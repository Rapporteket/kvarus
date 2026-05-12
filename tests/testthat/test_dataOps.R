test_that("getFirstRegistrations filters the first registration per patient", {
    # Create a sample dataset
    sample_data <- data.frame(
        PasientGUID = c("A", "A", "B", "B", "C"),
        dato_mp_beh = as.POSIXct(c("2023-01-01", "2023-02-01", "2023-01-15", "2023-01-20", "2023-03-01")),
        value = c(10, 20, 30, 40, 50)
    )

    # Run the function
    result <- getFirstRegistrations(sample_data)

    # Test if the result matches the expected output
    expect_equal_to_reference(result, "data/getFirstRegistrations.rds")
})



test_that("prePros preprocesses the data correctly", {
    # Create a sample dataset
    sample_data <- data.frame(
        db_unit_title = c("Hospital A", "Hospital B", "Hospital A",
                          "Hospital B", "Hospital A", "Hospital B",
                          "Hospital A", "Hospital B"),
        PatientAge = c(25, 30, 25, 30, 25, 30, 25, 30),
        behandlingsstatus = c("1", "2", "3", "4", "5", "6", "7", "8"),
        plan_beh = c("1", "2", "0", "3", "qwerty", "4", "1", "2"),
        plan_krise = c("1", "2", "0", "3", "qwerty", "4", "1", "2"),
        PatientGender = c("1", "2", "1", "2", "1", "2", "0", "3")
    )

    # Run the function
    result <- prePros(sample_data)
    expect_equal_to_reference(result, "data/prePros.rds")

})
