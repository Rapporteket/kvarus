test_that("The first registrations are correctly filtered", {

  set.seed(1)

  date1 <- "2020-01-01 UTC"
  date2 <- "2020-02-02 UTC"
  date3 <- "2020-03-03 UTC"

  testData <- data.frame(
    PasientGUID = rep(c("id1", "id2", "id3"), 3),
    dato_mp_beh = c(rep(date1, 3), rep(date2, 3), rep(date3, 3))
  )

  testData <- testData[sample(1:9), ]

  filteredData <- getFirstRegistrations(testData) %>% dplyr::arrange(PasientGUID) %>% as.data.frame()

  expectedData <- data.frame(PasientGUID = c("id1", "id2", "id3"), dato_mp_beh = rep(date1, 3))

  expect_equal(filteredData, expectedData)
})