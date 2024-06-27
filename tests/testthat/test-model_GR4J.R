test_that("model_GR4J works", {
  library(airGR)
  library(magrittr)
  library(data.table)
  library(dplyr)

  data(L0123001)
  df <- BasinObs %>%
    data.table() %>%
    rename(date = DatesR) %>%
    mutate(date = as.Date(date)) %>%
    .[, .(date, P, E, Robs = Qmm)]

  r <- create_GR4J(df)
  print(r)

  expect_true(r$gof$NSE[1] >= 0.7) # calib
  expect_true(r$gof$NSE[2] >= 0.7) # valid
})
