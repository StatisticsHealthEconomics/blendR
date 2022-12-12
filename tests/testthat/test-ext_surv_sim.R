

test_that("100 samples of 1 same as 1 sample of 100", {

  dat <- ext_surv_sim(t_info = c(10,20,50),
                      S_info = c(0.9, 0.8, 0.2),
                      T_max = 100, n = 50000)$time

  res1 <-
    cut(dat, breaks = c(0,10,20,50,101)) |>
    table() |>
    prop.table() |>
    cumsum() |>
    round(2)

  expect_equal(unname(1 - res1), c(0.9, 0.8, 0.2, 0))

  #

  dat <- NULL

  for (i in 1:50000) {

    dat[i] <- ext_surv_sim(t_info = c(10,20,50),
                           S_info = c(0.9, 0.8, 0.2),
                           T_max = 100, n = 1)$time
  }

  res2 <-
    cut(dat, breaks = c(0,10,20,50,101)) |>
    table() |>
    prop.table() |>
    cumsum() |>
    round(2)

  expect_equal(unname(1 - res2), c(0.9, 0.8, 0.2, 0))
})
