

test_that("100 samples of 1 same as 1 sample of 100", {

  event_times_to_S <- function(dat, breaks) {
    cut(dat, breaks = c(0, breaks, 101)) |>
      table() |>
      prop.table() |>
      cumsum() |>
      round(1)
  }

  npop <- 50000

  dat <- ext_surv_sim(t_info = c(10,20,50),
                      S_info = c(0.9, 0.8, 0.2),
                      T_max = 100, n = npop)$time

  res1 <-
    event_times_to_S(dat, breaks = c(10,20,50))

  expect_equal(unname(1 - res1), c(0.9, 0.8, 0.2, 0))

  #

  dat <- NULL

  for (i in 1:npop) {
    dat[i] <- ext_surv_sim(t_info = c(10,20,50),
                           S_info = c(0.9, 0.8, 0.2),
                           T_max = 100, n = 1)$time
  }

  res2 <-
    event_times_to_S(dat, breaks = c(10,20,50))

  expect_equal(unname(1 - res2), c(0.9, 0.8, 0.2, 0))
})


