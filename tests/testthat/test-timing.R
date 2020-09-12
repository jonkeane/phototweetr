test_that("timing works", {
  now <- as.POSIXct("2019-03-10 09:00:00")

  # if then is in the future as improbable as it is, false
  expect_message(
    expect_false(wait_and_window(as.POSIXct("2019-04-01 10:00:00"), now)),
    "Waiting until at least: 2019-04-08 10:00:00"
  )
  # if then is less than wait, false
  expect_message(
    expect_false(wait_and_window(as.POSIXct("2019-03-09 10:00:00"), now)),
    "Waiting until at least: 2019-03-16 11:00:00"
  )
  # if then is more than wait, true
  expect_true(wait_and_window(as.POSIXct("2019-03-02 10:00:00"), now))
  # if then is more than wait, but the window is now yet met, false
  expect_message(
    expect_false(wait_and_window(as.POSIXct("2019-03-02 07:00:00"), now)),
    "It is not during the window from 8 to 22"
  )

  # we can set options
  expect_message(
    expect_false(wait_and_window(as.POSIXct("2019-03-02 10:00:00"), now, window = c(12, 13))),
    "It is not during the window from 12 to 13"
  )
  expect_message(expect_false(wait_and_window(as.POSIXct("2019-03-02 14:00:00"), now, window = c(12, 13))))

  expect_true(wait_and_window(as.POSIXct("2019-03-10 08:00:00"), now, wait = 10))

  expect_true(wait_and_window(as.POSIXct("2019-03-02 01:00:00"), now, window = NULL))
})

test_that("can make an unfair coin", {
  thousand_flips <- replicate(1000, weighted_coin())
  # the default odds are 1:14, we would be very unlucky if the mean was > 1:10
  expect_lt(mean(thousand_flips), 1/10)

  # with a fair coin, we expect closer to 1/2 (and would be very unlucky if we
  # got as low as 1/10)
  thousand_flips <- replicate(1000, weighted_coin(1, 1))
  expect_gt(mean(thousand_flips), 1/10)
})
