test_that("snvec() inputs are checked", {
  # we test the astronomical_solution inputs for the get_solution helper in stead
  expect_error(snvec(tend = -Inf, quiet = TRUE))
  expect_error(snvec(tres = -Inf, quiet = TRUE))
  expect_error(snvec(tend = -1000, tres = 0.4, quiet = TRUE))
  expect_error(snvec(tend = 1000, tres = -0.4, quiet = TRUE))
  expect_error(snvec(tend = 1000, tres = 0.4, astronomical_solution = "full-ZB18a", quiet = TRUE))
  expect_error(snvec(tend = -1000, tres = -0.4, quiet = TRUE,
                     astronomical_solution = dplyr::mutate(get_solution("full-ZB18a", quiet = TRUE),
                                                           t = -t, time = -time)
                       ))
  expect_error(snvec(output = "banaan", quiet = TRUE))
  expect_error(snvec(solver = "banaan", quiet = TRUE))
  expect_error(snvec(astronomical_solution = "hoi", quiet = TRUE)) # same as get_solution
  expect_error(snvec(os_ref_frame = "hoi", quiet = TRUE))
  expect_error(snvec(os_ref_frame = "J2000", os_omt = 5, quiet = TRUE))
  expect_error(snvec(os_ref_frame = "J2000", os_inct = 7, quiet = TRUE))

  # these are a bit annoying
  expect_warning(snvec(ed = 1.1001, tend = -1, tres = -.5, quiet = TRUE))
  expect_warning(snvec(ed = 0.8999, tend = -1, tres = -.5, quiet = TRUE))
  expect_warning(snvec(td = 1.2001, tend = -1, tres = -.5, quiet = TRUE))
  expect_warning(snvec(td = -0.0001, tend = -1, tres = -.5, quiet = TRUE))
  expect_warning(snvec(atol = 0.9e-12, tend = -1, tres = -.5, quiet = TRUE))
  expect_warning(snvec(atol = 1.1e-3, tend = -1, tres = -.5, quiet = TRUE))
  expect_warning(snvec(rtol = 1.1e-3, tend = -1, tres = -.5, quiet = TRUE))
})

test_that("snvec() works", {
  pth <- withr::local_tempdir(pattern = "snvecR")
  withr::local_options(list(snvecR.cachedir = pth, width = 57))

  # I test a snapshot of the output
  expect_snapshot(
    # print the full 100 rows to monitor changes
    print(
      dplyr::select(
        snvec(tend = -49, # limit output to 50 ka so it takes <5 s (CRAN check)
              # specify default values explicitly in case they change in the future
              ed = 1,
              td = 0,
              # return results at a very low resolution for speed
              tres = -1,
              # do not show info messages because some have timestamps
              quiet = TRUE,
              # provide output = all so that we can see if the vectors are doing well
              output = "all"),
        # report only the columns of interest (the ones returned by the C-routine)
        # this means that if I change my mind about which columns to report it doesn't matter,
        # as long as the output of these columns remains the same.
        tidyselect::all_of(c("time", "sx", "sy", "sz", "epl", "phi", "cp"))),
      n = 50))
})

test_that("snvec() output columns are correct", {
  pth <- withr::local_tempdir(pattern = "snvecR")
  withr::local_options(list(snvecR.cachedir = pth))

  # we have the desired columns
  expect_equal(colnames(
    snvec(tend = -1, tres = -0.5, quiet = TRUE, output = "nice")),
    c("time", "epl", "phi", "cp"))
  # we have a deSolve matrix
  expect_equal(class(snvec(tend = -1, tres = -0.5, quiet = TRUE, output = "ode")),
               c("deSolve", "matrix"))
})
