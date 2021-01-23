
# Test mixingmatrix.egodata() ---------------------------------------------

test_that("mixing matrices for FMH and egoFMH are equivalent", {
  data("faux.mesa.high")
  fmh.ego <- as.egodata(faux.mesa.high)
  expect_equivalent(
    mm.ego <- mixingmatrix(fmh.ego, "Grade"),
    {
      mm <- mixingmatrix(faux.mesa.high, "Grade")
      diag(mm) <- diag(mm) * 2
      mm
    }
  )
  expect_identical(is.directed(mm.ego), is.directed(mm))
  expect_identical(is.bipartite(mm.ego), is.bipartite(mm))
})



test_that("error if attribute not present", {
  expect_error(
    mixingmatrix(fmh.ego, "no-such-attribute")
  )
})


test_that("mixing matrices for attribute with NAs", {
  data("emon")
  texas.ego <- as.egodata(emon$Texas)
  expect_equivalent(
    mm.ego <- mixingmatrix(texas.ego, "Formalization"),
    {
      mm <- mixingmatrix(emon$Texas, "Formalization")
      diag(mm) <- diag(mm) * 2
      mm
    }
  )
  expect_identical(is.directed(mm.ego), is.directed(mm))
  expect_identical(is.bipartite(mm.ego), is.bipartite(mm))
})
