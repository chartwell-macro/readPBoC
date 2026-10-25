test_that("current-values dashboard snapshot covers every current release", {
  data_dir <- system.file("examples", "data", package = "readPBOC")
  catalog <- readRDS(file.path(
    data_dir,
    "pboc-current-series-catalog.rds"
  ))
  values <- readRDS(file.path(
    data_dir,
    "pboc-current-series-values.rds"
  ))
  audit <- readRDS(file.path(
    data_dir,
    "pboc-current-series-audit.rds"
  ))

  expect_gt(nrow(catalog), 0L)
  expect_setequal(audit$revision_id, catalog$revision_id)
  expect_setequal(unique(values$revision_id), catalog$revision_id)
  expect_true(all(audit$status == "retrieved"))
  expect_true(all(audit$observations > 0L))
  expect_true(all(c(
    "series",
    "period",
    "value",
    "source_url"
  ) %in% names(values)))
})

test_that("current-series label parsing preserves ordinary initial capitals", {
  helpers <- new.env(parent = globalenv())
  sys.source(
    system.file(
      "examples",
      "refresh-pboc-current-series-data.R",
      package = "readPBOC"
    ),
    envir = helpers
  )

  expect_equal(
    helpers$current_series_bilingual("Overnight")$series_en,
    "Overnight"
  )
  expect_equal(
    helpers$current_series_bilingual("Weighted Average Interest Rate")$series_en,
    "Weighted Average Interest Rate"
  )
  expect_equal(
    helpers$current_series_bilingual("A. Household deposits")$series_en,
    "Household deposits"
  )
  expect_equal(
    helpers$current_series_bilingual("B) Corporate deposits")$series_en,
    "Corporate deposits"
  )
})
