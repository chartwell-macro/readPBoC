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

test_that("the snapshot generator preserves ordinary leading letters", {
  generator <- new.env(parent = globalenv())
  sys.source(
    test_path("..", "..", "inst", "examples", "refresh-pboc-current-series-data.R"),
    envir = generator
  )

  expect_equal(
    generator$current_series_bilingual("Foreign Assets")$series_en,
    "Foreign Assets"
  )
  expect_equal(
    generator$current_series_bilingual("A. Foreign Assets")$series_en,
    "Foreign Assets"
  )
})
