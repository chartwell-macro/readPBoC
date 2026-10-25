test_that("catalog comparisons identify all revision states", {
  previous <- data.frame(
    asset_id = c("same", "changed", "removed"),
    revision_id = c("r1", "r1", "r1"),
    url = paste0("https://example.test/", c("same", "old", "removed"), ".xlsx")
  )
  current <- data.frame(
    asset_id = c("same", "changed", "new"),
    revision_id = c("r1", "r2", "r1"),
    url = paste0("https://example.test/", c("same", "newer", "new"), ".xlsx")
  )

  result <- pboc_revisions(previous, current)
  status <- setNames(result$status, result$asset_id)

  expect_equal(status[["same"]], "unchanged")
  expect_equal(status[["changed"]], "revised")
  expect_equal(status[["new"]], "new")
  expect_equal(status[["removed"]], "removed")
})

test_that("a newly listed revision is detected even if the old URL remains", {
  previous <- data.frame(
    asset_id = "money",
    revision_id = "r1",
    url = "https://example.test/r1.xlsx",
    published_at = "2025-01-01"
  )
  current <- rbind(
    previous,
    transform(
      previous,
      revision_id = "r2",
      url = "https://example.test/r2.xlsx",
      published_at = "2025-02-01"
    )
  )

  result <- pboc_revisions(previous, current)
  expect_equal(result$status, "revised")
  expect_equal(result$current_revision, "r2")
})

test_that("snapshots round-trip as CSV", {
  catalog <- data.frame(
    asset_id = "a",
    revision_id = "r1",
    url = "https://example.test/a.xlsx"
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  expect_invisible(pboc_snapshot(catalog, path))
  expect_true(file.exists(path))
  expect_equal(
    pboc_revisions(path, catalog)$status,
    "unchanged"
  )
})
