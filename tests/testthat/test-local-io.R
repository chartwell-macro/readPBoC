test_that("snapshot and revision helpers preserve schemas and isolate side effects", {
  work <- tempfile("readPBOC-snapshot-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  snapshot_path <- file.path(work, "nested", "catalog.csv")
  catalog <- data.frame(
    asset_id = c("same", "revised", "removed"),
    revision_id = c("r1", "r1", "r1"),
    url = paste0("https://fixtures.test/", c("same", "old", "removed"), ".xlsx"),
    published_at = as.Date(c("2026-01-01", "2026-01-01", "2026-01-01")),
    stringsAsFactors = FALSE
  )
  expect_invisible(pboc_snapshot(catalog, snapshot_path))
  expect_true(file.exists(snapshot_path))
  expect_equal(
    names(utils::read.csv(snapshot_path, stringsAsFactors = FALSE)),
    names(catalog)
  )
  expect_error(
    pboc_snapshot(list(asset_id = "not-a-data-frame"), snapshot_path),
    "must be a catalog data frame"
  )

  current <- data.frame(
    asset_id = c("same", "revised", "new"),
    revision_id = c("r1", "r2", "r1"),
    url = paste0("https://fixtures.test/", c("same", "new", "new"), ".xlsx"),
    published_at = as.Date(c("2026-01-01", "2026-02-01", "2026-03-01")),
    stringsAsFactors = FALSE
  )
  before <- list.files(work, all.files = TRUE, recursive = TRUE)
  result <- pboc_revisions(snapshot_path, current)
  after <- list.files(work, all.files = TRUE, recursive = TRUE)

  expect_equal(before, after)
  expect_named(
    result,
    c(
      "asset_id", "status", "previous_revision", "current_revision",
      "previous_url", "current_url"
    ),
    ignore.order = FALSE
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(result$asset_id, c("same", "revised", "removed", "new"))
  expect_equal(result$status, c("unchanged", "revised", "removed", "new"))
  expect_equal(result$current_revision, c("r1", "r2", NA, "r1"))
  expect_equal(result$previous_revision, c("r1", "r1", "r1", NA))
  expect_error(
    pboc_revisions(data.frame(asset_id = "a"), current),
    "Both snapshots need"
  )
  expect_error(
    pboc_revisions(file.path(work, "missing.csv"), current),
    "existing CSV path"
  )
  expect_error(
    pboc_revisions(NA_character_, current),
    "data frame or an existing CSV path"
  )
})

test_that("pboc_read handles local HTML, Excel sheets, and explicit unsupported formats", {
  work <- tempfile("readPBOC-read-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  html_path <- file.path(work, "sample.html")
  writeLines(
    c(
      "<html><body><table>",
      "<tr><th>item</th><th>value</th></tr>",
      "<tr><td>alpha</td><td>100</td></tr>",
      "</table></body></html>"
    ),
    html_path,
    useBytes = TRUE
  )
  html_before <- list.files(work, all.files = TRUE)
  html_result <- pboc_read(html_path)
  html_after <- list.files(work, all.files = TRUE)

  expect_named(html_result, c("item", "value"), ignore.order = FALSE)
  expect_s3_class(html_result, "tbl_df")
  expect_equal(html_result$item, "alpha")
  expect_equal(as.character(html_result$value), "100")
  expect_true(is.na(attr(html_result, "source_url")))
  expect_equal(attr(html_result, "source_path"), html_path)
  expect_equal(html_before, html_after)

  source_xlsx <- system.file("extdata", "clippy.xlsx", package = "readxl")
  expect_true(nzchar(source_xlsx))
  xlsx_path <- file.path(work, "clippy.xlsx")
  expect_true(file.copy(source_xlsx, xlsx_path, overwrite = TRUE))
  xlsx_result <- pboc_read(xlsx_path)

  expect_named(xlsx_result, c("name", "value"), ignore.order = FALSE)
  expect_s3_class(xlsx_result, "tbl_df")
  expect_equal(nrow(xlsx_result), 4L)
  expect_equal(xlsx_result$name[[1L]], "Name")
  expect_equal(attr(xlsx_result, "source_path"), xlsx_path)
  expect_true(is.na(attr(xlsx_result, "source_url")))

  all_sheets <- pboc_read(xlsx_path, sheet = NA)
  expect_type(all_sheets, "list")
  expect_named(all_sheets, c("list-column", "two-row-header"), ignore.order = FALSE)
  expect_true(all(vapply(all_sheets, inherits, logical(1), what = "data.frame")))
  expect_equal(attr(all_sheets, "source_path"), xlsx_path)
  expect_true(is.na(attr(all_sheets, "source_url")))

  pdf_path <- file.path(work, "resource.pdf")
  expect_true(file.create(pdf_path))
  expect_error(
    pboc_read(pdf_path),
    "PDF extraction is not normalised"
  )
  csv_path <- file.path(work, "resource.csv")
  writeLines("a,b\n1,2", csv_path)
  expect_error(
    pboc_read(csv_path),
    "Unsupported PBOC resource format: csv"
  )
})

test_that("pboc_download supports offline file URLs and cleans temporary files", {
  work <- tempfile("readPBOC-download-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  source_path <- file.path(work, "source.xlsx")
  writeBin(charToRaw("first fixture bytes"), source_path)
  source_path <- normalizePath(source_path, winslash = "/", mustWork = TRUE)
  source_url <- if (.Platform$OS.type == "windows") {
    paste0("file:///", sub("^/+", "", source_path))
  } else {
    paste0("file://", source_path)
  }
  destination <- file.path(work, "destination")
  manifest <- file.path(destination, "manifest.csv")
  record <- tibble::tibble(
    asset_id = "asset-1",
    revision_id = "202607151",
    item = "Money Supply",
    format = "xlsx",
    published_at = as.Date("2026-07-15"),
    url = source_url
  )

  first <- pboc_download(record, destination, manifest = manifest)
  expected_path <- normalizePath(
    file.path(destination, "money-supply-202607151.xlsx"),
    winslash = "/",
    mustWork = TRUE
  )
  expected_names <- c(
    "asset_id", "revision_id", "status", "downloaded_at", "published_at",
    "url", "path", "size", "md5"
  )
  expect_named(first, expected_names, ignore.order = FALSE)
  expect_equal(first$status, "downloaded")
  expect_equal(first$path, expected_path)
  expect_equal(first$size, file.info(source_path)$size)
  expect_equal(first$md5, unname(tools::md5sum(source_path)))
  expect_type(first$published_at, "character")
  expect_equal(first$published_at, "2026-07-15")
  expect_true(file.exists(manifest))
  expect_length(list.files(destination, pattern = "^readPBOC-download-"), 0L)

  manifest_first <- utils::read.csv(manifest, stringsAsFactors = FALSE)
  expect_named(manifest_first, expected_names, ignore.order = FALSE)
  second <- pboc_download(record, destination, manifest = manifest)
  expect_equal(second$status, "existing")
  expect_equal(second$path, first$path)
  expect_equal(second$md5, first$md5)
  expect_equal(nrow(utils::read.csv(manifest, stringsAsFactors = FALSE)), 1L)
  expect_length(list.files(destination, pattern = "^readPBOC-download-"), 0L)

  writeBin(charToRaw("second fixture bytes"), source_path)
  overwritten <- pboc_download(
    record,
    destination,
    manifest = manifest,
    overwrite = TRUE
  )
  expect_equal(overwritten$status, "downloaded")
  expect_false(identical(overwritten$md5, first$md5))
  expect_equal(overwritten$md5, unname(tools::md5sum(source_path)))
  manifest_final <- utils::read.csv(manifest, stringsAsFactors = FALSE)
  expect_equal(nrow(manifest_final), 2L)
  expect_equal(nrow(unique(manifest_final[c("path", "md5")])), 2L)
  expect_length(list.files(destination, pattern = "^readPBOC-download-"), 0L)
})
