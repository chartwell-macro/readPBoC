#' Save a PBOC catalog snapshot as CSV
#'
#' `pboc_snapshot()` writes a catalog or other catalog-shaped data frame to a
#' caller-selected CSV path. Parent directories are created recursively when
#' needed. The file is a durable, human-readable record that can later be
#' supplied to [pboc_revisions()]; it is not a cache used automatically by the
#' package.
#'
#' @param x A catalog data frame, normally returned by [pboc_catalog()]. All
#'   columns are written in their current order; `x` must inherit from
#'   `data.frame`.
#' @param path One destination CSV path. Parent directories are created with
#'   `showWarnings = FALSE`; an existing file is overwritten by
#'   [utils::write.csv()].
#' @return The supplied `path`, a length-one character string returned
#'   invisibly.
#' @section Errors and side effects:
#' A non-data-frame `x` raises an error. The function performs no network
#' access, but it creates or replaces the requested CSV and can create its
#' parent directories; filesystem permission or encoding errors from the base R
#' writer are propagated.
#' @family preservation
#' @seealso [pboc_catalog()] for the source catalog and [pboc_revisions()] for
#'   comparing snapshots.
#' @export
#' @examples
#' # Synthetic catalog metadata; the reserved URL is never contacted.
#' catalog <- data.frame(
#'   asset_id = "zh/2026/money/money-supply/NA/xlsx",
#'   revision_id = "202607151",
#'   url = "https://example.test/readpboc-fixture.xlsx",
#'   stringsAsFactors = FALSE
#' )
#' path <- tempfile("readPBOC-snapshot-", fileext = ".csv")
#' pboc_snapshot(catalog, path)
#' stopifnot(file.exists(path))
#' unlink(path)
pboc_snapshot <- function(x, path) {
  if (!is.data.frame(x)) stop("`x` must be a catalog data frame.",
                              call. = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

#' Compare two PBOC catalog snapshots
#'
#' `pboc_revisions()` compares the stable `asset_id` keys in two catalog-shaped
#' data frames or CSV snapshots. It labels each key as `"new"`, `"removed"`,
#' `"unchanged"`, or `"revised"`, making it possible to audit an archive
#' without downloading every attachment. A CSV input is read with
#' [utils::read.csv()] only when the supplied character path already exists;
#' there is no network access and no output file is written.
#'
#' An asset is `"unchanged"` when every current revision ID is present in the
#' previous revisions or every current URL is present in the previous URLs. If
#' both snapshots contain the asset but neither condition holds, it is
#' `"revised"`. For each key, the previous/current revision and URL are taken
#' from the row with the latest parseable `published_at` date when that column
#' exists, otherwise from the last row for that key. Keys retain first-seen
#' order from `previous` followed by new keys in `current`.
#'
#' @param previous An earlier catalog data frame or a length-one path to an
#'   existing snapshot CSV. It must contain `asset_id`, `revision_id`, and
#'   `url` columns.
#' @param current A current catalog data frame or a length-one path to an
#'   existing snapshot CSV with the same required columns.
#' @return A tibble with columns `asset_id`, `status`, `previous_revision`,
#'   `current_revision`, `previous_url`, and `current_url`, in that order.
#'   Revision and URL fields are `NA` for an asset absent from the corresponding
#'   snapshot. An empty pair of snapshots returns an empty tibble.
#' @section Errors:
#' A character path that does not exist is not treated as a data frame and
#' raises an error. Inputs missing any of the three required columns raise an
#'   error; extra columns are ignored for classification except
#'   `published_at`, which is used to choose the displayed latest row.
#' @family preservation
#' @seealso [pboc_snapshot()] for creating the CSV input and [pboc_catalog()]
#'   for the stable/revision fields.
#' @export
#' @examples
#' previous <- data.frame(
#'   asset_id = "zh/2025/money/money-supply/NA/xlsx",
#'   revision_id = "20250101",
#'   url = "https://example.test/old.xlsx"
#' )
#' current <- transform(previous,
#'   revision_id = "20250201",
#'   url = "https://example.test/new.xlsx"
#' )
#' pboc_revisions(previous, current)
pboc_revisions <- function(previous, current) {
  previous <- read_snapshot(previous)
  current <- read_snapshot(current)
  required <- c("asset_id", "revision_id", "url")
  if (!all(required %in% names(previous)) ||
      !all(required %in% names(current))) {
    stop("Both snapshots need `asset_id`, `revision_id`, and `url` columns.",
         call. = FALSE)
  }

  keys <- union(unique(previous$asset_id), unique(current$asset_id))
  results <- lapply(keys, function(key) {
    old <- previous[previous$asset_id == key, , drop = FALSE]
    new <- current[current$asset_id == key, , drop = FALSE]
    if (!nrow(old)) {
      status <- "new"
    } else if (!nrow(new)) {
      status <- "removed"
    } else if (all(new$revision_id %in% old$revision_id) ||
               all(new$url %in% old$url)) {
      status <- "unchanged"
    } else {
      status <- "revised"
    }
    old_latest <- latest_snapshot_row(old)
    new_latest <- latest_snapshot_row(new)
    tibble::tibble(
      asset_id = key,
      status = status,
      previous_revision = if (nrow(old_latest)) old_latest$revision_id[[1L]]
      else NA,
      current_revision = if (nrow(new_latest)) new_latest$revision_id[[1L]]
      else NA,
      previous_url = if (nrow(old_latest)) old_latest$url[[1L]] else NA,
      current_url = if (nrow(new_latest)) new_latest$url[[1L]] else NA
    )
  })
  bind_rows_base(results)
}

latest_snapshot_row <- function(x) {
  if (!nrow(x)) return(x)
  if ("published_at" %in% names(x)) {
    dates <- suppressWarnings(as.Date(x$published_at))
    if (any(!is.na(dates))) {
      return(x[which.max(dates), , drop = FALSE])
    }
  }
  x[nrow(x), , drop = FALSE]
}

read_snapshot <- function(x) {
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    return(utils::read.csv(x, stringsAsFactors = FALSE,
                           na.strings = c("", "NA")))
  }
  if (!is.data.frame(x)) {
    stop("A snapshot must be a data frame or an existing CSV path.",
         call. = FALSE)
  }
  x
}
