#' Save a PBOC catalog snapshot
#'
#' @param x A catalog returned by [pboc_catalog()].
#' @param path Destination CSV file.
#' @return `path`, invisibly.
#' @export
pboc_snapshot <- function(x, path) {
  if (!is.data.frame(x)) stop("`x` must be a catalog data frame.",
                              call. = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

#' Compare two PBOC catalog snapshots
#'
#' @param previous An earlier catalog data frame or snapshot CSV path.
#' @param current A current catalog data frame or snapshot CSV path.
#' @return A tibble classifying stable assets as `"new"`, `"revised"`,
#'   `"unchanged"`, or `"removed"`.
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
