#' Download PBOC resources with revision metadata
#'
#' `pboc_download()` materialises one or more catalog attachments in a
#' caller-selected directory. Filenames combine a slug of the item/title and
#' the revision identifier, so a newly published revision does not silently
#' overwrite an earlier workbook. A manifest CSV records the source URL,
#' revision, checksum, byte size, and retrieval time for each distinct local
#' path/checksum pair.
#'
#' Existing files are returned with `status = "existing"` and are not fetched
#' again when `overwrite = FALSE`. With `overwrite = TRUE`, each file is first
#' downloaded to a temporary file under `dest_dir`, then the existing path is
#' replaced. The temporary file is removed if the download or rename fails.
#' Direct URLs are passed to [curl::curl_download()] (including supported local
#' URL schemes); HTTP(S) uses the package user agent and timeout but this
#' function does not add retry attempts. No credentials are read or written.
#'
#' @param x One or more direct URLs, or a data frame/tibble containing a `url`
#'   column such as rows returned by [pboc_catalog()]. Optional `asset_id`,
#'   `revision_id`, `item`/`title`, `format`, and `published_at` columns supply
#'   metadata; missing values are derived from the URL or use package defaults.
#' @param dest_dir One destination directory. It is created recursively when
#'   absent and receives downloaded files and, by default, the manifest.
#' @param manifest One CSV path for the download manifest; the default is
#'   `file.path(dest_dir, "readPBOC-manifest.csv")`. Use `NULL` to disable
#'   manifest reading and writing. The parent directory of a custom manifest
#'   must already exist.
#' @param overwrite Logical; if `TRUE`, fetch and replace an existing file for
#'   the same generated name. The default is `FALSE`; non-`TRUE` values are
#'   treated as no overwrite.
#' @return A tibble with one row per requested resource and columns
#'   `asset_id`, `revision_id`, `status` (`"downloaded"` or `"existing"`),
#'   `downloaded_at` (UTC character timestamp), `published_at`, `url`, `path`
#'   (normalised absolute local path), `size` (bytes), and `md5`, in that order.
#'   The manifest, when enabled, contains the combined output history with
#'   duplicate `path`/`md5` pairs removed.
#' @section Errors and side effects:
#' Missing or empty `dest_dir`, an empty input or missing `url` column, download
#' failures, filesystem permission errors, and failed renames raise errors.
#' The function creates `dest_dir`, downloaded files, temporary files while a
#' transfer is in progress, and the manifest when `manifest` is not `NULL`.
#' It does not modify a pre-existing file unless `overwrite = TRUE`.
#' @family preservation
#' @seealso [pboc_catalog()] for attachment metadata, [pboc_read()] for
#'   parsing a retained workbook, and [pboc_snapshot()] for catalog snapshots.
#' @export
#' @examples
#' if (interactive()) {
#'   catalog <- pboc_catalog(2025, "zh", "money")
#'   if (nrow(catalog)) {
#'     destination <- tempfile("readPBOC-download-")
#'     dir.create(destination)
#'     pboc_download(catalog[1, , drop = FALSE], destination)
#'     unlink(destination, recursive = TRUE)
#'   }
#' }
pboc_download <- function(x,
                          dest_dir,
                          manifest = file.path(dest_dir,
                                               "readPBOC-manifest.csv"),
                          overwrite = FALSE) {
  if (missing(dest_dir) || length(dest_dir) != 1L || !nzchar(dest_dir)) {
    stop("`dest_dir` must be one directory path.", call. = FALSE)
  }
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  records <- if (is.character(x)) {
    tibble::tibble(url = x)
  } else {
    tibble::as_tibble(x)
  }
  if (!"url" %in% names(records) || !nrow(records)) {
    stop("`x` must contain at least one resource URL.", call. = FALSE)
  }

  results <- lapply(seq_len(nrow(records)), function(i) {
    record <- records[i, , drop = FALSE]
    url <- as.character(record$url[[1L]])
    format <- record_value(record, "format", extension_from_url(url))
    revision <- record_value(record, "revision_id", revision_from_url(url))
    label <- record_value(
      record, "item",
      record_value(record, "title", "pboc-resource")
    )
    filename <- paste0(slugify(label), "-", slugify(revision, "revision"),
                       ".", format)
    path <- file.path(dest_dir, filename)

    status <- "existing"
    if (!file.exists(path) || isTRUE(overwrite)) {
      temporary <- tempfile("readPBOC-download-", tmpdir = dest_dir)
      on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
      pboc_download_url(url, temporary)
      if (file.exists(path) && isTRUE(overwrite)) unlink(path)
      if (!file.rename(temporary, path)) {
        stop("Could not move the downloaded file into ", path, call. = FALSE)
      }
      status <- "downloaded"
    }

    info <- file.info(path)
    tibble::tibble(
      asset_id = record_value(record, "asset_id"),
      revision_id = revision,
      status = status,
      downloaded_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      published_at = record_value(record, "published_at"),
      url = url,
      path = normalizePath(path, winslash = "/", mustWork = TRUE),
      size = unname(info$size),
      md5 = unname(tools::md5sum(path))
    )
  })
  out <- bind_rows_base(results)

  if (!is.null(manifest)) {
    old <- if (file.exists(manifest)) {
      utils::read.csv(manifest, stringsAsFactors = FALSE,
                      na.strings = c("", "NA"))
    } else {
      NULL
    }
    combined <- bind_rows_base(list(old, out))
    combined <- distinct_rows(combined, c("path", "md5"))
    utils::write.csv(combined, manifest, row.names = FALSE, na = "")
  }
  out
}
