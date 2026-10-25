#' Download PBOC resources with revision metadata
#'
#' Files are named with the PBOC attachment identifier so a revised workbook
#' does not silently overwrite an earlier revision. A CSV manifest records
#' source URL, revision, checksum, size, and retrieval time.
#'
#' @param x One or more rows from [pboc_catalog()], an attachment data frame,
#'   or a direct URL.
#' @param dest_dir Destination directory.
#' @param manifest Manifest CSV path. Use `NULL` to disable it.
#' @param overwrite Whether an existing revision file may be overwritten.
#' @return A tibble describing local files and checksums.
#' @export
#' @examples
#' \dontrun{
#' catalog <- pboc_catalog(2025, "zh", "money")
#' pboc_download(catalog[1, ], "data-raw/pboc")
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

