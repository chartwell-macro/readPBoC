#' Print a PBOC resource index
#'
#' Prints a compact header containing the number of rows, then delegates to the
#' next method (normally the tibble/data-frame printer) for the tabular body.
#' This method is installed for the `pboc_resource_index` class returned by
#' [pboc_years()], [pboc_sections()], and [pboc_catalog()].
#'
#' @param x A `pboc_resource_index` object, normally a tibble.
#' @param ... Additional arguments passed to the next print method.
#' @return `x`, invisibly, after writing the header and table to the console.
#' @seealso [pboc_years()], [pboc_sections()], [pboc_catalog()]
#' @export
#' @examples
#' index <- structure(
#'   data.frame(year = 2026L, language = "en", url = "https://example.test"),
#'   class = c("pboc_resource_index", "data.frame")
#' )
#' print(index)
print.pboc_resource_index <- function(x, ...) {
  cat("<PBOC resource index:", nrow(x), "rows>\n")
  NextMethod()
}




#' Print a PBOC resource row or table
#'
#' A `pboc_resource` object uses the ordinary next data-frame/tibble print
#' method. The method is provided for resource objects created by callers or
#' future package workflows; current catalog functions return
#' `pboc_resource_index` objects.
#'
#' @param x A data-frame-like object inheriting from `pboc_resource`.
#' @param ... Additional arguments passed to the next print method.
#' @return `x`, invisibly, after printing it with the next available method.
#' @seealso [pboc_catalog()] for the resource-index class.
#' @export
#' @examples
#' resource <- structure(
#'   data.frame(item = "Money Supply", format = "xlsx"),
#'   class = c("pboc_resource", "data.frame")
#' )
#' print(resource)
print.pboc_resource <- function(x, ...) {
  NextMethod()
}

#' Print a PBOC release index
#'
#' Prints a compact row count and then delegates to the next tabular print
#' method. This is the display method for the `pboc_release_index` tibble
#' returned by [pboc_releases()].
#'
#' @param x A `pboc_release_index` object, normally a tibble.
#' @param ... Additional arguments passed to the next print method.
#' @return `x`, invisibly, after writing the header and table to the console.
#' @seealso [pboc_releases()] for creating a release index.
#' @export
#' @examples
#' releases <- structure(
#'   data.frame(
#'     release_id = "example", title = "Example release",
#'     published_at = as.Date("2026-07-15")
#'   ),
#'   class = c("pboc_release_index", "data.frame")
#' )
#' print(releases)
print.pboc_release_index <- function(x, ...) {
  cat("<PBOC release index:", nrow(x), "rows>\n")
  NextMethod()
}

#' Print a PBOC release
#'
#' Prints the release title, language, publication date, URL, and number of
#' extracted paragraphs in a short human-readable summary. The complete text
#' and attachment table remain available in the returned object.
#'
#' @param x A list inheriting from `pboc_release`, normally returned by
#'   [pboc_read_release()]. It should contain `title`, `language`,
#'   `published_at`, `url`, and `paragraphs` elements.
#' @param ... Unused additional arguments accepted for the S3 print contract.
#' @return `x`, invisibly, after writing the summary to the console.
#' @seealso [pboc_read_release()] for creating a release object.
#' @export
#' @examples
#' release <- structure(
#'   list(
#'     title = "Example release", language = "en",
#'     published_at = as.Date("2026-07-15"),
#'     url = "https://example.test/release.html", paragraphs = "Text"
#'   ),
#'   class = "pboc_release"
#' )
#' print(release)
print.pboc_release <- function(x, ...) {
  cat("<PBOC release>\n")
  cat("  Title:    ", x$title, "\n", sep = "")
  cat("  Language: ", x$language, "\n", sep = "")
  cat("  Date:     ", as.character(x$published_at), "\n", sep = "")
  cat("  URL:      ", x$url, "\n", sep = "")
  cat("  Text:     ", length(x$paragraphs), " paragraph(s)\n", sep = "")
  invisible(x)
}
