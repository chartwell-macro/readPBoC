#' @export
print.pboc_resource_index <- function(x, ...) {
  cat("<PBOC resource index:", nrow(x), "rows>\n")
  NextMethod()
}

#' @export
print.pboc_resource <- function(x, ...) {
  NextMethod()
}

#' @export
print.pboc_release_index <- function(x, ...) {
  cat("<PBOC release index:", nrow(x), "rows>\n")
  NextMethod()
}

#' @export
print.pboc_release <- function(x, ...) {
  cat("<PBOC release>\n")
  cat("  Title:    ", x$title, "\n", sep = "")
  cat("  Language: ", x$language, "\n", sep = "")
  cat("  Date:     ", as.character(x$published_at), "\n", sep = "")
  cat("  URL:      ", x$url, "\n", sep = "")
  cat("  Text:     ", length(x$paragraphs), " paragraph(s)\n", sep = "")
  invisible(x)
}

