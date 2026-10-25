`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

pboc_languages <- function(language, allow_auto = FALSE) {
  choices <- c("zh", "en", "both", if (allow_auto) "auto")
  language <- match.arg(language, choices)
  if (identical(language, "both")) c("zh", "en") else language
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub(" {2,}", " ", x)
  trimws(x)
}

clean_text_keep_indent <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("[\r\n\t]+", " ", x)
  trimws(x, which = "right")
}

empty_to_na <- function(x) {
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}

bind_rows_base <- function(xs) {
  xs <- Filter(function(x) !is.null(x) && nrow(x) > 0L, xs)
  if (!length(xs)) return(tibble::tibble())
  columns <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    missing <- setdiff(columns, names(x))
    for (column in missing) x[[column]] <- NA
    x[columns]
  })
  tibble::as_tibble(do.call(rbind, xs))
}

distinct_rows <- function(x, columns) {
  if (!nrow(x)) return(x)
  columns <- intersect(columns, names(x))
  x[!duplicated(x[columns]), , drop = FALSE]
}

absolute_url <- function(href, source_url) {
  xml2::url_absolute(href, source_url)
}

extension_from_url <- function(url) {
  path <- sub("[?#].*$", "", url)
  ext <- tolower(sub("^.*\\.([^.]+)$", "\\1", path))
  ext[ext == "htm"] <- "html"
  ext
}

revision_from_url <- function(url) {
  path <- sub("[?#].*$", "", url)
  sub("\\.[^.]+$", "", basename(path))
}

attachment_date <- function(url) {
  revision <- revision_from_url(url)
  candidate <- substr(revision, 1L, 8L)
  valid <- grepl("^(19|20)[0-9]{6}$", candidate)
  out <- rep(as.Date(NA), length(url))
  out[valid] <- as.Date(candidate[valid], "%Y%m%d")
  out
}

slugify <- function(x, fallback = "pboc-resource") {
  x <- enc2utf8(clean_text(x %||% fallback))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("(^-+|-+$)", "", x)
  ifelse(nzchar(x), x, fallback)
}

split_bilingual <- function(x) {
  x <- clean_text_keep_indent(x)
  is_zh <- grepl("[\u3400-\u9fff]", x)
  is_en <- grepl("[A-Za-z]", x)
  zh <- en <- rep(NA_character_, length(x))

  for (i in seq_along(x)) {
    value <- x[[i]]
    if (is.na(value) || !nzchar(value)) next
    if (is_zh[[i]] && is_en[[i]]) {
      boundary <- regexpr("\\s+(?=[A-Za-z])", value, perl = TRUE)
      if (boundary[[1L]] > 0L) {
        zh[[i]] <- trimws(substr(value, 1L, boundary[[1L]] - 1L))
        en[[i]] <- trimws(substr(
          value,
          boundary[[1L]] + attr(boundary, "match.length"),
          nchar(value)
        ))
      } else {
        zh[[i]] <- clean_text(value)
      }
    } else if (is_zh[[i]]) {
      zh[[i]] <- clean_text(value)
    } else if (is_en[[i]]) {
      en[[i]] <- clean_text(value)
    }
  }

  list(zh = zh, en = en)
}

section_id_from_title <- function(x) {
  x <- tolower(clean_text(x))
  patterns <- list(
    aggregate_financing = paste0(
      "\u793e\u4f1a\u878d\u8d44", "|aggregate financing"
    ),
    money_and_banking = paste0(
      "\u8d27\u5e01\u7edf\u8ba1", "|money and banking"
    ),
    balance_sheets = paste0(
      "\u91d1\u878d\u4e1a\u673a\u6784\u8d44\u4ea7\u8d1f\u503a",
      "|assets and liabilities"
    ),
    credit_funds = paste0(
      "\u4fe1\u8d37\u6536\u652f", "|sources and uses of credit"
    ),
    financial_markets = paste0(
      "\u91d1\u878d\u5e02\u573a\u7edf\u8ba1",
      "|financial market statistics"
    ),
    financial_accounts = paste0(
      "\u91d1\u878d\u8d26\u6237",
      "|financial accounts|finacial accounts"
    ),
    cgpi = paste0(
      "\u4f01\u4e1a\u5546\u54c1\u4ef7\u683c",
      "|corporate goods price"
    ),
    surveys = paste0(
      "\u666f\u6c14\u8c03\u67e5", "|climate survey"
    )
  )
  out <- rep(NA_character_, length(x))
  for (id in names(patterns)) {
    hit <- is.na(out) & grepl(patterns[[id]], x, perl = TRUE)
    out[hit] <- id
  }
  out
}

query_filter <- function(x, query, fields) {
  if (is.null(query) || !length(query) || !nzchar(query)) return(x)
  fields <- intersect(fields, names(x))
  if (!length(fields) || !nrow(x)) return(x[0, , drop = FALSE])
  haystack <- apply(x[fields], 1L, function(row) {
    paste(row[!is.na(row)], collapse = " ")
  })
  x[grepl(query, haystack, ignore.case = TRUE, perl = TRUE), , drop = FALSE]
}

record_value <- function(x, name, default = NA_character_) {
  if (is.data.frame(x)) {
    if (!nrow(x) || !name %in% names(x)) return(default)
    return(as.character(x[[name]][[1L]]))
  }
  if (is.list(x) && !is.null(x[[name]])) return(as.character(x[[name]][[1L]]))
  default
}

resource_url <- function(x) {
  if (is.character(x) && length(x) == 1L) return(x)
  url <- record_value(x, "url")
  if (is.na(url) || !nzchar(url)) {
    stop("`x` must be a URL, local path, or a catalog row containing `url`.",
         call. = FALSE)
  }
  url
}
