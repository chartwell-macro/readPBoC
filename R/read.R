#' Read a PBOC spreadsheet or HTML table
#'
#' @param x A local path, direct URL, or one catalog row from
#'   [pboc_catalog()].
#' @param sheet Spreadsheet sheet name or number. The default reads the first
#'   sheet. Use `NA` to read every non-empty sheet into a named list.
#' @param tidy If `TRUE`, pass spreadsheet cells through [pboc_tidy()].
#' @param ... Additional arguments passed to [readxl::read_excel()] for
#'   spreadsheets.
#' @return A tibble, a named list of tibbles, or an HTML table.
#' @export
#' @examples
#' \dontrun{
#' catalog <- pboc_catalog(2025, "en", "money", c("xls", "xlsx"))
#' pboc_read(catalog[grepl("Money Supply", catalog$item), ][1, ], tidy = TRUE)
#' }
pboc_read <- function(x, sheet = 1, tidy = FALSE, ...) {
  is_url <- is.character(x) && length(x) == 1L &&
    grepl("^https?://", x)
  is_path <- is.character(x) && length(x) == 1L && file.exists(x)
  source <- if (is_url || is_path) x else resource_url(x)
  format <- if (is_path) {
    extension_from_url(source)
  } else {
    record_value(x, "format", extension_from_url(source))
  }

  local_path <- source
  remove_after <- FALSE
  if (grepl("^https?://", source)) {
    suffix <- paste0(".", ifelse(format == "html", "html", format))
    local_path <- tempfile("readPBOC-", fileext = suffix)
    pboc_download_url(source, local_path)
    remove_after <- TRUE
    on.exit(if (remove_after && file.exists(local_path)) unlink(local_path),
            add = TRUE)
  }

  if (format %in% c("xls", "xlsx")) {
    sheets <- readxl::excel_sheets(local_path)
    if (length(sheet) == 1L && is.na(sheet)) {
      out <- lapply(sheets, function(current_sheet) {
        read_pboc_sheet(local_path, current_sheet, tidy, ...)
      })
      names(out) <- sheets
      nonempty <- vapply(out, function(value) nrow(value) || ncol(value),
                         logical(1))
      out <- out[nonempty]
    } else {
      out <- read_pboc_sheet(local_path, sheet, tidy, ...)
    }
  } else if (format == "html") {
    tables <- rvest::html_table(xml2::read_html(local_path), fill = TRUE)
    out <- if (length(tables) == 1L) tables[[1L]] else tables
  } else if (format == "pdf") {
    stop("PDF extraction is not normalised. Use `pboc_download()` to retain ",
         "the official PDF.", call. = FALSE)
  } else {
    stop("Unsupported PBOC resource format: ", format, call. = FALSE)
  }

  attr(out, "source_url") <- if (is_url) source else record_value(x, "url")
  attr(out, "source_path") <- if (is_path) source else NULL
  out
}

read_pboc_sheet <- function(path, sheet, tidy, ...) {
  dots <- list(...)
  if (tidy) {
    dots$col_names <- FALSE
    dots$col_types <- "text"
    dots$.name_repair <- "minimal"
  }
  args <- c(list(path = path, sheet = sheet), dots)
  raw <- do.call(readxl::read_excel, args)
  if (tidy) pboc_tidy(raw) else raw
}

#' Read one matching PBOC data series or table
#'
#' This is a high-level wrapper around [pboc_catalog()] and [pboc_read()].
#' `"auto"` prefers the Chinese archive and falls back to English.
#'
#' @param year Four-digit statistics year.
#' @param section Section ID, title fragment, or regular expression.
#' @param item Optional item title fragment or regular expression. Required
#'   when the selected section contains more than one workbook.
#' @param language One of `"auto"`, `"zh"`, or `"en"`.
#' @param formats Preferred spreadsheet formats, in order.
#' @param sheet Sheet passed to [pboc_read()].
#' @param tidy Return the normalised long form.
#' @param ... Additional arguments passed to [pboc_read()].
#' @return The selected PBOC data.
#' @export
#' @examples
#' \dontrun{
#' pboc_data(2025, "money", "Money Supply", language = "en")
#' pboc_data(2026, "money", "Money Supply", language = "zh")
#' }
pboc_data <- function(year,
                      section,
                      item = NULL,
                      language = c("auto", "zh", "en"),
                      formats = c("xlsx", "xls"),
                      sheet = 1,
                      tidy = TRUE,
                      ...) {
  language <- match.arg(language)
  languages <- if (language == "auto") c("zh", "en") else language
  matches <- tibble::tibble()

  for (lang in languages) {
    catalog <- tryCatch(
      pboc_catalog(year, lang, section = section, formats = formats),
      error = function(error) tibble::tibble()
    )
    catalog <- query_filter(
      catalog, item, c("item", "item_zh", "item_en")
    )
    if (nrow(catalog)) {
      matches <- catalog
      break
    }
  }
  if (!nrow(matches)) {
    stop("No PBOC workbook matched the requested year, section, item, and ",
         "language.", call. = FALSE)
  }

  preference <- match(matches$format, formats)
  published_order <- suppressWarnings(as.numeric(matches$published_at))
  matches <- matches[order(preference, -published_order, na.last = TRUE), ,
                     drop = FALSE]
  unique_assets <- unique(matches$asset_id)
  if (length(unique_assets) > 1L) {
    choices <- unique(matches$item)
    stop(
      "More than one PBOC item matched. Supply a narrower `item`: ",
      paste(utils::head(choices, 8L), collapse = "; "),
      call. = FALSE
    )
  }
  pboc_read(matches[1L, , drop = FALSE], sheet = sheet, tidy = tidy, ...)
}

#' Normalise an irregular PBOC worksheet
#'
#' Detects one or more dated header rows, joins bilingual series labels that
#' are either in the same cell or on adjacent rows, and returns observations
#' in long form. Source row and column numbers are retained for auditability.
#'
#' @param x A data frame read with `col_names = FALSE`; cells are converted to
#'   character before processing.
#' @return A tibble with `table`, bilingual series labels, period, numeric and
#'   raw values, unit, and source coordinates. Notes are retained in the
#'   `"notes"` attribute.
#' @export
pboc_tidy <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!nrow(x) || !ncol(x)) return(tibble::tibble())
  x[] <- lapply(x, function(column) empty_to_na(clean_text_keep_indent(column)))

  nonempty_rows <- apply(!is.na(x), 1L, any)
  nonempty_cols <- apply(!is.na(x), 2L, any)
  original_rows <- which(nonempty_rows)
  original_cols <- which(nonempty_cols)
  x <- x[nonempty_rows, nonempty_cols, drop = FALSE]
  if (!nrow(x) || !ncol(x)) return(tibble::tibble())

  time_matrix <- vapply(x, is_time_label, logical(nrow(x)))
  if (is.null(dim(time_matrix))) {
    time_matrix <- matrix(time_matrix, ncol = ncol(x))
  }
  column_time_counts <- colSums(time_matrix, na.rm = TRUE)
  row_time_counts <- rowSums(time_matrix, na.rm = TRUE)
  if (
    max(column_time_counts, 0L) >= 2L &&
      max(column_time_counts, 0L) > max(row_time_counts, 0L)
  ) {
    period_column <- tidy_period_column(x, original_rows, original_cols)
    if (nrow(period_column)) return(period_column)
  }
  header_rows <- which(row_time_counts >= 2L)
  if (!length(header_rows)) {
    period_column <- tidy_period_column(x, original_rows, original_cols)
    if (nrow(period_column)) return(period_column)
    return(tidy_fallback(x, original_rows, original_cols))
  }

  unit <- extract_unit(x)
  results <- list()
  result_index <- 0L

  for (table_index in seq_along(header_rows)) {
    header <- header_rows[[table_index]]
    end <- if (table_index < length(header_rows)) {
      header_rows[[table_index + 1L]] - 1L
    } else {
      nrow(x)
    }
    time_cols <- which(time_matrix[header, ])
    label_cols <- setdiff(seq_len(ncol(x)), time_cols)
    if (!length(label_cols)) next

    for (row in seq.int(header + 1L, end)) {
      raw_values <- unlist(x[row, time_cols, drop = TRUE], use.names = FALSE)
      has_values <- is_value_cell(raw_values)
      if (!any(has_values, na.rm = TRUE)) next

      label <- collapse_label(x[row, label_cols, drop = TRUE])
      if (!nzchar(label)) next
      bilingual <- split_bilingual(label)
      series_zh <- bilingual$zh[[1L]]
      series_en <- bilingual$en[[1L]]

      if ((is.na(series_en) || !nzchar(series_en)) && row < end) {
        next_values <- unlist(x[row + 1L, time_cols, drop = TRUE],
                              use.names = FALSE)
        next_label <- collapse_label(x[row + 1L, label_cols, drop = TRUE])
        if (!any(is_value_cell(next_values), na.rm = TRUE) &&
            nzchar(next_label) &&
            grepl("[A-Za-z]", next_label)) {
          series_en <- clean_text(next_label)
        }
      }
      series <- if (!is.na(series_en) && nzchar(series_en)) {
        series_en
      } else if (!is.na(series_zh) && nzchar(series_zh)) {
        series_zh
      } else {
        clean_text(label)
      }

      selected <- which(has_values)
      result_index <- result_index + 1L
      periods <- normalize_period_label(unlist(
        x[header, time_cols[selected], drop = TRUE],
        use.names = FALSE
      ))
      values <- clean_text(raw_values[selected])
      results[[result_index]] <- tibble::tibble(
        table = table_index,
        series = series,
        series_zh = series_zh,
        series_en = series_en,
        period = periods,
        value = parse_value(values),
        value_raw = values,
        unit = unit,
        source_row = original_rows[[row]],
        source_column = original_cols[time_cols[selected]]
      )
    }
  }

  out <- bind_rows_base(results)
  note_rows <- apply(x, 1L, function(row) {
    text <- clean_text(paste(row[!is.na(row)], collapse = " "))
    if (grepl("^(\u6ce8|note\\b|notes\\b)", tolower(text), perl = TRUE)) {
      text
    } else {
      ""
    }
  })
  attr(out, "notes") <- unique(note_rows[nzchar(note_rows)])
  out
}

decimal_month_components <- function(x) {
  x <- clean_text(x)
  numeric_period <- suppressWarnings(as.numeric(x))
  year <- suppressWarnings(as.integer(floor(numeric_period)))
  month <- suppressWarnings(as.integer(round((numeric_period - year) * 100)))
  reconstructed <- year + month / 100
  valid <-
    grepl("^(19|20)[0-9]{2}\\.[0-9]{3,}$", x) &
    !is.na(year) &
    year >= 1900L &
    year <= 2099L &
    !is.na(month) &
    month >= 1L &
    month <= 12L &
    is.finite(numeric_period) &
    abs(numeric_period - reconstructed) < 1e-7
  list(valid = valid, year = year, month = month)
}

normalize_period_label <- function(x) {
  x <- clean_text(x)
  decimal_month <- decimal_month_components(x)
  if (any(decimal_month$valid)) {
    x[decimal_month$valid] <- sprintf(
      "%04d.%02d",
      decimal_month$year[decimal_month$valid],
      decimal_month$month[decimal_month$valid]
    )
  }
  x
}

is_time_label <- function(x) {
  x <- normalize_period_label(x)
  !is.na(x) & (
    grepl(
      paste0(
        "^(19|20)[0-9]{2}",
        "([./-](0?[1-9]|1[0-2]))?",
        "([./-](0?[1-9]|[12][0-9]|3[01]))?$"
      ),
      x
    ) |
      grepl("^(0?[1-9]|1[0-2])[./-](19|20)[0-9]{2}$", x) |
      grepl("^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*[ -](19|20)[0-9]{2}$",
            x, ignore.case = TRUE)
  )
}

is_value_cell <- function(x) {
  x <- clean_text(x)
  !is.na(x) & grepl(
    "^[-+]?([0-9]{1,3}(,[0-9]{3})+|[0-9]+)(\\.[0-9]+)?%?$",
    x
  )
}

parse_value <- function(x) {
  value <- gsub(",", "", x, fixed = TRUE)
  value <- sub("%$", "", value)
  suppressWarnings(as.numeric(value))
}

wide_sheet_label <- function(x) {
  x <- clean_text(x)
  x <- x[!is.na(x) & nzchar(x)]
  x <- x[!grepl(
    paste0(
      "^(\u5355\u4f4d|unit|of which|\u5176\u4e2d|items?|",
      "\u9879\u76ee|month|\u6708\u4efd|\u5e74\u6708)\\b|100million"
    ),
    x,
    ignore.case = TRUE
  )]
  if (!length(x)) return(NA_character_)

  english <- x[grepl("[A-Za-z]", x)]
  if (length(english)) {
    return(clean_text(paste(utils::tail(unique(english), 2L), collapse = " ")))
  }
  utils::tail(unique(x), 1L)
}

wide_sheet_bilingual <- function(x) {
  x <- clean_text(x)
  without_enumerator <- sub("^[A-Z][.)]\\s*", "", x)
  has_chinese <- grepl("[\u4e00-\u9fff]", without_enumerator)
  has_english <- grepl("[A-Za-z]", without_enumerator)
  english <- if (has_english && has_chinese) {
    sub("^.*[\u4e00-\u9fff]\\s*", "", without_enumerator)
  } else if (has_english) {
    without_enumerator
  } else {
    NA_character_
  }
  if (!is.na(english)) {
    english <- sub("^[^A-Za-z0-9]+", "", english)
  }
  chinese <- if (has_chinese) {
    clean_text(sub("\\s*[A-Za-z].*$", "", without_enumerator))
  } else {
    NA_character_
  }
  list(
    series = if (!is.na(english) && nzchar(english)) {
      english
    } else {
      without_enumerator
    },
    series_zh = if (!is.na(chinese) && nzchar(chinese)) {
      chinese
    } else {
      NA_character_
    },
    series_en = if (!is.na(english) && nzchar(english)) {
      english
    } else {
      NA_character_
    }
  )
}

tidy_period_column <- function(x, original_rows, original_cols) {
  period_counts <- vapply(
    x,
    function(column) sum(is_time_label(column), na.rm = TRUE),
    integer(1)
  )
  time_column <- which.max(period_counts)
  if (!length(time_column) || period_counts[[time_column]] < 2L) {
    return(tibble::tibble())
  }

  period_rows <- which(is_time_label(x[[time_column]]))
  block_id <- cumsum(c(TRUE, diff(period_rows) > 1L))
  blocks <- split(period_rows, block_id)
  rows <- list()
  row_index <- 0L
  previous_end <- 0L

  for (block_number in seq_along(blocks)) {
    block_rows <- blocks[[block_number]]
    header_start <- previous_end + 1L
    header_end <- min(block_rows) - 1L
    header_rows <- if (header_end >= header_start) {
      seq.int(header_start, header_end)
    } else {
      integer()
    }
    unit_text <- clean_text(unlist(
      x[header_rows, , drop = FALSE],
      use.names = FALSE
    ))
    unit_text <- unit_text[
      !is.na(unit_text) &
        grepl(
          "\u5355\u4f4d|unit|%|100million",
          unit_text,
          ignore.case = TRUE
        )
    ]
    unit <- if (length(unit_text)) {
      paste(unique(unit_text), collapse = " / ")
    } else {
      NA_character_
    }
    measure <- if (block_number > 1L && length(header_rows)) {
      wide_sheet_label(x[header_rows, time_column, drop = TRUE])
    } else {
      NA_character_
    }

    for (column in setdiff(seq_len(ncol(x)), time_column)) {
      raw_values <- x[block_rows, column, drop = TRUE]
      numeric <- is_value_cell(raw_values)
      if (!any(numeric)) next

      label <- wide_sheet_label(x[header_rows, column, drop = TRUE])
      if (!is.na(measure) && nzchar(measure)) {
        label <- clean_text(paste(label, measure, sep = " \u2014 "))
      }
      if (is.na(label) || !nzchar(label)) {
        label <- paste("Series", original_cols[[column]])
      }
      bilingual <- wide_sheet_bilingual(label)
      selected <- which(numeric)
      row_index <- row_index + 1L
      rows[[row_index]] <- tibble::tibble(
        table = block_number,
        series = bilingual$series,
        series_zh = bilingual$series_zh,
        series_en = bilingual$series_en,
        period = normalize_period_label(
          x[block_rows[selected], time_column, drop = TRUE]
        ),
        value = parse_value(raw_values[selected]),
        value_raw = clean_text(raw_values[selected]),
        unit = unit,
        source_row = original_rows[block_rows[selected]],
        source_column = original_cols[[column]]
      )
    }
    previous_end <- max(block_rows)
  }

  bind_rows_base(rows)
}

collapse_label <- function(x) {
  values <- as.character(unlist(x, use.names = FALSE))
  values <- clean_text_keep_indent(values)
  values <- values[!is.na(values) & nzchar(clean_text(values))]
  clean_text_keep_indent(paste(values, collapse = " / "))
}

extract_unit <- function(x) {
  first <- x[seq_len(min(12L, nrow(x))), , drop = FALSE]
  rows <- apply(first, 1L, function(row) {
    text <- clean_text(paste(row[!is.na(row)], collapse = " "))
    if (grepl("\u5355\u4f4d|\\bunit\\b", tolower(text), perl = TRUE)) {
      text
    } else {
      ""
    }
  })
  rows <- unique(rows[nzchar(rows)])
  if (length(rows)) paste(rows, collapse = " / ") else NA_character_
}

tidy_fallback <- function(x, original_rows, original_cols) {
  names(x) <- paste0("column_", original_cols)
  out <- tibble::as_tibble(x)
  attr(out, "source_rows") <- original_rows
  warning(
    "No dated header row was detected; returning a cleaned wide table.",
    call. = FALSE
  )
  out
}
