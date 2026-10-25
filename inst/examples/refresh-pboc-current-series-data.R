current_series_period <- function(x) {
  readPBOC:::is_time_label(x)
}

current_series_numeric <- function(x) {
  x <- readPBOC:::clean_text(x)
  !is.na(x) & grepl(
    "^[-+]?([0-9]{1,3}(,[0-9]{3})+|[0-9]+)(\\.[0-9]+)?%?$",
    x
  )
}

current_series_label <- function(x) {
  x <- readPBOC:::clean_text(x)
  x <- x[!is.na(x) & nzchar(x)]
  x <- x[!grepl(
    "^(单位|unit|of which|其中|items?|项目|month|月份|年月)\\b|100million",
    x,
    ignore.case = TRUE
  )]
  if (!length(x)) return(NA_character_)

  english <- x[grepl("[A-Za-z]", x)]
  if (length(english)) {
    english <- utils::tail(unique(english), 2L)
    return(readPBOC:::clean_text(paste(english, collapse = " ")))
  }
  utils::tail(unique(x), 1L)
}

current_series_bilingual <- function(x) {
  x <- readPBOC:::clean_text(x)
  # Only remove an explicit outline enumerator. The previous optional
  # punctuation also removed the first capital from ordinary labels such as
  # "Overnight", "Central Bank Bills", and "Weighted Average".
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
    readPBOC:::clean_text(
      sub("\\s*[A-Za-z].*$", "", without_enumerator)
    )
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

current_series_wide_sheet <- function(x, record, sheet_name) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!nrow(x) || !ncol(x)) return(tibble::tibble())
  x[] <- lapply(x, readPBOC:::clean_text)

  period_counts <- vapply(
    x,
    function(column) sum(current_series_period(column), na.rm = TRUE),
    integer(1)
  )
  time_column <- which.max(period_counts)

  if (length(time_column) && period_counts[[time_column]] >= 2L) {
    period_rows <- which(current_series_period(x[[time_column]]))
    block_id <- cumsum(c(TRUE, diff(period_rows) > 1L))
    period_blocks <- split(period_rows, block_id)
    rows <- list()
    row_index <- 0L
    previous_end <- 0L
    for (block_number in seq_along(period_blocks)) {
      block_rows <- period_blocks[[block_number]]
      header_start <- previous_end + 1L
      header_end <- min(block_rows) - 1L
      header_rows <- if (header_end >= header_start) {
        seq.int(header_start, header_end)
      } else {
        integer()
      }
      unit_text <- readPBOC:::clean_text(unlist(
        x[header_rows, , drop = FALSE],
        use.names = FALSE
      ))
      unit_text <- unit_text[
        !is.na(unit_text) &
          grepl("单位|unit|%|100million", unit_text, ignore.case = TRUE)
      ]
      unit <- if (length(unit_text)) {
        paste(unique(unit_text), collapse = " / ")
      } else {
        NA_character_
      }
      measure_label <- if (block_number > 1L && length(header_rows)) {
        current_series_label(x[header_rows, time_column, drop = TRUE])
      } else {
        NA_character_
      }

      for (column in setdiff(seq_len(ncol(x)), time_column)) {
        raw_values <- x[block_rows, column, drop = TRUE]
        numeric <- current_series_numeric(raw_values)
        if (!any(numeric)) next

        label <- current_series_label(x[header_rows, column, drop = TRUE])
        if (!is.na(measure_label) && nzchar(measure_label)) {
          label <- readPBOC:::clean_text(
            paste(label, measure_label, sep = " — ")
          )
        }
        if (is.na(label) || !nzchar(label)) {
          label <- paste("Series", column)
        }
        bilingual <- current_series_bilingual(label)
        selected <- which(numeric)
        row_index <- row_index + 1L
        rows[[row_index]] <- tibble::tibble(
          table = block_number,
          series = bilingual$series,
          series_zh = bilingual$series_zh,
          series_en = bilingual$series_en,
          period = readPBOC:::normalize_period_label(
            x[block_rows[selected], time_column, drop = TRUE]
          ),
          value = readPBOC:::parse_value(raw_values[selected]),
          value_raw = raw_values[selected],
          unit = unit,
          source_row = block_rows[selected],
          source_column = column
        )
      }
      previous_end <- max(block_rows)
    }
    return(dplyr::bind_rows(rows))
  }

  numeric_matrix <- vapply(x, current_series_numeric, logical(nrow(x)))
  if (is.null(dim(numeric_matrix))) {
    numeric_matrix <- matrix(numeric_matrix, ncol = ncol(x))
  }
  numeric_counts <- colSums(numeric_matrix, na.rm = TRUE)
  numeric_columns <- which(numeric_counts > 0L)
  if (!length(numeric_columns)) return(tibble::tibble())

  column_headers <- vapply(numeric_columns, function(column) {
    first_numeric <- which(numeric_matrix[, column])[[1L]]
    current_series_label(x[seq_len(max(1L, first_numeric - 1L)), column])
  }, character(1))
  usd_columns <- numeric_columns[grepl(
    "USD|美元",
    column_headers,
    ignore.case = TRUE
  )]
  value_column <- if (length(usd_columns)) {
    usd_columns[[which.max(numeric_counts[usd_columns])]]
  } else {
    numeric_columns[[which.max(numeric_counts[numeric_columns])]]
  }
  value_rows <- which(numeric_matrix[, value_column])
  unit <- column_headers[match(value_column, numeric_columns)]

  rows <- lapply(value_rows, function(row) {
    label_cells <- readPBOC:::clean_text(
      unlist(x[row, seq_len(max(1L, value_column - 1L)), drop = FALSE])
    )
    label_cells <- label_cells[
      !is.na(label_cells) &
        nzchar(label_cells) &
        !current_series_numeric(label_cells)
    ]
    label <- if (length(label_cells)) {
      paste(label_cells, collapse = " / ")
    } else {
      paste("Row", row)
    }
    bilingual <- current_series_bilingual(label)
    raw_value <- x[row, value_column, drop = TRUE]
    tibble::tibble(
      table = 1L,
      series = bilingual$series,
      series_zh = bilingual$series_zh,
      series_en = bilingual$series_en,
      period = as.character(record$published_at[[1L]]),
      value = readPBOC:::parse_value(raw_value),
      value_raw = raw_value,
      unit = unit,
      source_row = row,
      source_column = value_column
    )
  })
  dplyr::bind_rows(rows)
}

current_series_value_rows <- function(record, sheet_name, sheet_data) {
  tibble::tibble(
    revision_id = record$revision_id,
    year = record$year,
    language = record$language,
    section_id = record$section_id,
    release_title = record$item,
    release_title_zh = record$item_zh,
    release_title_en = record$item_en,
    published_at = record$published_at,
    source_url = record$url,
    sheet = sheet_name,
    table = sheet_data$table,
    series = sheet_data$series,
    series_zh = sheet_data$series_zh,
    series_en = sheet_data$series_en,
    period = sheet_data$period,
    value = sheet_data$value,
    value_raw = sheet_data$value_raw,
    unit = sheet_data$unit,
    source_row = sheet_data$source_row,
    source_column = sheet_data$source_column
  )
}

refresh_current_series_data <- function(example_dir = NULL) {
  if (is.null(example_dir)) {
    command <- commandArgs(trailingOnly = FALSE)
    script_arg <- command[grepl("^--file=", command)]
    if (length(script_arg)) {
      example_dir <- dirname(normalizePath(
        sub("^--file=", "", script_arg[[1L]]),
        mustWork = TRUE
      ))
    } else {
      example_dir <- normalizePath("inst/examples", mustWork = TRUE)
    }
  }

  project_root <- normalizePath(
    file.path(example_dir, "..", ".."),
    mustWork = FALSE
  )
  if (file.exists(file.path(project_root, "DESCRIPTION"))) {
    pkgload::load_all(project_root, quiet = TRUE)
  } else {
    library(readPBOC)
  }

  available_years <- pboc_years("both")
  current_year <- max(available_years$year)
  catalog <- pboc_catalog(
    year = current_year,
    language = "zh",
    formats = c("xlsx", "xls")
  )

  is_statistical_table <- !grepl(
    "公布日程|Advance Release Calendar",
    catalog$item,
    ignore.case = TRUE
  )
  catalog <- catalog[is_statistical_table, , drop = FALSE]
  release_key <- paste(catalog$section_id, catalog$item, sep = "::")
  groups <- split(seq_len(nrow(catalog)), release_key)
  selected_rows <- vapply(groups, function(rows) {
    published <- as.numeric(catalog$published_at[rows])
    format_preference <- match(catalog$format[rows], c("xlsx", "xls"))
    rows[order(-published, format_preference, na.last = TRUE)][[1L]]
  }, integer(1))
  current_catalog <- catalog[selected_rows, , drop = FALSE]
  current_catalog <- current_catalog[
    order(current_catalog$section_id, current_catalog$item),
    ,
    drop = FALSE
  ]

  value_rows <- list()
  audit_rows <- vector("list", nrow(current_catalog))
  value_index <- 0L

  for (i in seq_len(nrow(current_catalog))) {
    record <- current_catalog[i, , drop = FALSE]
    message(
      sprintf(
        "[%d/%d] %s",
        i,
        nrow(current_catalog),
        record$item[[1L]]
      )
    )

    result <- tryCatch(
      suppressWarnings(pboc_read(record, sheet = NA, tidy = TRUE)),
      error = identity
    )

    if (inherits(result, "error")) {
      audit_rows[[i]] <- tibble::tibble(
        revision_id = record$revision_id,
        section_id = record$section_id,
        item = record$item,
        status = conditionMessage(result),
        workbook_sheets = NA_integer_,
        chartable_sheets = 0L,
        observations = 0L
      )
      next
    }

    if (!is.list(result) || inherits(result, "data.frame")) {
      result <- list(`Sheet 1` = result)
    }
    if (is.null(names(result))) {
      names(result) <- paste("Sheet", seq_along(result))
    }

    release_values <- list()
    release_value_index <- 0L
    for (sheet_name in names(result)) {
      sheet_data <- result[[sheet_name]]
      required <- c(
        "table",
        "series",
        "series_zh",
        "series_en",
        "period",
        "value",
        "value_raw",
        "unit",
        "source_row",
        "source_column"
      )
      if (!is.data.frame(sheet_data) ||
          !all(required %in% names(sheet_data)) ||
          !nrow(sheet_data)) {
        next
      }
      sheet_data <- sheet_data[
        !is.na(sheet_data$value),
        required,
        drop = FALSE
      ]
      if (!nrow(sheet_data)) next

      release_value_index <- release_value_index + 1L
      release_values[[release_value_index]] <- current_series_value_rows(
        record,
        sheet_name,
        sheet_data
      )
    }

    observations <- sum(vapply(
      release_values,
      nrow,
      integer(1)
    ))
    use_wide_layout <- observations == 0L

    if (use_wide_layout) {
      raw_result <- tryCatch(
        suppressWarnings(pboc_read(
          record,
          sheet = NA,
          tidy = FALSE,
          col_names = FALSE,
          col_types = "text",
          .name_repair = "minimal"
        )),
        error = identity
      )
      if (!inherits(raw_result, "error")) {
        if (!is.list(raw_result) || inherits(raw_result, "data.frame")) {
          raw_result <- list(`Sheet 1` = raw_result)
        }
        if (is.null(names(raw_result))) {
          names(raw_result) <- paste("Sheet", seq_along(raw_result))
        }

        fallback_values <- list()
        fallback_index <- 0L
        for (sheet_name in names(raw_result)) {
          sheet_data <- current_series_wide_sheet(
            raw_result[[sheet_name]],
            record,
            sheet_name
          )
          if (!nrow(sheet_data)) next
          fallback_index <- fallback_index + 1L
          fallback_values[[fallback_index]] <- current_series_value_rows(
            record,
            sheet_name,
            sheet_data
          )
        }
        fallback_observations <- sum(vapply(
          fallback_values,
          nrow,
          integer(1)
        ))
        if (fallback_observations > observations) {
          release_values <- fallback_values
          observations <- fallback_observations
          result <- raw_result
        }
      }
    }

    for (release_value in release_values) {
      value_index <- value_index + 1L
      value_rows[[value_index]] <- release_value
    }

    audit_rows[[i]] <- tibble::tibble(
      revision_id = record$revision_id,
      section_id = record$section_id,
      item = record$item,
      status = if (observations > 0L) "retrieved" else "empty_table",
      workbook_sheets = length(result),
      chartable_sheets = length(release_values),
      observations = observations
    )
  }

  values <- dplyr::bind_rows(value_rows)
  audit <- dplyr::bind_rows(audit_rows)
  data_dir <- file.path(example_dir, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(
    current_catalog,
    file.path(data_dir, "pboc-current-series-catalog.rds"),
    version = 3
  )
  saveRDS(
    values,
    file.path(data_dir, "pboc-current-series-values.rds"),
    version = 3
  )
  saveRDS(
    audit,
    file.path(data_dir, "pboc-current-series-audit.rds"),
    version = 3
  )
  utils::write.csv(
    current_catalog,
    file.path(data_dir, "pboc-current-series-catalog.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    values,
    file.path(data_dir, "pboc-current-series-values.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    audit,
    file.path(data_dir, "pboc-current-series-audit.csv"),
    row.names = FALSE,
    na = ""
  )

  invisible(list(
    catalog = current_catalog,
    values = values,
    audit = audit
  ))
}

if (sys.nframe() == 0L) {
  refresh_current_series_data()
}
