#' Discover years in the PBOC data archives
#'
#' Reads the official Chinese and/or English PBOC statistics indexes. The
#' Chinese archive is normally more current and extends further back.
#'
#' @param language One of `"zh"`, `"en"`, or `"both"`.
#' @return A tibble with one row per available year and language.
#' @export
#' @examples
#' \dontrun{
#' pboc_years("both")
#' }
pboc_years <- function(language = c("zh", "en", "both")) {
  language <- pboc_languages(match.arg(language))
  out <- lapply(language, function(lang) {
    url <- unname(.PBOC_DATA_INDEX[[lang]])
    parse_pboc_years(pboc_fetch_html(url), lang, url)
  })
  out <- bind_rows_base(out)
  if (nrow(out)) {
    out <- out[order(out$year, out$language, decreasing = TRUE), ,
               drop = FALSE]
  }
  class(out) <- c("pboc_resource_index", class(out))
  out
}

parse_pboc_years <- function(document, language, source_url) {
  anchors <- rvest::html_elements(document, "a[href]")
  title <- clean_text(rvest::html_text2(anchors))
  href <- rvest::html_attr(anchors, "href")

  if (identical(language, "zh")) {
    keep <- grepl(
      "^(19|20)[0-9]{2}\u5e74\u7edf\u8ba1\u6570\u636e",
      title
    )
    year <- suppressWarnings(as.integer(substr(title, 1L, 4L)))
  } else {
    keep <- grepl("^(19|20)[0-9]{2}$", title)
    year <- suppressWarnings(as.integer(title))
  }
  keep <- keep & !is.na(href) & !is.na(year)

  out <- tibble::tibble(
    year = year[keep],
    language = language,
    title = title[keep],
    url = absolute_url(href[keep], source_url),
    source_url = source_url
  )
  out <- distinct_rows(out, c("year", "language"))
  out[order(out$year, decreasing = TRUE), , drop = FALSE]
}

#' Discover sections for a PBOC data year
#'
#' @param year Four-digit statistics year.
#' @param language One of `"zh"`, `"en"`, or `"both"`.
#' @return A tibble of data sections.
#' @export
#' @examples
#' \dontrun{
#' pboc_sections(2025, "both")
#' }
pboc_sections <- function(year, language = c("zh", "en", "both")) {
  year <- as.integer(year)
  if (length(year) != 1L || is.na(year)) {
    stop("`year` must be one four-digit year.", call. = FALSE)
  }
  languages <- pboc_languages(match.arg(language))
  years <- pboc_years(if (length(languages) == 2L) "both" else languages)
  years <- years[years$year == year & years$language %in% languages, ,
                 drop = FALSE]
  if (!nrow(years)) {
    stop("The requested year is not listed for the selected language(s).",
         call. = FALSE)
  }

  out <- lapply(seq_len(nrow(years)), function(i) {
    record <- years[i, , drop = FALSE]
    parse_pboc_sections(
      pboc_fetch_html(record$url[[1L]]),
      record$language[[1L]],
      record$url[[1L]],
      record$year[[1L]]
    )
  })
  out <- bind_rows_base(out)
  class(out) <- c("pboc_resource_index", class(out))
  out
}

parse_pboc_sections <- function(document, language, source_url, year) {
  selector <- if (identical(language, "zh")) {
    "a.lan14cu, td.guidlevel01_style1 a"
  } else {
    ".leftNav .ListR a, .conRight .ListR a"
  }
  anchors <- rvest::html_elements(document, selector)

  if (!length(anchors)) {
    anchors <- rvest::html_elements(document, "a[href]")
  }
  title <- clean_text(rvest::html_text2(anchors))
  href <- rvest::html_attr(anchors, "href")
  section_id <- section_id_from_title(title)
  keep <- !is.na(section_id) & !is.na(href) & nzchar(href)

  bilingual <- split_bilingual(title[keep])
  out <- tibble::tibble(
    year = as.integer(year),
    language = language,
    section_id = section_id[keep],
    section = title[keep],
    section_zh = bilingual$zh,
    section_en = bilingual$en,
    url = absolute_url(href[keep], source_url),
    source_url = source_url
  )
  distinct_rows(out, c("year", "language", "section_id", "url"))
}

#' Build a catalog of PBOC data files
#'
#' The catalog follows the selected year into each statistics section and
#' returns direct PBOC attachment URLs. Chinese (`"zh"`) and English (`"en"`)
#' pages are independently scraped; use `"both"` to compare their coverage.
#'
#' @param year Four-digit statistics year.
#' @param language One of `"zh"`, `"en"`, or `"both"`.
#' @param section Optional regular expression matched against canonical section
#'   IDs and Chinese/English section titles.
#' @param formats File formats to retain, such as `c("xls", "xlsx")`. Use
#'   `NULL` to retain spreadsheets, HTML, and PDF links.
#' @return A `pboc_resource_index` tibble. `asset_id` is stable across revised
#'   attachment URLs, while `revision_id` identifies the published file.
#' @export
#' @examples
#' \dontrun{
#' pboc_catalog(2025, "both", section = "money", formats = c("xls", "xlsx"))
#' }
pboc_catalog <- function(year,
                         language = c("zh", "en", "both"),
                         section = NULL,
                         formats = c("xls", "xlsx")) {
  language <- match.arg(language)
  sections <- pboc_sections(year, language)
  sections <- query_filter(
    sections, section,
    c("section_id", "section", "section_zh", "section_en")
  )
  if (!nrow(sections)) {
    stop("No PBOC data sections matched `section`.", call. = FALSE)
  }

  out <- lapply(seq_len(nrow(sections)), function(i) {
    record <- sections[i, , drop = FALSE]
    parse_pboc_files(pboc_fetch_html(record$url[[1L]]), record)
  })
  out <- bind_rows_base(out)

  if (!is.null(formats) && nrow(out)) {
    formats <- tolower(formats)
    formats[formats == "htm"] <- "html"
    out <- out[out$format %in% formats, , drop = FALSE]
  }
  if (nrow(out)) {
    out <- out[order(out$language, out$section_id, out$item, out$period,
                     out$format, na.last = TRUE), , drop = FALSE]
  }
  class(out) <- c("pboc_resource_index", class(out))
  out
}

parse_pboc_files <- function(document, section_record) {
  source_url <- record_value(section_record, "url")
  links <- rvest::html_elements(document, "a[href]")
  href <- rvest::html_attr(links, "href")
  keep <- !is.na(href) &
    grepl("/attachDir/", href, ignore.case = TRUE) &
    grepl("\\.(xlsx?|pdf|html?)(?:[?#].*)?$", href,
          ignore.case = TRUE, perl = TRUE)
  links <- links[keep]
  href <- href[keep]
  if (!length(links)) return(tibble::tibble())

  link_label <- clean_text(rvest::html_text2(links))
  item <- vapply(seq_along(links), function(i) {
    row <- rvest::html_element(links[[i]], xpath = "ancestor::tr[1]")
    value <- item_from_container(row)
    if (!nzchar(value)) {
      table <- rvest::html_element(links[[i]], xpath = "ancestor::table[1]")
      value <- item_from_container(table)
    }
    value
  }, character(1))

  # Month links often sit in later rows of the same table. Carry the table
  # title forward when that row contains no descriptive cell.
  for (i in seq_along(item)) {
    if (nzchar(item[[i]])) next
    table <- rvest::html_element(links[[i]], xpath = "ancestor::table[1]")
    item[[i]] <- item_from_container(table)
  }
  item[!nzchar(item)] <- record_value(section_record, "section")

  url <- absolute_url(href, source_url)
  format <- extension_from_url(url)
  is_month <- grepl("^(0?[1-9]|1[0-2])$", link_label)
  period <- rep(NA_character_, length(link_label))
  period[is_month] <- sprintf("%02d", as.integer(link_label[is_month]))
  bilingual <- split_bilingual(item)
  section_id <- record_value(section_record, "section_id")
  year <- as.integer(record_value(section_record, "year"))
  language <- record_value(section_record, "language")
  asset_id <- paste(
    language, year, section_id, slugify(item), period, format,
    sep = "/"
  )

  out <- tibble::tibble(
    asset_id = asset_id,
    revision_id = revision_from_url(url),
    year = year,
    language = language,
    section_id = section_id,
    section = record_value(section_record, "section"),
    item = clean_text(item),
    item_zh = bilingual$zh,
    item_en = bilingual$en,
    period = period,
    format = format,
    published_at = attachment_date(url),
    url = url,
    source_url = source_url
  )
  distinct_rows(out, c("asset_id", "revision_id", "url"))
}

item_from_container <- function(node) {
  if (inherits(node, "xml_missing") || length(node) == 0L) return("")
  cells <- rvest::html_elements(node, "td")
  if (!length(cells)) return("")
  values <- clean_text(rvest::html_text2(cells))
  is_label <- nzchar(values) &
    !grepl("^(htm|html|xls|xlsx|pdf|0?[1-9]|1[0-2])$", values,
           ignore.case = TRUE)
  values <- values[is_label]
  if (!length(values)) "" else values[[1L]]
}
