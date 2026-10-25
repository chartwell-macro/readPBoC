#' Discover years in the PBOC statistical archives
#'
#' `pboc_years()` reads the official Chinese and/or English PBOC statistics
#' index and extracts the year links that it recognises. The Chinese archive is
#' normally more current and extends further back, while the English archive
#' may provide a different coverage set. A returned year is evidence of a
#' recognised year-labelled link, not a guarantee that any particular series,
#' section, period, or format exists in that language. The request is made when
#' the function is called; no index is cached by the package.
#'
#' @param language Character choice of `"zh"`, `"en"`, or `"both"`; the
#'   default is `"zh"`. `"both"` requests both indexes and returns one row per
#'   language/year pair.
#' @return A `pboc_resource_index` tibble with columns `year` (integer),
#'   `language` (`"zh"` or `"en"`), `title` (the archive link text), `url`
#'   (absolute HTTPS year-index URL), and `source_url` (the index URL used for
#'   the request), in that order. Rows are sorted by decreasing year and then
#'   decreasing language. Duplicate year/language pairs are removed.
#' @section Network and failure behaviour:
#' The official index is fetched over HTTPS with the package user agent,
#' `getOption("readPBOC.timeout", 60)` second timeout, and up to three attempts
#' for transient failures. A non-2xx response or unavailable PBOC endpoint
#' raises an error; the function does not write files or require credentials.
#' @family discovery
#' @seealso [pboc_sections()] to continue into one year and
#'   [pboc_catalog()] to list its attachments.
#' @export
#' @examples
#' if (interactive()) {
#'   pboc_years("both")
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

#' Discover statistical sections for a PBOC year
#'
#' `pboc_sections()` follows the year link returned by [pboc_years()] and
#' identifies the supported statistical sections whose titles map to the
#' package's canonical section IDs. Chinese and English pages are parsed
#' independently; use `language = "both"` to compare their coverage. The result
#' is an availability union, not a set of paired translations, and one language
#' can legitimately return fewer or no recognised sections for a year.
#'
#' @param year One four-digit statistics year. It is coerced to integer and an
#'   error is raised when it is missing or not listed for the selected
#'   language.
#' @param language Character choice of `"zh"`, `"en"`, or `"both"`; the
#'   default is `"zh"`.
#' @return A `pboc_resource_index` tibble with columns `year` (integer),
#'   `language`, `section_id` (canonical character ID), `section` (source
#'   title), `section_zh`, `section_en`, `url` (absolute section URL), and
#'   `source_url` (the year page used), in that order. Duplicate
#'   year/language/section/URL rows are removed.
#' @section Network and failure behaviour:
#' The relevant year index and section pages are fetched over HTTPS with the
#' package timeout and retry policy. A year that is not listed, an unavailable
#' page, or a non-2xx response raises an error. No files are written.
#' @family discovery
#' @seealso [pboc_years()] for archive coverage and [pboc_catalog()] for the
#'   attachment-level catalog.
#' @export
#' @examples
#' if (interactive()) {
#'   pboc_sections(2025, "both")
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

#' Build a revision-aware catalog of PBOC attachments
#'
#' `pboc_catalog()` follows a selected year into matching statistical sections
#' and parses direct attachment links from the official pages. Chinese (`"zh"`)
#' and English (`"en"`) pages are scraped independently; `"both"` returns both
#' coverage sets without asserting that rows are translations or have matching
#' periods, units, formats, or publication dates. The catalog includes
#' spreadsheet, HTML, and PDF attachment metadata when requested, but it does
#' not download the files.
#'
#' `asset_id` identifies the logical language/year/section/item/period/format
#' asset and remains stable when the official attachment URL is revised;
#' `revision_id` is derived from the published attachment name and distinguishes
#' those revisions. Month links are represented by a two-digit `period`; other
#' attachments have `NA` for that column.
#'
#' @param year One four-digit statistics year, passed to [pboc_sections()].
#' @param language Character choice of `"zh"`, `"en"`, or `"both"`; the
#'   default is `"zh"`.
#' @param section Optional regular expression matched case-insensitively
#'   against canonical section IDs and Chinese/English section titles. `NULL`
#'   (the default) includes every recognised section.
#' @param formats Character vector of formats to retain, with default
#'   `c("xls", "xlsx")`. Use `NULL` to retain spreadsheet, HTML, and PDF links;
#'   `"htm"` is normalised to `"html"`.
#' @return A `pboc_resource_index` tibble with these columns in order:
#'   `asset_id`, `revision_id`, `year`, `language`, `section_id`, `section`,
#'   `item`, `item_zh`, `item_en`, `period`, `format`, `published_at` (a Date
#'   parsed from the first eight digits of the revision when valid), `url`, and
#'   `source_url`. Rows are sorted by language, section ID, item, period, and
#'   format. Empty catalogs retain the resource-index class.
#' @section Network and failure behaviour:
#' Each year/section page is fetched over HTTPS with the package user agent,
#' timeout, and retry policy. A section filter that matches no section, an
#' unavailable page, or a non-2xx response raises an error. Cataloging has no
#' filesystem side effects and does not require credentials.
#' @family discovery
#' @seealso [pboc_sections()] for section metadata, [pboc_data()] for selecting
#'   and reading one asset, and [pboc_download()] for preserving files.
#' @export
#' @examples
#' if (interactive()) {
#'   pboc_catalog(
#'     2025, "both", section = "money", formats = c("xls", "xlsx")
#'   )
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
