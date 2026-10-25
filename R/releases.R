#' Discover PBOC statistical releases and statements
#'
#' The Chinese `data_interpretation` feed combines financial-statistics,
#' aggregate-financing, regional, survey, and other statements that are not
#' all translated. The English site separates releases into categories.
#'
#' @param language One of `"zh"`, `"en"`, or `"both"`.
#' @param category Optional category names. See `names(.PBOC_RELEASE_INDEX$en)`
#'   in the package source; common values are `"financial_statistics"` and
#'   `"aggregate_financing"`. `NULL` reads every category for the language.
#' @param pages Positive page numbers to retrieve, or `"all"` to traverse every
#'   page reported by each official release index.
#' @param query Optional regular expression matched against release titles.
#' @return A `pboc_release_index` tibble.
#' @export
#' @examples
#' \dontrun{
#' pboc_releases("both", pages = 1, query = "2026|2025")
#' pboc_releases("zh", pages = 1)
#' }
pboc_releases <- function(language = c("zh", "en", "both"),
                          category = NULL,
                          pages = 1L,
                          query = NULL) {
  languages <- pboc_languages(match.arg(language))
  retrieve_all_pages <- is.character(pages) &&
    length(pages) == 1L &&
    identical(tolower(pages), "all")
  if (!retrieve_all_pages) {
    pages <- unique(as.integer(pages))
    if (!length(pages) || anyNA(pages) || any(pages < 1L)) {
      stop("`pages` must contain positive page numbers or be \"all\".",
           call. = FALSE)
    }
  }

  jobs <- list()
  k <- 0L
  for (lang in languages) {
    indexes <- .PBOC_RELEASE_INDEX[[lang]]
    if (!is.null(category)) {
      selected <- intersect(names(indexes), category)
      if (!length(selected)) next
      indexes <- indexes[selected]
    }
    for (category_name in names(indexes)) {
      k <- k + 1L
      jobs[[k]] <- list(
        language = lang,
        category = category_name,
        url = unname(indexes[[category_name]])
      )
    }
  }
  if (!length(jobs)) {
    stop("No release categories matched `category`.", call. = FALSE)
  }

  out <- lapply(jobs, function(job) {
    first <- pboc_fetch_html(job$url)
    job_pages <- if (retrieve_all_pages) {
      seq_len(release_total_pages(first))
    } else {
      pages
    }
    results <- list()
    if (1L %in% job_pages) {
      results[[1L]] <- parse_pboc_release_index(
        first, job$language, job$category, job$url
      )
    }
    later <- setdiff(job_pages, 1L)
    if (length(later)) {
      template <- release_page_template(first, job$url)
      if (is.na(template)) {
        stop("Could not determine the pagination URL for ", job$url,
             call. = FALSE)
      }
      for (page in later) {
        page_url <- sub("%1", as.character(page), template, fixed = TRUE)
        results[[length(results) + 1L]] <- parse_pboc_release_index(
          pboc_fetch_html(page_url),
          job$language,
          job$category,
          page_url
        )
      }
    }
    bind_rows_base(results)
  })

  out <- bind_rows_base(out)
  out <- query_filter(out, query, "title")
  out <- distinct_rows(out, c("language", "url"))
  if (nrow(out)) {
    out <- out[order(out$published_at, decreasing = TRUE, na.last = TRUE),
               , drop = FALSE]
  }
  class(out) <- c("pboc_release_index", class(out))
  out
}

release_total_pages <- function(document) {
  hidden <- rvest::html_attr(
    rvest::html_elements(document, "input[totalpage]"),
    "totalpage"
  )
  visible <- rvest::html_text2(
    rvest::html_elements(document, ".totalPage")
  )
  candidates <- suppressWarnings(as.integer(c(hidden, visible)))
  candidates <- candidates[!is.na(candidates) & candidates >= 1L]
  if (length(candidates)) max(candidates) else 1L
}

parse_pboc_release_index <- function(document, language, category, source_url) {
  links <- rvest::html_elements(document, "a[istitle='true'][href]")
  if (!length(links)) return(tibble::tibble())

  title <- clean_text(rvest::html_attr(links, "title"))
  missing_title <- is.na(title) | !nzchar(title)
  title[missing_title] <- clean_text(rvest::html_text2(links[missing_title]))
  href <- rvest::html_attr(links, "href")
  published <- vapply(links, function(link) {
    container <- rvest::html_element(
      link, xpath = "ancestor::li[1] | ancestor::tr[1]"
    )
    if (inherits(container, "xml_missing") || !length(container)) {
      return(NA_character_)
    }
    text <- clean_text(rvest::html_text2(container))
    match <- regmatches(text, regexpr("(19|20)[0-9]{2}-[0-9]{2}-[0-9]{2}",
                                     text, perl = TRUE))
    if (!length(match)) NA_character_ else match[[1L]]
  }, character(1))
  url <- absolute_url(href, source_url)

  tibble::tibble(
    release_id = revision_from_release_url(url),
    language = language,
    category = category,
    title = title,
    published_at = as.Date(published),
    url = url,
    source_url = source_url
  )
}

revision_from_release_url <- function(url) {
  path <- strsplit(sub("[?#].*$", "", url), "/", fixed = TRUE)
  vapply(path, function(parts) {
    parts <- parts[nzchar(parts)]
    if (length(parts) >= 2L && utils::tail(parts, 1L) == "index.html") {
      parts[[length(parts) - 1L]]
    } else {
      sub("\\.[^.]+$", "", utils::tail(parts, 1L))
    }
  }, character(1))
}

release_page_template <- function(document, source_url) {
  nodes <- rvest::html_elements(
    document,
    "input[onkeydown*='%1'], [tagname*='-2.html']"
  )
  candidates <- c(
    rvest::html_attr(nodes, "onkeydown"),
    rvest::html_attr(nodes, "tagname")
  )
  candidates <- candidates[!is.na(candidates)]
  for (candidate in candidates) {
    start <- regexpr("/", candidate, fixed = TRUE)[[1L]]
    if (start < 1L) next
    tail <- substr(candidate, start, nchar(candidate))
    finish <- regexpr(".html", tail, fixed = TRUE)[[1L]]
    if (finish < 1L) next
    path <- substr(tail, 1L, finish + 4L)

    if (grepl("%1", path, fixed = TRUE)) {
      placeholder <- "READPBOCPAGENUMBER"
      resolved <- absolute_url(
        sub("%1", placeholder, path, fixed = TRUE),
        source_url
      )
      return(sub(placeholder, "%1", resolved, fixed = TRUE))
    }
    if (grepl("-2.html", path, fixed = TRUE)) {
      placeholder <- "READPBOCPAGENUMBER"
      template <- sub(
        "-2.html",
        paste0("-", placeholder, ".html"),
        path,
        fixed = TRUE
      )
      resolved <- absolute_url(template, source_url)
      return(sub(placeholder, "%1", resolved, fixed = TRUE))
    }
  }
  NA_character_
}

#' Read a PBOC statistical release
#'
#' @param x A release URL or one row returned by [pboc_releases()].
#' @return A `pboc_release` list containing metadata, paragraph-level text,
#'   complete text, and attachment links.
#' @export
#' @examples
#' \dontrun{
#' releases <- pboc_releases("en", category = "financial_statistics")
#' pboc_read_release(releases[1, ])
#' }
pboc_read_release <- function(x) {
  url <- resource_url(x)
  document <- pboc_fetch_html(url)

  meta <- function(name) {
    node <- rvest::html_element(
      document, paste0("meta[name='", name, "']")
    )
    clean_text(rvest::html_attr(node, "content"))
  }
  title <- meta("ArticleTitle")
  if (is.na(title) || !nzchar(title)) {
    title <- clean_text(rvest::html_text2(
      rvest::html_element(document, "h2, h1")
    ))
  }
  date <- suppressWarnings(as.Date(meta("PubDate")))
  category <- meta("ColumnName")
  content <- rvest::html_element(document, ".content")
  paragraphs <- rvest::html_elements(content, "p, h3, h4, li")
  paragraph_text <- clean_text(rvest::html_text2(paragraphs))
  paragraph_text <- paragraph_text[nzchar(paragraph_text)]
  if (!length(paragraph_text)) {
    paragraph_text <- clean_text(rvest::html_text2(content))
  }

  links <- rvest::html_elements(content, "a[href]")
  if (length(links)) {
    href <- rvest::html_attr(links, "href")
    attachments <- tibble::tibble(
      title = clean_text(rvest::html_text2(links)),
      format = extension_from_url(href),
      url = absolute_url(href, url)
    )
  } else {
    attachments <- tibble::tibble(
      title = character(),
      format = character(),
      url = character()
    )
  }

  language <- record_value(x, "language")
  if (is.na(language)) {
    language <- if (grepl("/en/", url, fixed = TRUE)) "en" else "zh"
  }
  out <- list(
    release_id = revision_from_release_url(url),
    language = language,
    category = category,
    title = title,
    published_at = date,
    text = paste(paragraph_text, collapse = "\n\n"),
    paragraphs = paragraph_text,
    attachments = attachments,
    url = url
  )
  class(out) <- "pboc_release"
  out
}
