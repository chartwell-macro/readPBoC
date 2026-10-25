test_that("discovery wrappers return stable public resource-index contracts", {
  zh_document <- xml2::read_html(
    paste0(
      "<html><body>",
      "<a href='/stats/2026/index.html'>2026年统计数据</a>",
      "<a href='/stats/2025/index.html'>2025年统计数据</a>",
      "<a href='/stats/2026/index.html'>2026年统计数据</a>",
      "</body></html>"
    )
  )
  en_document <- xml2::read_html(
    paste0(
      "<html><body>",
      "<a href='/en/data/2025/index.html'>2025</a>",
      "<a href='/en/data/2024/index.html'>2024</a>",
      "</body></html>"
    )
  )
  fetched <- character()
  local_mocked_bindings(
    pboc_fetch_html = function(url) {
      fetched <<- c(fetched, url)
      if (grepl("/en/", url, fixed = TRUE)) en_document else zh_document
    },
    .package = "readPBOC"
  )

  result <- pboc_years("both")

  expect_named(
    result,
    c("year", "language", "title", "url", "source_url"),
    ignore.order = FALSE
  )
  expect_s3_class(result, "pboc_resource_index")
  expect_s3_class(result, "tbl_df")
  expect_type(result$year, "integer")
  expect_equal(result$year, c(2026L, 2025L, 2025L, 2024L))
  expect_equal(result$language, c("zh", "zh", "en", "en"))
  expect_equal(
    fetched,
    unname(readPBOC:::.PBOC_DATA_INDEX[c("zh", "en")])
  )
  expect_true(all(grepl("^https://", result$url)))
  expect_true(all(grepl("^https://", result$source_url)))
})

test_that("section discovery filters years and preserves bilingual schema", {
  years <- tibble::tibble(
    year = c(2026L, 2026L),
    language = c("zh", "en"),
    title = c("2026年统计数据", "2026"),
    url = c(
      "https://fixtures.test/zh/2026/index.html",
      "https://fixtures.test/en/2026/index.html"
    ),
    source_url = c(
      "https://fixtures.test/zh/index.html",
      "https://fixtures.test/en/index.html"
    )
  )
  class(years) <- c("pboc_resource_index", class(years))
  zh_sections <- xml2::read_html(paste0(
    "<html><body>",
    "<a class='lan14cu' href='/zh/2026/money/index.html'>",
    "货币统计概览 Money and Banking Statistics</a>",
    "<a class='lan14cu' href='/zh/2026/af/index.html'>",
    "社会融资规模 Aggregate Financing to the Real Economy</a>",
    "</body></html>"
  ))
  en_sections <- xml2::read_html(paste0(
    "<html><body><div class='leftNav'><div class='ListR'>",
    "<a href='/en/2026/money/index.html'>Money and Banking Statistics</a>",
    "</div></div></body></html>"
  ))
  fetched <- character()
  local_mocked_bindings(
    pboc_years = function(language = c("zh", "en", "both")) years,
    pboc_fetch_html = function(url) {
      fetched <<- c(fetched, url)
      if (grepl("/en/", url, fixed = TRUE)) en_sections else zh_sections
    },
    .package = "readPBOC"
  )

  result <- pboc_sections(2026, "both")

  expect_named(
    result,
    c(
      "year", "language", "section_id", "section", "section_zh",
      "section_en", "url", "source_url"
    ),
    ignore.order = FALSE
  )
  expect_s3_class(result, "pboc_resource_index")
  expect_type(result$year, "integer")
  expect_equal(result$language, c("zh", "zh", "en"))
  expect_equal(
    result$section_id,
    c("money_and_banking", "aggregate_financing", "money_and_banking")
  )
  expect_equal(result$section_zh[[1L]], "货币统计概览")
  expect_equal(result$section_en[[1L]], "Money and Banking Statistics")
  expect_equal(length(fetched), 2L)
  expect_error(
    suppressWarnings(pboc_sections("not-a-year", "zh")),
    "`year` must be one four-digit year"
  )

  local_mocked_bindings(
    pboc_years = function(language = c("zh", "en", "both")) years[0, ],
    .package = "readPBOC"
  )
  expect_error(
    pboc_sections(1999, "zh"),
    "requested year is not listed"
  )
})

test_that("catalog discovery applies section and format filters", {
  sections <- tibble::tibble(
    year = c(2026L, 2026L),
    language = c("zh", "zh"),
    section_id = c("money_and_banking", "financial_accounts"),
    section = c(
      "货币统计概览 Money and Banking Statistics",
      "金融账户 Financial Accounts"
    ),
    section_zh = c("货币统计概览", "金融账户"),
    section_en = c("Money and Banking Statistics", "Financial Accounts"),
    url = c(
      "https://fixtures.test/zh/2026/money/index.html",
      "https://fixtures.test/zh/2026/accounts/index.html"
    ),
    source_url = c(
      "https://fixtures.test/zh/2026/index.html",
      "https://fixtures.test/zh/2026/index.html"
    )
  )
  class(sections) <- c("pboc_resource_index", class(sections))
  money_document <- xml2::read_html(paste0(
    "<html><body><table><tr>",
    "<td>货币供应量 Money Supply</td>",
    "<td><a href='/attachDir/2026/07/202607151.xlsx'>xlsx</a></td>",
    "<td><a href='/attachDir/2026/07/202607152.xls'>xls</a></td>",
    "<td><a href='/attachDir/2026/07/202607153.pdf'>pdf</a></td>",
    "</tr></table></body></html>"
  ))
  accounts_document <- xml2::read_html(paste0(
    "<html><body><table><tr>",
    "<td>金融账户 Financial Accounts</td>",
    "<td><a href='/attachDir/2026/06/202606011.xlsx'>xlsx</a></td>",
    "</tr></table></body></html>"
  ))
  fetched <- character()
  local_mocked_bindings(
    pboc_sections = function(year, language, ...) sections,
    pboc_fetch_html = function(url) {
      fetched <<- c(fetched, url)
      if (grepl("accounts", url, fixed = TRUE)) {
        accounts_document
      } else {
        money_document
      }
    },
    .package = "readPBOC"
  )

  result <- pboc_catalog(
    2026,
    "zh",
    section = "money",
    formats = c("xlsx", "xls")
  )

  expect_named(
    result,
    c(
      "asset_id", "revision_id", "year", "language", "section_id",
      "section", "item", "item_zh", "item_en", "period", "format",
      "published_at", "url", "source_url"
    ),
    ignore.order = FALSE
  )
  expect_s3_class(result, "pboc_resource_index")
  expect_equal(result$format, c("xls", "xlsx"))
  expect_type(result$published_at, "double")
  expect_s3_class(result$published_at, "Date")
  expect_equal(
    as.character(result$published_at),
    c("2026-07-15", "2026-07-15")
  )
  expect_equal(result$item_zh, c("货币供应量", "货币供应量"))
  expect_equal(result$item_en, c("Money Supply", "Money Supply"))
  expect_true(all(grepl("^https://fixtures.test/attachDir/", result$url)))
  expect_equal(length(fetched), 1L)

  all_formats <- pboc_catalog(2026, "zh", section = "money", formats = NULL)
  expect_equal(all_formats$format, c("pdf", "xls", "xlsx"))
  expect_error(
    pboc_catalog(2026, "zh", section = "does-not-match"),
    "No PBOC data sections matched"
  )
})

test_that("pboc_data searches Chinese first and prefers requested formats", {
  catalog <- tibble::tibble(
    asset_id = c("asset", "asset"),
    revision_id = c("202601011", "202602011"),
    item = c("Money Supply", "Money Supply"),
    item_zh = c("货币供应量", "货币供应量"),
    item_en = c("Money Supply", "Money Supply"),
    format = c("xls", "xlsx"),
    published_at = as.Date(c("2026-01-01", "2026-02-01")),
    url = c(
      "https://fixtures.test/old.xls",
      "https://fixtures.test/new.xlsx"
    )
  )
  calls <- character()
  selected <- NULL
  local_mocked_bindings(
    pboc_catalog = function(year, language, section = NULL, formats = NULL) {
      calls <<- c(calls, language)
      catalog
    },
    pboc_read = function(x, sheet = 1, tidy = TRUE, ...) {
      selected <<- x
      tibble::tibble(chosen_format = x$format[[1L]])
    },
    .package = "readPBOC"
  )

  result <- pboc_data(
    2026,
    "money",
    item = "supply",
    language = "auto",
    formats = c("xlsx", "xls"),
    tidy = FALSE
  )

  expect_equal(calls, "zh")
  expect_equal(selected$format[[1L]], "xlsx")
  expect_equal(result$chosen_format, "xlsx")
})

test_that("pboc_data falls back to English only on a Chinese non-match", {
  english <- tibble::tibble(
    asset_id = "en-asset",
    revision_id = "202601011",
    item = "Money Supply",
    item_zh = NA_character_,
    item_en = "Money Supply",
    format = "xlsx",
    published_at = as.Date("2026-01-01"),
    url = "https://fixtures.test/en.xlsx"
  )
  calls <- character()
  local_mocked_bindings(
    pboc_catalog = function(year, language, section = NULL, formats = NULL) {
      calls <<- c(calls, language)
      if (identical(language, "zh")) {
        stop("no Chinese fixture", call. = FALSE)
      }
      english
    },
    pboc_read = function(x, sheet = 1, tidy = TRUE, ...) {
      tibble::tibble(chosen_language = "en", chosen_url = x$url[[1L]])
    },
    .package = "readPBOC"
  )

  result <- pboc_data(
    2026,
    "money",
    item = "supply",
    language = "auto",
    tidy = FALSE
  )

  expect_equal(calls, c("zh", "en"))
  expect_equal(result$chosen_language, "en")
  expect_equal(result$chosen_url, "https://fixtures.test/en.xlsx")
})

test_that("pboc_data rejects ambiguous and absent matches", {
  ambiguous <- tibble::tibble(
    asset_id = c("asset-1", "asset-2"),
    revision_id = c("r1", "r2"),
    item = c("Money Supply", "Interest Rates"),
    item_zh = c("货币供应量", "利率"),
    item_en = c("Money Supply", "Interest Rates"),
    format = c("xlsx", "xlsx"),
    published_at = as.Date(c("2026-01-01", "2026-01-02")),
    url = c("https://fixtures.test/a.xlsx", "https://fixtures.test/b.xlsx")
  )
  local_mocked_bindings(
    pboc_catalog = function(...) ambiguous,
    pboc_read = function(...) stop("reader should not be called", call. = FALSE),
    .package = "readPBOC"
  )
  expect_error(
    pboc_data(2026, "money", language = "zh", tidy = FALSE),
    "More than one PBOC item matched"
  )

  local_mocked_bindings(
    pboc_catalog = function(...) tibble::tibble(),
    .package = "readPBOC"
  )
  expect_error(
    pboc_data(2026, "money", item = "missing", language = "zh"),
    "No PBOC workbook matched"
  )
})

test_that("release discovery returns filtered dated release indexes", {
  document <- xml2::read_html(paste0(
    "<html><body><ul>",
    "<li><span>2026-07-15</span><a istitle='true'",
    "href='/en/releases/abc/index.html'",
    "title='Financial Statistics Report (H1 2026)'>Report</a></li>",
    "<li><span>2025-01-02</span><a istitle='true'",
    "href='/en/releases/old/index.html' title='Old Report'>Old</a></li>",
    "</ul></body></html>"
  ))
  fetched <- character()
  local_mocked_bindings(
    pboc_fetch_html = function(url) {
      fetched <<- c(fetched, url)
      document
    },
    .package = "readPBOC"
  )

  result <- pboc_releases(
    "en",
    category = "financial_statistics",
    pages = 1L,
    query = "H1"
  )

  expect_named(
    result,
    c(
      "release_id", "language", "category", "title", "published_at",
      "url", "source_url"
    ),
    ignore.order = FALSE
  )
  expect_s3_class(result, "pboc_release_index")
  expect_equal(nrow(result), 1L)
  expect_equal(result$release_id, "abc")
  expect_equal(result$language, "en")
  expect_equal(result$category, "financial_statistics")
  expect_equal(result$title, "Financial Statistics Report (H1 2026)")
  expect_s3_class(result$published_at, "Date")
  expect_equal(as.character(result$published_at), "2026-07-15")
  expect_equal(fetched, readPBOC:::.PBOC_RELEASE_INDEX$en[["financial_statistics"]])
  expect_error(
    pboc_releases("en", pages = 0L),
    "positive page numbers"
  )
  expect_error(
    pboc_releases("en", category = "not-a-category"),
    "No release categories matched"
  )
})

test_that("release readers expose metadata, paragraphs, attachments, and S3 output", {
  release_url <- "https://fixtures.test/en/releases/abc/index.html"
  document <- xml2::read_html(paste0(
    "<html><head>",
    "<meta name='ArticleTitle' content='Financial Statistics Report'>",
    "<meta name='PubDate' content='2026-07-15'>",
    "<meta name='ColumnName' content='Financial Statistics'>",
    "</head><body><div class='content'>",
    "<h3>Summary</h3><p>First paragraph.</p><p>Second paragraph.</p>",
    "<a href='/attachDir/2026/07/202607151.xlsx'>Workbook</a>",
    "<a href='/attachDir/2026/07/202607152.pdf'>Appendix</a>",
    "</div></body></html>"
  ))
  local_mocked_bindings(
    pboc_fetch_html = function(url) document,
    .package = "readPBOC"
  )
  input <- tibble::tibble(
    language = "en",
    url = release_url
  )

  result <- pboc_read_release(input)

  expect_named(
    result,
    c(
      "release_id", "language", "category", "title", "published_at",
      "text", "paragraphs", "attachments", "url"
    ),
    ignore.order = FALSE
  )
  expect_s3_class(result, "pboc_release")
  expect_equal(result$release_id, "abc")
  expect_equal(result$language, "en")
  expect_equal(result$category, "Financial Statistics")
  expect_equal(result$title, "Financial Statistics Report")
  expect_s3_class(result$published_at, "Date")
  expect_equal(as.character(result$published_at), "2026-07-15")
  expect_equal(result$paragraphs, c("Summary", "First paragraph.", "Second paragraph."))
  expect_equal(result$text, paste(result$paragraphs, collapse = "\n\n"))
  expect_named(result$attachments, c("title", "format", "url"), ignore.order = FALSE)
  expect_equal(result$attachments$format, c("xlsx", "pdf"))
  expect_equal(
    result$attachments$url,
    c(
      "https://fixtures.test/attachDir/2026/07/202607151.xlsx",
      "https://fixtures.test/attachDir/2026/07/202607152.pdf"
    )
  )
  expect_output(print(result), "<PBOC release>")
  expect_output(print(result), "Title:    Financial Statistics Report")
  printed <- capture.output(value <- withVisible(print(result)))
  expect_false(value$visible)
  expect_true(any(grepl("Text:     3 paragraph\\(s\\)", printed)))
})

test_that("all exported S3 print methods retain concise class headers", {
  resource_index <- tibble::tibble(year = 2026L)
  class(resource_index) <- c("pboc_resource_index", class(resource_index))
  release_index <- tibble::tibble(release_id = "abc")
  class(release_index) <- c("pboc_release_index", class(release_index))
  resource <- tibble::tibble(url = "https://fixtures.test/a.xlsx")
  class(resource) <- c("pboc_resource", class(resource))

  expect_output(
    print(resource_index),
    "<PBOC resource index: 1 rows>"
  )
  expect_output(
    print(release_index),
    "<PBOC release index: 1 rows>"
  )
  expect_output(print(resource), "https://fixtures.test/a.xlsx")
  printed <- capture.output(value <- withVisible(print(resource)))
  expect_false(value$visible)
  expect_true(any(grepl("https://fixtures.test/a.xlsx", printed, fixed = TRUE)))
})

test_that("pboc_tidy preserves empty, notes, and no-header contracts", {
  empty <- pboc_tidy(data.frame())
  expect_named(empty, character())
  expect_equal(nrow(empty), 0L)
  expect_s3_class(empty, "tbl_df")

  cells <- data.frame(
    label = c("Unit: %", "Item", "M1", "Note: revised values"),
    jan = c(NA, "2026.01", "100", NA),
    feb = c(NA, "2026.02", "110", NA),
    check.names = FALSE
  )
  result <- pboc_tidy(cells)
  expect_named(
    result,
    c(
      "table", "series", "series_zh", "series_en", "period", "value",
      "value_raw", "unit", "source_row", "source_column"
    ),
    ignore.order = FALSE
  )
  expect_equal(result$value, c(100, 110))
  expect_equal(result$period, c("2026.01", "2026.02"))
  expect_equal(attr(result, "notes"), "Note: revised values")
  expect_equal(unique(result$unit), "Unit: %")

  wide <- data.frame(
    first = c("item", "series", "M2"),
    second = c("value", "unit", "100")
  )
  expect_warning(
    fallback <- pboc_tidy(wide),
    "No dated header row was detected"
  )
  expect_named(fallback, c("column_1", "column_2"), ignore.order = FALSE)
  expect_equal(attr(fallback, "source_rows"), 1:3)
  expect_type(attr(fallback, "source_rows"), "integer")
  expect_null(attr(fallback, "notes"))
})
