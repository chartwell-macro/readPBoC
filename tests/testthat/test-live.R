test_that("live Chinese and English PBOC discovery works", {
  skip_on_cran()
  skip_if(Sys.getenv("READPBOC_LIVE_TESTS") != "true")

  zh <- pboc_catalog(
    as.integer(format(Sys.Date(), "%Y")),
    "zh",
    section = "money",
    formats = c("xls", "xlsx")
  )
  en <- pboc_catalog(
    2025,
    "en",
    section = "money",
    formats = c("xls", "xlsx")
  )
  en_releases <- pboc_releases(
    "en",
    category = "financial_statistics",
    pages = 1
  )
  zh_releases <- pboc_releases("zh", pages = 1)

  expect_gt(nrow(zh), 0L)
  expect_gt(nrow(en), 0L)
  expect_gt(nrow(en_releases), 0L)
  expect_gt(nrow(zh_releases), nrow(en_releases))
  expect_true(all(grepl("^https://www.pbc.gov.cn/", zh$url)))

  zh_money <- zh[grepl("货币供应量|Money Supply", zh$item), , drop = FALSE]
  en_money <- en[grepl("Money Supply", en$item), , drop = FALSE]
  expect_gt(nrow(zh_money), 0L)
  expect_gt(nrow(en_money), 0L)
  expect_gt(nrow(pboc_read(zh_money[1, ], tidy = TRUE)), 0L)
  expect_gt(nrow(pboc_read(en_money[1, ], tidy = TRUE)), 0L)

  expect_gt(nchar(pboc_read_release(zh_releases[1, ])$text), 0L)
  expect_gt(nchar(pboc_read_release(en_releases[1, ])$text), 0L)
})
