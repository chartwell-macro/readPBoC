test_that("year indexes are parsed in Chinese and English", {
  zh <- xml2::read_html('
    <html><body>
      <a href="/stats/2026/index.html">2026年统计数据</a>
      <a href="/stats/2025/index.html">2025年统计数据</a>
      <a href="/stats/2026/index.html">2026年统计数据</a>
    </body></html>
  ')
  en <- xml2::read_html('
    <html><body>
      <a href="/en/data/2025/index.html">2025</a>
      <a href="/en/data/2024/index.html">2024</a>
    </body></html>
  ')

  zh_result <- readPBOC:::parse_pboc_years(
    zh, "zh", "https://www.pbc.gov.cn/index.html"
  )
  en_result <- readPBOC:::parse_pboc_years(
    en, "en", "https://www.pbc.gov.cn/en/index.html"
  )

  expect_equal(zh_result$year, c(2026L, 2025L))
  expect_equal(en_result$year, c(2025L, 2024L))
  expect_equal(zh_result$language, c("zh", "zh"))
  expect_match(en_result$url[[1]], "^https://www.pbc.gov.cn/en/data/")
})

test_that("section indexes retain bilingual titles and canonical IDs", {
  document <- xml2::read_html('
    <html><body>
      <a class="lan14cu" href="/stats/2026/money/index.html">
        货币统计概览 Money and Banking Statistics
      </a>
      <a class="lan14cu" href="/stats/2026/afre/index.html">
        社会融资规模 Aggregate Financing to the Real Economy
      </a>
    </body></html>
  ')

  result <- readPBOC:::parse_pboc_sections(
    document, "zh", "https://www.pbc.gov.cn/stats/2026/index.html", 2026
  )

  expect_equal(
    result$section_id,
    c("money_and_banking", "aggregate_financing")
  )
  expect_equal(result$section_zh[[1]], "货币统计概览")
  expect_equal(result$section_en[[1]], "Money and Banking Statistics")
})

test_that("attachment rows become stable assets and distinct revisions", {
  document <- xml2::read_html('
    <html><body>
      <table class="a2015">
        <tr>
          <td>货币供应量<br/>Money Supply</td>
          <td><a href="/diaochatongjisi/attachDir/2026/07/202607151.xlsx">xls</a></td>
          <td><a href="/diaochatongjisi/attachDir/2026/07/202607152.pdf">pdf</a></td>
        </tr>
      </table>
      <table class="a2015">
        <tr><td>国际储备与外币流动性数据模板<br/>International Reserves</td></tr>
        <tr>
          <td></td>
          <td><a href="/diaochatongjisi/attachDir/2026/02/202602011.xls">01</a></td>
          <td><a href="/diaochatongjisi/attachDir/2026/03/202603011.xls">02</a></td>
        </tr>
      </table>
    </body></html>
  ')
  section <- tibble::tibble(
    year = 2026L,
    language = "zh",
    section_id = "money_and_banking",
    section = "货币统计概览 Money and Banking Statistics",
    url = "https://www.pbc.gov.cn/stats/2026/money/index.html"
  )

  result <- readPBOC:::parse_pboc_files(document, section)

  expect_equal(nrow(result), 4L)
  expect_equal(result$format, c("xlsx", "pdf", "xls", "xls"))
  expect_equal(result$period, c(NA, NA, "01", "02"))
  expect_equal(as.character(result$published_at[[1]]), "2026-07-15")
  expect_equal(result$item_zh[[1]], "货币供应量")
  expect_equal(result$item_en[[1]], "Money Supply")
  expect_false(anyDuplicated(result$asset_id) > 0L)
})

