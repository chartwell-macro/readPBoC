test_that("release lists expose dates, languages, categories, and paging", {
  document <- xml2::read_html('
    <html><body>
      <ul class="prhhul">
        <li>
          <span class="prhhdata">2026-07-15</span>
          <a href="/en/releases/abc/index.html"
             title="Financial Statistics Report (H1 2026)"
             istitle="true">Financial Statistics Report</a>
        </li>
      </ul>
      <input onkeydown="jumpToPage(event,this,\'18\',\'1\',\'/en/releases/list-%1.html\')" />
    </body></html>
  ')

  result <- readPBOC:::parse_pboc_release_index(
    document,
    "en",
    "financial_statistics",
    "https://www.pbc.gov.cn/en/releases/index.html"
  )
  template <- readPBOC:::release_page_template(
    document,
    "https://www.pbc.gov.cn/en/releases/index.html"
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$release_id, "abc")
  expect_equal(as.character(result$published_at), "2026-07-15")
  expect_equal(result$category, "financial_statistics")
  expect_equal(
    template,
    "https://www.pbc.gov.cn/en/releases/list-%1.html"
  )
  expect_equal(readPBOC:::release_total_pages(document), 1L)
})

test_that("release indexes report their complete page count", {
  document <- xml2::read_html('
    <html><body>
      <span class="totalPage">18</span>
      <input totalpage="18" />
    </body></html>
  ')

  expect_equal(readPBOC:::release_total_pages(document), 18L)
})

test_that("release pagination supports numbered PBOC tagname links", {
  document <- xml2::read_html('
    <html><body>
      <a tagname="/releases/11871-2.html">Next</a>
      <input totalpage="37" />
    </body></html>
  ')

  expect_equal(
    readPBOC:::release_page_template(
      document,
      "https://www.pbc.gov.cn/releases/index.html"
    ),
    "https://www.pbc.gov.cn/releases/11871-%1.html"
  )
})
