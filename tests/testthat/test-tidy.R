test_that("normalisation handles adjacent and in-cell bilingual labels", {
  cells <- data.frame(
    c1 = c(
      "货币供应量", "Money Supply", NA, "项目 Item",
      "货币和准货币（M2）", "Money & Quasi-money",
      "国外资产   Foreign Assets"
    ),
    c2 = c(NA, NA, "单位：亿元人民币", "2026.01", "100", NA, "200"),
    c3 = c(NA, NA, "Unit: 100 Million Yuan", "2026.02", "110", NA, "210"),
    check.names = FALSE
  )

  result <- pboc_tidy(cells)

  expect_equal(nrow(result), 4L)
  expect_equal(unique(result$series), c("Money & Quasi-money", "Foreign Assets"))
  expect_equal(unique(result$series_zh), c("货币和准货币（M2）", "国外资产"))
  expect_equal(unique(result$series_en), c("Money & Quasi-money", "Foreign Assets"))
  expect_equal(result$value, c(100, 110, 200, 210))
  expect_equal(result$source_row, c(5L, 5L, 7L, 7L))
})

test_that("normalisation detects revised-history subtables", {
  cells <- data.frame(
    c1 = c("项目", "M1", NA, "余额", "增速"),
    c2 = c("2025.01", "10", "2024.01", "9", "3%"),
    c3 = c("2025.02", "11", "2024.02", "8", "4%"),
    check.names = FALSE
  )

  result <- pboc_tidy(cells)

  expect_equal(unique(result$table), c(1L, 2L))
  expect_equal(result$value, c(10, 11, 9, 8, 3, 4))
  expect_equal(tail(result$value_raw, 2), c("3%", "4%"))
})

test_that("wide tables are preserved when no dated header exists", {
  cells <- data.frame(a = c("item", "M2"), b = c("value", "100"))
  expect_warning(result <- pboc_tidy(cells), "No dated header")
  expect_equal(names(result), c("column_1", "column_2"))
})

test_that("period-column XLS tables retain and canonicalize every month", {
  periods <- c(
    paste0("2018.0", 1:5),
    "2018.0599999999999",
    "2018.0699999999999",
    "2018.0799999999999",
    "2018.0899999999999",
    "2018.0999999999999",
    "2018.1099999999999",
    "2018.1199999999999"
  )
  cells <- data.frame(
    c1 = c(
      "中债国债收益率统计表",
      "Statistics of Chinabond Government Securities Yield",
      "单位：%",
      NA,
      NA,
      NA,
      periods
    ),
    c2 = c(
      NA,
      NA,
      NA,
      NA,
      "1年",
      "1-Year",
      as.character(seq_along(periods))
    ),
    c3 = c(
      NA,
      NA,
      "Unit: %",
      NA,
      "10年",
      "10-Year",
      as.character(seq_along(periods) + 100)
    ),
    check.names = FALSE
  )

  result <- pboc_tidy(cells)

  expect_equal(nrow(result), 24L)
  expect_equal(unique(result$series_en), c("1-Year", "10-Year"))
  expect_equal(
    unique(result$period),
    sprintf("2018.%02d", seq_len(12L))
  )
  expect_equal(result$source_row, rep(7:18, 2L))
  expect_equal(result$source_column, c(rep(2L, 12L), rep(3L, 12L)))
  expect_equal(result$value, c(seq_len(12L), seq_len(12L) + 100))
  expect_equal(unique(result$unit), "单位：% / Unit: %")
})

test_that("period columns outrank date-like decimal observations", {
  cells <- data.frame(
    period = c("Period", sprintf("2023.%02d", 1:12)),
    one_year = c(
      "1-Year",
      "2.1535", "2.3291", "2.2328", "2.1464", "1.9829", "1.8723",
      "1.8102", "1.9051", "2.1677", "2.2051", "2.3367", "2.0796"
    ),
    ten_year = c(
      "10-Year",
      "2.8977", "2.9017", "2.8528", "2.7788", "2.6850", "2.6351",
      "2.6597", "2.5575", "2.6751", "2.6938", "2.6680", "2.5553"
    ),
    check.names = FALSE
  )

  result <- pboc_tidy(cells)

  expect_equal(nrow(result), 24L)
  expect_equal(unique(result$series_en), c("1-Year", "10-Year"))
  expect_equal(unique(result$period), sprintf("2023.%02d", 1:12))
  expect_equal(result$source_column, c(rep(2L, 12L), rep(3L, 12L)))
})

test_that("numeric period recovery is bounded to valid Excel months", {
  expect_equal(
    normalize_period_label(c(
      "2018.0599999999999",
      "2018.0999999999999",
      "2018.1199999999999"
    )),
    c("2018.06", "2018.10", "2018.12")
  )
  expect_false(is_time_label("2018.9999999999999"))
  expect_equal(
    normalize_period_label("2018.9999999999999"),
    "2018.9999999999999"
  )
})
