# readPBOC

`readPBOC` discovers, downloads, reads, and normalises official statistical
data from the People’s Bank of China (PBOC). It covers both official websites:

- the Chinese statistics archive, which provides the broadest and most current
  collection;
- the English data archive, which provides official English tables and
  statistical releases.

The catalog-driven design follows official year and section indexes, records
direct attachment URLs, and preserves each PBOC attachment identifier. This
structure supports reproducible retrieval, bilingual analysis, and durable
revision histories.

## Installation

Install the package from a local checkout:

```r
install.packages(".", repos = NULL, type = "source")
```

Install the built source package:

```r
install.packages(
  "readPBOC_0.1.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

Load the package:

```r
library(readPBOC)
```

## Quick start

The following example discovers the current Chinese money-and-banking
workbooks and returns the money-supply observations in tidy form:

```r
library(readPBOC)

money_supply <- pboc_data(
  year = 2026,
  section = "货币统计|money",
  item = "货币供应量|Money Supply",
  language = "auto",
  tidy = TRUE
)

head(money_supply)
```

`language = "auto"` selects the Chinese archive first and then selects the
English archive. This ordering combines current coverage with automatic
secondary selection from the English archive.

## Discover available years

`pboc_years()` reads the official archive indexes:

```r
chinese_years <- pboc_years("zh")
english_years <- pboc_years("en")
all_years <- pboc_years("both")

head(chinese_years)
head(english_years)
```

Each result contains:

- `year`;
- `language`;
- the official archive title;
- the year-page URL;
- the source index URL.

## Discover sections within a year

`pboc_sections()` returns canonical section identifiers alongside the official
Chinese and English titles:

```r
sections <- pboc_sections(
  year = 2025,
  language = "both"
)

sections[, c(
  "year",
  "language",
  "section_id",
  "section_zh",
  "section_en",
  "url"
)]
```

Canonical section identifiers include:

- `aggregate_financing`;
- `money_and_banking`;
- `balance_sheets`;
- `credit_funds`;
- `financial_markets`;
- `financial_accounts`;
- `cgpi`;
- `surveys`.

These identifiers provide stable filters across both website languages.

## Build a file catalog

`pboc_catalog()` follows each selected section and returns direct attachment
URLs:

```r
money_catalog <- pboc_catalog(
  year = 2025,
  language = "both",
  section = "money",
  formats = c("xls", "xlsx")
)

money_catalog[, c(
  "language",
  "section_id",
  "item",
  "period",
  "format",
  "published_at",
  "url"
)]
```

Use a regular expression to select one or more sections:

```r
core_catalog <- pboc_catalog(
  year = 2025,
  language = "zh",
  section = "money|aggregate|credit",
  formats = c("xls", "xlsx")
)
```

Use `formats = NULL` to catalog spreadsheets, HTML tables, and PDFs together:

```r
complete_catalog <- pboc_catalog(
  year = 2025,
  language = "both",
  formats = NULL
)

table(complete_catalog$language, complete_catalog$format)
```

Every catalog row includes two revision fields:

- `asset_id` identifies the conceptual table across publication revisions;
- `revision_id` identifies the specific PBOC attachment.

## Retrieve one table with `pboc_data()`

`pboc_data()` combines discovery, selection, download, reading, and
normalisation.

Retrieve the current Chinese money-supply table:

```r
zh_money <- pboc_data(
  year = 2026,
  section = "money_and_banking|货币统计",
  item = "Money Supply|货币供应量",
  language = "zh",
  tidy = TRUE
)
```

Retrieve the official English money-supply table:

```r
en_money <- pboc_data(
  year = 2025,
  section = "money_and_banking|Money and Banking",
  item = "Money Supply",
  language = "en",
  tidy = TRUE
)
```

Select another official table by title:

```r
monetary_authority <- pboc_data(
  year = 2026,
  section = "money",
  item = "Balance Sheet of Monetary Authority|货币当局资产负债表",
  language = "auto",
  tidy = TRUE
)
```

Precise `section` and `item` expressions create deterministic selections when
a section contains several workbooks.

## Read catalog records directly

`pboc_read()` accepts a catalog row, a direct URL, or a local file path.

Select one catalog record:

```r
money_record <- money_catalog[
  grepl("Money Supply", money_catalog$item) &
    money_catalog$format == "xlsx",
  ,
  drop = FALSE
][1, ]
```

Read the original worksheet geometry:

```r
raw_money <- pboc_read(
  money_record,
  sheet = 1,
  tidy = FALSE
)

raw_money[1:12, 1:8]
```

Read the same worksheet in normalised long form:

```r
tidy_money <- pboc_read(
  money_record,
  sheet = 1,
  tidy = TRUE
)

head(tidy_money)
```

Read every populated sheet:

```r
all_sheets <- pboc_read(
  money_record,
  sheet = NA,
  tidy = FALSE
)

names(all_sheets)
```

Read an already-downloaded workbook:

```r
local_money <- pboc_read(
  "data-raw/pboc/money-supply-2025111913535649624.xlsx",
  tidy = TRUE
)
```

## Understand the tidy output

`pboc_tidy()` detects dated header rows and returns observations in long form.
The normaliser supports:

- Chinese and English labels in the same cell;
- Chinese and English labels on adjacent rows;
- multiple date blocks within one worksheet;
- revised historical subtables;
- numeric values, percentage values, and original cell text;
- source coordinates for workbook-level auditing.

The output columns are:

```r
names(tidy_money)
```

| Column | Meaning |
|---|---|
| `table` | Detected subtable number |
| `series` | Preferred display label |
| `series_zh` | Chinese series label |
| `series_en` | English series label |
| `period` | Workbook period label |
| `value` | Parsed numeric value |
| `value_raw` | Original workbook value |
| `unit` | Unit text reported by the workbook |
| `source_row` | Original worksheet row |
| `source_column` | Original worksheet column |

Workbook notes remain available as an attribute:

```r
attr(tidy_money, "notes")
```

The source URL remains attached to remote reads, and the source path remains
attached to local reads:

```r
attr(tidy_money, "source_url")
attr(local_money, "source_path")
```

## Read HTML data tables

Catalog HTML resources and read their tables:

```r
html_catalog <- pboc_catalog(
  year = 2025,
  language = "zh",
  section = "money",
  formats = "html"
)

html_table <- pboc_read(html_catalog[1, ])
```

The result contains one data frame for a single HTML table and a list for a
page containing several tables.

## Discover statistical statements

`pboc_releases()` retrieves release indexes and publication metadata.

Read the first Chinese release page:

```r
zh_releases <- pboc_releases(
  language = "zh",
  pages = 1
)

zh_releases[, c(
  "published_at",
  "category",
  "title",
  "url"
)]
```

Search Chinese financial-statistics and aggregate-financing statements:

```r
zh_financial_releases <- pboc_releases(
  language = "zh",
  pages = 1:2,
  query = "金融统计|社会融资"
)
```

Read selected English release categories:

```r
en_releases <- pboc_releases(
  language = "en",
  category = c(
    "financial_statistics",
    "aggregate_financing"
  ),
  pages = 1
)
```

Read every configured release feed in both languages:

```r
all_releases <- pboc_releases(
  language = "both",
  pages = "all"
)

table(all_releases$language, all_releases$category)
```

`pages = "all"` traverses every page reported by each official release index.

The Chinese `data_interpretation` feed brings financial-statistics,
aggregate-financing, regional, survey, and related statements into one
collection. The English site provides these categories:

- `general`;
- `financial_markets`;
- `financial_statistics`;
- `aggregate_financing`;
- `other`.

## Retrieve statement text

`pboc_read_release()` retrieves publication metadata, paragraph-level text,
complete text, and attachment links:

```r
statement <- pboc_read_release(
  zh_financial_releases[1, ]
)

statement
statement$title
statement$published_at
statement$language
statement$category
statement$paragraphs
statement$text
statement$attachments
statement$url
```

The `paragraphs` vector provides stable paragraph boundaries for search,
summarisation, translation, and text analysis. The `text` field provides a
complete plain-text version of the statement.

Search retrieved statement text:

```r
grep(
  "社会融资规模",
  statement$paragraphs,
  value = TRUE
)
```

Create a release corpus:

```r
selected_releases <- zh_financial_releases[1:5, ]

release_documents <- lapply(
  seq_len(nrow(selected_releases)),
  function(i) pboc_read_release(selected_releases[i, ])
)

release_corpus <- data.frame(
  title = vapply(release_documents, `[[`, character(1), "title"),
  published_at = as.Date(vapply(
    release_documents,
    function(x) as.character(x$published_at),
    character(1)
  )),
  language = vapply(release_documents, `[[`, character(1), "language"),
  text = vapply(release_documents, `[[`, character(1), "text"),
  stringsAsFactors = FALSE
)
```

Download statement attachments:

```r
statement_files <- pboc_download(
  statement$attachments,
  dest_dir = "data-raw/pboc-statements"
)
```

## Download and preserve official files

`pboc_download()` stores each file with its PBOC attachment identifier:

```r
downloaded <- pboc_download(
  money_catalog,
  dest_dir = "data-raw/pboc"
)

downloaded[, c(
  "revision_id",
  "status",
  "published_at",
  "path",
  "size",
  "md5"
)]
```

The default manifest path is:

```text
data-raw/pboc/readPBOC-manifest.csv
```

The manifest records:

- stable asset and revision identifiers;
- retrieval status and timestamp;
- publication date;
- official URL;
- local path;
- file size;
- MD5 checksum.

Each revision receives a distinct filename, so the download directory retains
the complete publication history.

Use a custom manifest:

```r
downloaded <- pboc_download(
  money_catalog,
  dest_dir = "data-raw/pboc",
  manifest = "manifests/pboc-downloads.csv"
)
```

## Track catalog revisions

Save a catalog snapshot:

```r
old_catalog <- pboc_catalog(
  year = 2025,
  language = "zh",
  formats = c("xls", "xlsx")
)

pboc_snapshot(
  old_catalog,
  "snapshots/pboc-2025-previous.csv"
)
```

Create a current snapshot and compare it:

```r
current_catalog <- pboc_catalog(
  year = 2025,
  language = "zh",
  formats = c("xls", "xlsx")
)

changes <- pboc_revisions(
  "snapshots/pboc-2025-previous.csv",
  current_catalog
)

table(changes$status)
changes[changes$status == "revised", ]
```

`pboc_revisions()` assigns one of four statuses:

- `new` marks a newly listed asset;
- `revised` marks a new attachment for an existing asset;
- `unchanged` marks a retained revision;
- `removed` marks a formerly listed asset that leaves the current catalog.

## End-to-end reproducible collection

The following workflow collects Chinese money-and-banking workbooks,
normalises money supply, stores release text, downloads official files, and
saves a revision snapshot:

```r
library(readPBOC)

dir.create("data-raw/pboc", recursive = TRUE, showWarnings = FALSE)
dir.create("data-processed", recursive = TRUE, showWarnings = FALSE)
dir.create("snapshots", recursive = TRUE, showWarnings = FALSE)

catalog <- pboc_catalog(
  year = 2026,
  language = "zh",
  section = "money",
  formats = c("xls", "xlsx")
)

pboc_download(
  catalog,
  dest_dir = "data-raw/pboc"
)

money_supply <- pboc_data(
  year = 2026,
  section = "money",
  item = "Money Supply|货币供应量",
  language = "zh",
  tidy = TRUE
)

write.csv(
  money_supply,
  "data-processed/pboc-money-supply-2026.csv",
  row.names = FALSE
)

releases <- pboc_releases(
  language = "zh",
  pages = 1,
  query = "金融统计|社会融资"
)

latest_statement <- pboc_read_release(releases[1, ])

writeLines(
  latest_statement$text,
  "data-processed/latest-pboc-statement.txt",
  useBytes = TRUE
)

pboc_snapshot(
  catalog,
  "snapshots/pboc-money-2026.csv"
)
```

## Configure requests

Set a longer request timeout:

```r
options(readPBOC.timeout = 120)
```

Set a project-specific user agent:

```r
options(
  readPBOC.user_agent =
    "my-research-project/1.0 (contact: analyst@example.com)"
)
```

The request layer follows redirects, identifies the package, and retries
transient server responses.

## Render the complete release-coverage dashboard

The package ships with a self-contained R Markdown dashboard built with
`ggplot2`. The included 23 July 2026 snapshot contains every release returned
by every page of the six configured PBOC release feeds:

- 1,432 unique Chinese and English releases;
- 108 official index pages;
- 740 Chinese data-interpretation statements;
- 692 English statements across aggregate financing, financial markets,
  financial statistics, general releases, and other statistics;
- publication dates from 17 April 2001 through 15 July 2026;
- complete identifiers, titles, dates, release URLs, and source-page URLs for
  every row.

Open the rendered dashboard:

```text
inst/examples/pboc-release-coverage-dashboard.html
```

Render the included snapshot:

```r
rmarkdown::render(
  "inst/examples/pboc-release-coverage-dashboard.Rmd"
)
```

Refresh every official index page, retrieve the earliest and latest statement
from each feed, and render the current evidence:

```r
rmarkdown::render(
  "inst/examples/pboc-release-coverage-dashboard.Rmd",
  params = list(
    refresh_index = TRUE,
    refresh_text_audit = TRUE
  )
)
```

The dashboard presents annual and cumulative release histories, monthly
publication cadence, feed-level coverage, a year-by-feed heatmap, metadata
completeness, statement-text extraction results, and a searchable table
containing every release.

## Chart every current statistical release

The current-values dashboard selects the latest official workbook for every
distinct statistical table in the comprehensive Chinese archive. The included
snapshot covers all 26 current releases and contains 3,095 reported values
across five statistical sections:

- two aggregate-financing releases;
- one corporate-goods-price release;
- nine credit-fund releases;
- six financial-market releases;
- eight money-and-banking releases.

Each release receives a dedicated tab, an official-source link, a chart of its
major series, and a table of latest values. The dashboard presents line charts
for monthly series and horizontal value charts for point-in-time releases.

Open the rendered dashboard:

```text
inst/examples/pboc-current-series-dashboard.html
```

Render the included values snapshot:

```r
rmarkdown::render(
  "inst/examples/pboc-current-series-dashboard.Rmd"
)
```

Refresh the official catalog, download the latest workbook for every current
statistical table, normalise the reported values, and render:

```r
rmarkdown::render(
  "inst/examples/pboc-current-series-dashboard.Rmd",
  params = list(refresh_data = TRUE)
)
```

Run the data-refresh script independently:

```bash
Rscript inst/examples/refresh-pboc-current-series-data.R
```

The refresh workflow stores a complete audit record:

```r
audit <- readRDS(
  "inst/examples/data/pboc-current-series-audit.rds"
)

table(audit$status)
sum(audit$observations)
```

## Test the package

Run the offline unit tests:

```bash
Rscript -e 'testthat::test_local(reporter = "summary")'
```

Run the complete live integration suite against both PBOC websites:

```bash
NOT_CRAN=true READPBOC_LIVE_TESTS=true \
Rscript -e 'testthat::test_local(reporter = "summary")'
```

Build the source package:

```bash
R CMD build .
```

Run the CRAN-style code, documentation, example, and test checks:

```bash
R CMD check --as-cran --no-manual readPBOC_0.1.0.tar.gz
```

Run the full PDF-manual check in an R environment with LaTeX and `makeindex`:

```bash
R CMD check --as-cran readPBOC_0.1.0.tar.gz
```

## Package scope

`readPBOC` provides:

- official Chinese and English archive discovery;
- XLS/XLSX and HTML table reading;
- bilingual worksheet normalisation;
- statistical-release indexing and text extraction;
- official PDF attachment cataloging and download;
- revision-safe filenames and checksum manifests;
- catalog snapshots and revision comparisons;
- offline unit tests and live integration tests.

Spreadsheets and HTML tables enter the normalisation workflow. PDF files remain
preserved as official source artifacts through `pboc_download()`.
