# Bundled PBOC evidence snapshots

The RDS files in this directory are frozen, normalised evidence snapshots used
by readPBOC examples, vignettes, tests, and dashboards. They are installed with
the package so these materials can be rendered and checked without relying on
live network availability. Repository CSV copies support human review and are
excluded from the CRAN source tarball.

The current-series files contain a 2026 catalog snapshot, the corresponding
normalised worksheet values, and a retrieval audit. The release files contain
a bilingual release-index snapshot and a small release-text retrieval audit.
Every applicable row retains an official PBOC URL and publication or revision
identifier. These files are frozen evidence, not an automatically refreshed
cache or a guarantee of the latest PBOC publication.

## Money Supply chart example

The README and Getting Started chart select the following source from
`pboc-current-series-values.rds`:

- table: `货币供应量 Money Supply`;
- catalog asset: `zh/2026/money_and_banking/money-supply/NA/xlsx`;
- attachment revision: `2026071516481518348`;
- publication date: 15 July 2026;
- official index page:
  <https://www.pbc.gov.cn/diaochatongjisi/116219/116319/2026ntjsj/hbtjgl/index.html>;
- official XLSX attachment:
  <https://www.pbc.gov.cn/diaochatongjisi/attachDir/2026/07/2026071516481518348.xlsx>;
- plotted series: `Money` (M1) and `Money & Quasi-money` (M2); and
- plotted periods: January through June 2026.

The source reports values in RMB 100 million. The chart divides the normalised
numeric `value` column by 10,000 to express the levels in RMB trillion. Its
period conversion uses the package's canonical `YYYY.MM` labels, and the
source `value_raw`, `unit`, `source_row`, and `source_column` fields remain in
the snapshot for audit.

The snapshot was generated with the repository's
`refresh-pboc-current-series-data.R` workflow and package parsers. No separate
retrieval timestamp is asserted because one is not recorded in the installed
values snapshot.

## Rights and attribution

PBOC retains copyright in the source material. The maintainer-confirmed
redistribution permission, exact scope, and required attribution are recorded
in `inst/COPYRIGHTS`; the PBOC-derived snapshots are not public-domain or
MIT-licensed data. Preserve the attribution
“稿件来源：中国人民银行网站” / “Source: People's Bank of China website” and the
source metadata when redistributing them or figures derived from them.
