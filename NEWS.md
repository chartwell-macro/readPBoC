# readPBOC 0.1.2

- Preserve ordinary leading letters when generating the bundled current-series
  snapshot; only explicit `A.`/`A)`-style enumerators are removed.

# readPBOC 0.1.1

- Parse period-down-rows financial-market workbooks directly in `pboc_tidy()`.
- Recover valid monthly labels exposed by older XLS files as binary
  floating-point text, such as `2018.0599999999999`, and return canonical
  `YYYY.MM` periods without dropping June through December.

# readPBOC 0.1.0

- Initial Chinese and English PBOC data discovery.
- Direct XLS/XLSX, HTML, and PDF attachment cataloging.
- Bilingual, multi-table worksheet normalisation.
- Chinese and English statistical-release discovery and text extraction.
- Complete release-index traversal with `pboc_releases(pages = "all")`.
- A self-contained `ggplot2` release-coverage dashboard and frozen bilingual
  evidence snapshot.
- A current-values dashboard with a dedicated tab and major-series chart for
  every current PBOC statistical table.
- Revision-safe downloads, checksummed manifests, and catalog comparisons.
