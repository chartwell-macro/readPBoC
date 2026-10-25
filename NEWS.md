# readPBOC 0.1.3

- Documented live and packaged series coverage, time ranges, sparse
  observations, and Chinese/English source differences.
- Expanded the package documentation with task-oriented reference topics,
  deterministic examples, getting-started and troubleshooting articles, and
  explicit network, output, and failure contracts.
- Added small author-created structural fixtures and offline tests for local
  reading, downloading, snapshotting, and public return contracts. These
  fixtures are not PBOC product data.
- Added release metadata, provenance and redistribution guidance, a pkgdown
  configuration, and GitHub Actions checks for R CMD check, coverage, and
  documentation builds.
- Recorded maintainer-confirmed permission to redistribute the bundled
  PBOC-derived materials with readPBOC, including through CRAN and its mirrors,
  while preserving PBOC ownership, provenance, and attribution.
- Added a prominent, reproducible Money Supply workflow that shows live
  discovery and reading, deterministic snapshot-backed filtering, and a
  rendered M1/M2 `ggplot2` chart on the homepage and in Getting Started.
- Standardised the public package author, maintainer, and citation display as
  Chartwell Macro, including the package's copyright-holder metadata.
- Replaced the personal creator email with a GitHub noreply address in required
  package metadata and removed email addresses from optional public surfaces.

# readPBOC 0.1.2

- Reserved for the separately published, downstream-pinned GitHub snapshot
  build. Version 0.1.3 continues from the default branch without changing its
  executable runtime code or bundled snapshot values.

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
