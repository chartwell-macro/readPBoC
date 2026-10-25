#' Read official People's Bank of China statistics and releases
#'
#' `readPBOC` discovers data published on the People's Bank of China (PBOC)
#' Chinese and English websites, reads spreadsheet and HTML resources, and
#' preserves revision metadata for reproducible workflows. It does not attempt
#' to infer or revise the official figures.
#'
#' The package is organised around four user tasks:
#'
#' * discovery: [pboc_years()], [pboc_sections()], and [pboc_catalog()] list
#'   official archive pages and attachment URLs;
#' * reading: [pboc_read()] reads a local or remote spreadsheet or HTML table,
#'   [pboc_tidy()] normalises irregular worksheets, and [pboc_data()] combines
#'   discovery with reading;
#' * releases: [pboc_releases()] indexes statistical statements and
#'   [pboc_read_release()] extracts release text and attachment links; and
#' * preservation: [pboc_download()] downloads revision-aware files,
#'   [pboc_snapshot()] records a catalog CSV, and [pboc_revisions()] compares
#'   two snapshots.
#'
#' Discovery and release functions access the official PBOC websites over
#' HTTPS at call time. Reading a local path and comparing or snapshotting local
#' data are offline operations. Downloaded files and manifests are written only
#' to paths supplied by the caller; the package does not install data or make
#' startup downloads. See the individual function references for return
#' columns, classes, attributes, failure modes, and network behaviour.
#'
#' @author Chartwell Macro
#' @name readPBOC-package
#' @aliases readPBOC
#' @docType package
#' @keywords package
"_PACKAGE"
