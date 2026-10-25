## readPBOC 0.1.3

This is a development check record for the documentation and CRAN-readiness
work. No CRAN submission was made.

### Test environments

* macOS arm64, R 4.5.1.
* The fresh 0.1.3 source tarball installed and checked in an isolated
  library.

### R CMD check results

The independent final `R CMD check --as-cran` run completed with 0 errors,
0 warnings, and 1 note. Examples, vignettes, manual generation, installation,
and the full package tests completed successfully. The test suite had 210
successful expectations, plus one intentional live-network test skip.

The remaining note concerns incoming feasibility: this is a new submission,
and the canonical GitHub repository and pkgdown Pages URLs are currently
private or unpublished and return 404 to unauthenticated checks. One local
repeat also reported the environment-only note that the current time could
not be verified.

### Additional checks

The package name was checked against current and archived CRAN and current
Bioconductor release/devel package indexes on 2026-08-24; no case-insensitive
match was found. There are no CRAN reverse dependencies. Chartwell is an
unregistered downstream contract and was treated as read-only; its focused
provider check is not a CRAN reverse-dependency check. GitHub Actions checks
completed successfully on the pushed branch for R release on Linux, macOS,
and Windows, and for R oldrel and devel on Linux. The coverage and pkgdown
artifact-build workflows also completed successfully; feature-branch Pages
deployment was intentionally skipped.
`pkgdown::check_pkgdown()` reported no problems, and
`spelling::spell_check_package()` reported no spelling errors using the
reviewed `inst/WORDLIST`. After intentional reserved synthetic hosts were
excluded, URL checking reported only the private/unpublished GitHub and Pages
URLs described above.

The package metadata consistently records Chartwell Macro as the primary author,
maintainer, and copyright holder. The creator email required by R uses a
non-personal GitHub noreply address and is omitted from optional public
surfaces.

### Submission status

The maintainer confirms that permission has been obtained for perpetual
redistribution of the bundled PBOC-derived material with readPBOC, including
through CRAN and its mirrors. PBOC retains copyright; `inst/COPYRIGHTS`
records the scope, provenance, and required attribution without asserting
that the material is public domain or covered by the package's MIT License.
Before a CRAN submission, the canonical repository and Pages URLs must be
publicly accessible.
