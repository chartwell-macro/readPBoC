# Contributing to readPBOC

Email support and issue reports to [chartwellmacro@gmail.com](mailto:chartwellmacro@gmail.com). Pull requests are welcome at
<https://github.com/chartwell-macro/readPBoC>. For a substantial API, output, or
runtime change, please email [chartwellmacro@gmail.com](mailto:chartwellmacro@gmail.com) first so that compatibility and the
downstream contract can be discussed.

Before opening a pull request:

1. Use R 4.1.0 or newer and keep changes focused on the stated issue.
2. Preserve the existing public signatures, defaults, output schemas, classes,
   attributes, dates, errors, and side effects unless a compatibility change
   has been explicitly agreed.
3. Keep tests and examples deterministic and offline by default. Use temporary
   paths for files and do not add credentials, network caches, or copied PBOC
   product data without documented redistribution rights.
4. Run the local tests:

   ```sh
   Rscript --vanilla -e 'testthat::test_local(reporter = "summary")'
   ```

   The live discovery test is opt-in and requires network access:

   ```sh
   READPBOC_LIVE_TESTS=true Rscript --vanilla -e 'testthat::test_local(reporter = "summary")'
   ```

5. If package files or examples change, build and check the source package
   locally with `R CMD build` and `R CMD check --as-cran`. Regenerate tracked
   documentation from its source and keep README and vignette examples in
   sync.

Please describe the user-visible effect, tests run, and any network or data
rights considerations in the pull request. Do not include live credentials or
private downloaded files in an issue or patch.
