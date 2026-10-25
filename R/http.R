pboc_user_agent <- function() {
  getOption(
    "readPBOC.user_agent",
    "readPBOC/0.1.0 (R package for official PBOC statistics)"
  )
}

pboc_handle <- function() {
  handle <- curl::new_handle()
  curl::handle_setopt(
    handle,
    useragent = pboc_user_agent(),
    followlocation = TRUE,
    timeout = getOption("readPBOC.timeout", 60)
  )
  handle
}

pboc_fetch <- function(url, attempts = 3L) {
  attempts <- max(1L, as.integer(attempts))
  last_error <- NULL

  for (attempt in seq_len(attempts)) {
    response <- tryCatch(
      curl::curl_fetch_memory(url, handle = pboc_handle()),
      error = function(error) error
    )
    if (!inherits(response, "error") &&
        response$status_code >= 200L &&
        response$status_code < 300L) {
      return(response)
    }

    if (inherits(response, "error")) {
      last_error <- conditionMessage(response)
    } else {
      last_error <- paste("HTTP status", response$status_code)
      if (response$status_code < 500L && response$status_code != 429L) break
    }
    if (attempt < attempts) Sys.sleep(0.25 * 2^(attempt - 1L))
  }

  stop("Could not fetch <", url, ">: ", last_error, call. = FALSE)
}

pboc_fetch_html <- function(url) {
  response <- pboc_fetch(url)
  xml2::read_html(response$content, encoding = "UTF-8")
}

pboc_download_url <- function(url, path) {
  curl::curl_download(
    url,
    destfile = path,
    quiet = TRUE,
    mode = "wb",
    handle = pboc_handle()
  )
  invisible(path)
}
