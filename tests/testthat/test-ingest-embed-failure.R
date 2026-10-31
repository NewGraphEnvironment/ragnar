# A worker that cannot embed hands its chunks back unembedded, and the main
# process embeds them again. If the provider refuses that input (HTTP 400, as
# Ollama answers "the input length exceeds the context length"), the input
# should be reported like any other worker failure and the ingest carry on.
# A broken embedder still ends the ingest (see test-ingest.R).

test_that("an input that cannot be embedded is reported, not fatal", {
  skip_on_cran()

  paths <- c(tempfile(fileext = ".md"), tempfile(fileext = ".md"))
  writeLines(c("# Fine", "", "Some content."), paths[1])
  writeLines(c("# Refused", "", "REFUSE this content."), paths[2])

  store <- ragnar_store_create(
    embed = function(x) {
      if (any(grepl("REFUSE", x, fixed = TRUE))) {
        stop(structure(
          class = c("httr2_http_400", "httr2_http", "httr2_error", "error", "condition"),
          list(message = "HTTP 400 Bad Request: input refused", call = NULL)
        ))
      }
      matrix(1, nrow = length(x), ncol = 4L)
    }
  )
  withr::defer(DBI::dbDisconnect(store@con, shutdown = TRUE))

  captured <- NULL
  result <- withCallingHandlers(
    ragnar_store_ingest(store, paths, progress = FALSE),
    ragnar_ingest_worker_error = function(w) {
      captured <<- w
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(captured$path, paths[2])
  expect_match(conditionMessage(captured), "input refused")
  summary <- attr(result, "ingest_summary")
  expect_equal(summary$status[match(paths, summary$path)], c("ok", "error"))
  expect_equal(
    DBI::dbGetQuery(store@con, "SELECT origin FROM documents")$origin,
    paths[1]
  )
})
