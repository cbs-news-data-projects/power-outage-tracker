# datawrapper.R
# Minimal Datawrapper v3 API client (replaces the DatawRappr package).
# Only the calls this project needs: upload data, edit metadata, publish.
# Depends on httr + jsonlite, which the workflow already installs.

library(httr)
library(jsonlite)

dw_base <- "https://api.datawrapper.de/v3"

# Stop with a clear message that includes the API's response body.
dw_check <- function(resp, task) {
  if (http_error(resp)) {
    body <- content(resp, as = "text", encoding = "UTF-8")
    stop(sprintf("Failed to %s: HTTP %s. %s", task, status_code(resp), body),
         call. = FALSE)
  }
  invisible(resp)
}

# Upload a data frame to a chart as CSV.
dw_upload_data <- function(chart_id, df, api_key) {
  csv <- paste(
    capture.output(write.csv(df, file = "", row.names = FALSE, na = "")),
    collapse = "\n"
  )
  resp <- PUT(
    sprintf("%s/charts/%s/data", dw_base, chart_id),
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type("text/csv"),
    body = csv,
    user_agent("cbs-news-power-outage-tracker")
  )
  dw_check(resp, "upload data to Datawrapper")
  invisible(resp)
}

# Update chart title and metadata. Any argument left NULL is not sent.
dw_update_chart <- function(chart_id, api_key, title = NULL, intro = NULL,
                            annotate = NULL, byline = NULL,
                            source_name = NULL, source_url = NULL,
                            folder_id = NULL) {
  describe <- list()
  if (!is.null(intro))       describe[["intro"]] <- intro
  if (!is.null(byline))      describe[["byline"]] <- byline
  if (!is.null(source_name)) describe[["source-name"]] <- source_name
  if (!is.null(source_url))  describe[["source-url"]] <- source_url

  metadata <- list()
  if (length(describe) > 0)  metadata[["describe"]] <- describe
  if (!is.null(annotate))    metadata[["annotate"]] <- list(notes = annotate)

  payload <- list()
  if (!is.null(title))      payload[["title"]] <- title
  if (!is.null(folder_id))  payload[["folderId"]] <- folder_id
  if (length(metadata) > 0) payload[["metadata"]] <- metadata

  resp <- PATCH(
    sprintf("%s/charts/%s", dw_base, chart_id),
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = toJSON(payload, auto_unbox = TRUE),
    user_agent("cbs-news-power-outage-tracker")
  )
  dw_check(resp, "update Datawrapper chart")
  invisible(resp)
}

# Publish a chart.
dw_publish <- function(chart_id, api_key) {
  resp <- POST(
    sprintf("%s/charts/%s/publish", dw_base, chart_id),
    add_headers(Authorization = paste("Bearer", api_key)),
    user_agent("cbs-news-power-outage-tracker")
  )
  dw_check(resp, "publish Datawrapper chart")
  invisible(resp)
}
