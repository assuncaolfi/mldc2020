# clear -----------------------------------------------------------------------
cat("\ncleaning environment...")

rm(list = ls())

# params ----------------------------------------------------------------------
cat("\nsetting constants...")

options(timeout = 1000)
# ncores <- parallel::detectCores()
# data.table::setDTthreads(ncores)
# cat("\n* using", ncores, "cores")

train <- FALSE
submit <- !train

# libs ------------------------------------------------------------------------
cat("\nloading libs...")

suppressMessages({
  library(data.table)
  library(jsonlite)
  library(magrittr)
  library(parallel)
  library(lubridate)
  # library(stringi)
  library(stringr)
  # library(text2vec)
})
if (train) library(ParBayesianOptimization)

# functions
source(file.path("functions", "infer.R"))
source(file.path("functions", "read.R"))
source(file.path("functions", "eval.R"))
source(file.path("functions", "search.R"))

# download --------------------------------------------------------------------
cat("\ndownloading and extracting data...")

if (!file.exists("data")) dir.create("data")

file_names <- paste0(
  c("test_dataset", "train_dataset", "item_data"),
  ".jl"
)
file_paths <- file.path("data", file_names)
remaining_files <- file_names[!file.exists(file_paths)]

dls <- lapply(
  remaining_files,
  function(file_name) {
    compressed_file_name <- paste0(file_name, ".gz")
    compressed_file_path <- file.path("data", compressed_file_name)
    cat("\n*", compressed_file_name)
    download.file(
      paste0(
        "https://meli-data-challenge.s3.amazonaws.com/2020/",
        compressed_file_name
      ),
      destfile = compressed_file_path,
      quiet = TRUE
    )
    file_path <- file.path("data", file_name)
    writeLines(
      readLines(gzfile(compressed_file_path)),
      con = file(file_path)
    )
    file.remove(compressed_file_path)
    closeAllConnections()
    TRUE
  }
)

# read ------------------------------------------------------------------------
cat("\nreading data...")

if (train) {
  history_name <- "train_dataset"
} else {
  history_name <- "test_dataset"
}
history <- read_data(history_name, type = "history")
purchase <- read_data("train_dataset", type = "purchase")
item <- read_data("item_data", type = "item")

# transform -------------------------------------------------------------------
cat("\nprocessing data...")

# history
history[, item_id := suppressWarnings(as.integer(event_info))]
history[, event_timestamp := as_datetime(event_timestamp)]
history[event_type == "search", event_info := prep_text(event_info)]
setorder(history, event_id, event_timestamp)
# item
item[, title := prep_text(title)]
item[, price := as.numeric(price)]
item[, domain_size := .N, domain_id]
item[
  TRUE, 
  price := ifelse(is.na(price), median(price, na.rm = TRUE), price),
  domain_id
]

sales <- purchase[, .(sales = .N), item_bought]
sales[, item_id := item_bought]
item <- sales[item, on = "item_id"]
item[is.na(sales), sales := 0]
if (train) item[sales > 0, sales := sales - 1]
setorder(item, -sales)
item[, domain_rank := 1:.N, domain_id]

# search ----------------------------------------------------------------------
cat("\nimputing items for search events...")

search_path <- file.path("data", paste0(history_name, "_search.csv"))

if (file.exists(search_path)) {
  search <- fread(search_path, encoding = "UTF-8")
} else {
  history <- item %>% 
    .[, .(item_id, title)] %>% 
    .[history, on = "item_id"] 
  total_events <- history[, length(unique(event_id))]
  external_item <- item[domain_size >= 10 & sales > 0]
  external_titles <- external_item$title
  external_item_ids <- external_item$item_id
  cat("\ntotal events =", total_events, "\n")
  history[
    TRUE, 
    item_id := suppressWarnings(search_event(
      event_type,
      event_info,
      title,
      item_id,
      .GRP,
      external_item_ids,
      external_titles
    )),
    event_id
  ]
  history[, title := NULL]
  search <- unique(
    history[event_type == "search", .(event_id, event_info, item_id)]
  )
  fwrite(search, search_path)
}

history <- search %>% 
  .[, .(event_id, event_info, result_id = item_id)] %>% 
  .[history, on = c("event_id", "event_info")]
history[event_type == "search", item_id := result_id]
history[, result_id := NULL]
history <- na.omit(history) 

# tune ------------------------------------------------------------------------
cat("\ntuning params...")

best_params_path <- file.path("data", "best_params.csv")
if (train) {
  bounds <- list(
    frequency_weight = c(0L, 10L),
    recency_weight = c(0L, 10L),
    popularity_weight = c(0L, 10L)
  )
  set.seed(1995)
  opt_obj <- ParBayesianOptimization::bayesOpt(
    FUN = infer_eval,
    bounds = bounds,
    initPoints = 12,
    iters.n = 4 * 12,
    parallel = FALSE,
    plotProgress = TRUE
  )
  saveRDS(opt_obj, file.path("data", "pars.rds"))
  score_summary <- as.data.table(opt_obj$scoreSummary)
  best_params <- score_summary[Score == max(Score)]
  fwrite(best_params, best_params_path)
} else { 
  best_params <- fread(best_params_path)
}

# infer -----------------------------------------------------------------------
cat("\ninferring...")

inference <- infer(history, item, weights = as.numeric(best_params[, 3:7]))

# write -----------------------------------------------------------------------
cat("\nwriting submission...")

if (submit) {
  submission <- inference[, lapply(1:10, function(x) item_id[x]), event_id]
  setorder(submission, event_id)
  submission[, event_id := NULL]
  fwrite(submission, file.path("data", "submission.csv"), col.names = FALSE)
}

# versioning ------------------------------------------------------------------
cat("\nversioning packages...")

si <- sessionInfo()
pkg_names <- names(si$otherPkgs)
pkg_versions <- sapply(pkg_names, function(x) si$otherPkgs[[x]]$Version)
install_lines <- paste0(
  "remotes::install_version('",
  pkg_names, "', '", pkg_versions,
  "', repos = 'http://cran.us.r-project.org', upgrade = FALSE, quiet = TRUE)"
)
install_lines <- c("install.packages('remotes')", install_lines)
writeLines(install_lines, file("install.R"))

# done ------------------------------------------------------------------------

cat("\ndone! :-)")
