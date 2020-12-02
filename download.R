cat("\ndownloading and extracting data...")

if (!file.exists("data")) dir.create("data")

file_names <- paste0(
  c("test_dataset", "train_dataset", "item_data"),
  ".jl"
)
file_paths <- file.path("data", file_names)
remaining_files <- file_names[!file.exists(file_paths)]

dls <- suppressMessages(lapply(
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
)
