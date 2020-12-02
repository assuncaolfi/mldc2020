null2na <- function(x) {
  if (is.null(x)) NA else x
}

read_data <- function(file_name, type, cache = TRUE) {
  
  cat("\n*", file_name) 
  csv_path <- file.path("data", paste0(file_name, "_", type, ".csv"))
  if (cache) {
    if (file.exists(csv_path)) return(fread(csv_path, encoding = "UTF-8"))
  }
  
  jl_path <- file.path("data", paste0(file_name, ".jl"))
  con <- file(jl_path)
  lines <- readLines(con, encoding = "UTF-8")
  n_lines <- length(lines)
  
  history_names <- c("train_dataset", "test_dataset")
  if (file_name %in% history_names & type == "history") {
   
    dat <- lines %>% 
      lapply(fromJSON) %>% 
      lapply(function(x) as.data.table(x$user_history)) %>% 
      rbindlist(idcol = "event_id")
    
  } else if (file_name %in% history_names & type == "purchase") {
    
    dat <- lines %>% 
      lapply(fromJSON) %>% 
      lapply(function(x) data.table(item_bought = x$item_bought)) %>% 
      rbindlist(idcol = "event_id")  
    
  } else if (file_name == "item_data") {
    
    dat_list <- lines %>% 
      lapply(fromJSON)
    dat <- data.table(
      item_id = sapply(dat_list, function(x) x$item_id),
      title = sapply(dat_list, function(x) x$title),
      domain_id = sapply(dat_list, function(x) null2na(x$domain_id)),
      category_id = sapply(dat_list, function(x) null2na(x$category_id)),
      price = sapply(dat_list, function(x) null2na(x$price))
    )
    
  }
  
  closeAllConnections()
  fwrite(dat, csv_path)
  return(dat)
}