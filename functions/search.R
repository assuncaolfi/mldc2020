# search within events with other titles
search_event <- function(event_type,
                         event_info, 
                         title, 
                         item_id, 
                         event_id,
                         external_item_ids,
                         external_titles) {
  is_search <- event_type == "search"
  no_searches <- all(!is_search)
  if (no_searches) {
    return(item_id)
  }
  
  all_searches <- event_info[is_search]
  searches <- unique(all_searches)
  search_idx <- match(all_searches, searches)
  titles <- unique(title[!is_search])
  item_ids <- unique(item_id[!is_search])

  if (length(titles) == 0) {
    titles <- external_titles
    item_ids <- external_item_ids
  }
  
  it <- itoken(c(searches, titles), progressbar = FALSE)
  v <- create_vocabulary(it)
  vectorizer <- vocab_vectorizer(v)

  it1 <- itoken(searches, progressbar = FALSE)
  it2 <- itoken(titles, progressbar = FALSE)
  dtm1 <- create_dtm(it1, vectorizer)
  dtm2 <- create_dtm(it2, vectorizer)
  mat <- sim2(dtm1, dtm2, "jaccard", norm = "none")
  
  item_id[is_search] <- item_ids[apply(mat, 1, which.max)][search_idx]
  cat("\r events completed =", event_id)
  return(item_id)
}

prep_text <- function(x) {
  # Encoding(x) <- "latin1"
  # x %>% 
  #   gsub("[0-9]+", "", .) %>% 
  #   gsub("\\b\\w{1,3}\\b", "", .) %>% 
  #   tolower()
  # x %>%
  #   stri_replace_all("", regex = "[0-9]+") %>%
  #   stri_replace_all("", regex = "\\b\\w{1,3}\\b") %>%
  #   stri_trans_tolower() %>%
    # stri_trans_general("latin-ascii") # %>%
  x %>%
    str_remove_all("[0-9]+") %>%
    str_remove_all("\\b\\w{1,3}\\b") %>%
    str_to_lower() %>%
    stringi::stri_trans_general("latin-ascii") %>%
    str_squish()
}