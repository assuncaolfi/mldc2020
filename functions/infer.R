normalize <- function(x) {
  if (all(x == 0)) return(x)
  x / sum(x)
}

score <- function(X, b) {
  normalize(as.matrix(X) %*% b)
}

scale0 <- function(x) {
  if (all(x == 0)) return(x)
  as.numeric(scale(x))
}

infer_group <- function(history_inference, item, group, keep_cols) {
  top10 <- item[, .SD[1:10], group]
  event_group <- history_inference[
    TRUE,
    setNames(.(group = get(group)[1]), group),
    # .(category_id = unique(category_id)),
    event_id
  ]
  inference <- event_group %>% 
    .[top10, on = group, allow.cartesian = TRUE] %>% 
    .[!is.na(event_id), ..keep_cols] 
}

infer <- function(history, 
                  item, 
                  weights,
                  min_score_quantile = 10,
                  min_sales = 1,
                  domain_variable = "score",
                  make_beep = FALSE) {
  if (make_beep) beepr::beep(10)
  
  # create features
  groups <- c("event_id", "item_id")
  history_inference <- history %>% 
    .[
      TRUE, 
      list(frequency = .N, activity = sum(event_type == "search")),
      groups
    ] %>% 
    .[, recency := 1:.N, event_id]
  
  # optional features
  keep_cols <- c("item_id", "popularity", "cheapness")
  # keep_cols <- c("item_id", "popularity")
  item_feature <- item %>% 
    .[, .(item_id, popularity = sales, cheapness = 1 / price)] %>%
    .[, ..keep_cols]
  history_inference <- item_feature[history_inference, on = "item_id"]
  
  # reoder
  history_inference <- history_inference[
    TRUE,
    .(event_id, item_id, frequency, recency, popularity, cheapness, activity)
  ]
  
  # get variable names
  variables <- names(history_inference) %>% 
    .[!names(history_inference) %in% groups]
  n_variables <- length(variables)
  if (missing(weights)) weights <- normalize(rep(1, n_variables))
 
  # cat("\n")
  # print(weights)
  # print(variables)
  
  # infer history
  history_inference %>% 
    .[, (variables) := lapply(.SD, as.numeric), .SDcols = variables] %>% 
    .[, (variables) := lapply(.SD, normalize), event_id, .SDcols = variables] %>%
    .[, score := score(.SD, weights), event_id, .SDcols = variables] # %>% 
  # set order for history recs
  setorder(history_inference, event_id, -score)
  
  # filter history recommendations by score
  # min_score <- quantile(history_inference$score, min_score_quantile)
  # history_inference[, min_score := min(score[1], min_score), event_id]
  # history_inference <- history_inference[score >= min_score]
  history_inference <- history_inference[
    TRUE,
    .SD[1:min(c(.N, min_score_quantile))],
    event_id
  ]
 
  # join extra info
  history_inference <- item[history_inference, on = "item_id"]
  
  # keep only one domain
  # history_inference[, pos := 1:.N, event_id]
  # event_domain <- history_inference %>%
  #   .[
  #     sales >= min_sales | pos == 1,
  #     .(sum_score = sum(score, na.rm = TRUE)),
  #     .(event_id, domain_id)
  #   ] %>%
  #   .[
  #     order(event_id, -sum_score),
  #     .(event_domain = domain_id[1]),
  #     event_id
  #   ]
  # history_inference <- history_inference %>%
  #   event_domain[., on = "event_id"] %>%
  #   .[domain_id == event_domain]
  
  # infer category
  category_inference <- infer_group(
    history_inference, 
    item, 
    "category_id",
    keep_cols = groups
  )
  
  # infer domain
  domain_inference <- infer_group(
    history_inference,
    item,
    "domain_id",
    keep_cols = groups
  )
  
  # bind history and domain inferences
  inference_list <- list(
    history_inference,
    category_inference,
    domain_inference
  )
  names(inference_list) <- c("history", "category", "domain")
  inference <- rbindlist(
    inference_list,
    fill = TRUE,
    idcol = "inference_type"
  )
  inference[
    TRUE, 
    inference_type := factor(
      inference_type,
      levels = names(inference_list),
      ordered = TRUE
    )
  ]
  keep_cols <- c(groups, variables, "inference_type", "score")
  inference <- inference[, ..keep_cols]
  
  # add extra cols to inference
  extra_cols <- c("item_id", "domain_id", "domain_rank", "sales", "price") 
  extra_item <- item[, ..extra_cols]
  ordered_cols <- c(keep_cols, extra_cols)
  inference <- extra_item[inference, on = "item_id"][, ..ordered_cols]
  
  # filter history recommendations by sales
  inference <- inference[inference_type != "history" | sales >= min_sales]
  
  # set final order
  setorder(inference, event_id, inference_type, -score, -sales, domain_rank)
  # setorder(inference, event_id, domain_rank)
  
  # limit inference to 10 products
  inference <- inference[, unique(.SD, by = "item_id")[1:10], event_id]
  
  # rank positions
  inference[, position := 1:.N, event_id]
  
  return(inference)
}
