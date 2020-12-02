relevance <- function(y, domain_y, yhat, domain_yhat) {
  fcase(
    y == yhat, 12,
    domain_y == domain_yhat, 1,
    default = 0
  )
}

DCG <- function(relevance, position) {
  relevance <- relevance[position]
  sum(relevance / log(1 + position))
}

iDCG <- DCG(
  relevance = c(12, rep(1, 9)),
  position = 1:10
)

NDCG <- function(relevance, position) {
  DCG(relevance, position) / iDCG
}

evaluate_loss <- function(inference,
                          item, 
                          purchase, 
                          view_events = 1:1000) {
  bought_domain <- item[
    TRUE, 
    .(item_bought = item_id, domain_bought = domain_id)
  ]
  evaluation <- inference %>% 
    purchase[., on = "event_id"] %>% 
    bought_domain[., on = "item_bought"]
  evaluation[
    TRUE, 
    relevance := relevance(
      item_bought, domain_bought,
      item_id, domain_id
    )
  ]
  evaluation[, NDCG := NDCG(relevance, position), event_id]
  loss <- mean(evaluation$NDCG)
  cat("\n> mean NDCG =", evaluation[, mean(NDCG)], "\n")
  
  open_viewer <- length(view_events) > 0
  if (open_viewer) {
    evaluation <- item %>% 
      .[, .(item_id, title)] %>% 
      .[evaluation, on = "item_id"]
    evaluation[, doable := any(domain_id %in% domain_bought), event_id]
    evaluation %>% 
      # .[, benefit := round(benefit, 4)] %>% 
      .[doable == TRUE & event_id %in% view_events] %>% 
      tibble::view()
  }
  
  return(loss)
}

infer_eval <- function(frequency_weight,
                       recency_weight,
                       popularity_weight) {
  inference <- infer(
    history,
    item,
    weights = c(
      frequency_weight,
      recency_weight,
      popularity_weight,
      0,
      0
    )
  )
  NDCG <- evaluate_loss(
    inference,
    item,
    purchase,
    view_events = NULL
  )
  list(Score = NDCG)
}