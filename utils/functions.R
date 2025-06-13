# functions.R

# kmeans-functions

# dfm -- a quanteda document-term object
# clust.vect -- a boolean vector the same length of dfm, where TRUE indicates a member of the focal cluster
# alpha.0 -- the strength of the prior, expressed as number of terms per sub-corpus
clusterFightinWords <- function(dfm, clust.vect, alpha.0=500) {
  if(alpha.0<=0) stop("prior must be greater than 0")
  
  overall.terms <- colSums(dfm)
  
  n <- sum(overall.terms)
  
  prior.terms <- overall.terms / n*alpha.0
  
  cluster.terms <- colSums(dfm[clust.vect,])
  
  cluster.n <- sum(cluster.terms)
  
  cluster.term.odds <-
    (cluster.terms + prior.terms) /
    (cluster.n + alpha.0 - cluster.terms - prior.terms)
  overall.term.odds <-
    (cluster.terms + prior.terms) /
    (n + alpha.0 - overall.terms - prior.terms)
  
  log.odds <- log(cluster.term.odds) - log(overall.term.odds)
  
  variance <- 1/(cluster.terms + prior.terms) + 1/(overall.terms + prior.terms)
  
  return(log.odds / sqrt(variance))
}

# dfm -- a quanteda document-term object
# results -- a k-means results object
# n.terms -- the number of terms to include in the label
clusterLabels <- function(dfm, results, n.terms=20) {
  clusters <- length(results$withinss)
  labels <- rep("", clusters)
  
  for(clust.num in 1:clusters) {
    clust.vect <- results$cluster == clust.num
    
    terms <- clusterFightinWords(dfm, clust.vect)
    
    terms <- order(terms, decreasing = T)[1:n.terms]
    
    labels[clust.num] <- paste(colnames(dfm)[terms],collapse=", ")
  }
  return(labels)
}

# In case we want to print nice versions of the labels.
# I don't use this here, but sprintf() is super useful to know.
printLabels <- function(labels, results){
  for (clust.num in length(labels)) {
    # sprintf() is a little wonky, but a super powerful tool for formatting your text output
    cat(sprintf("%2i) %5.0f Members | %s",
                clust.num,
                sum(results$cluster==clust.num),
                labels[clust.num]
    )
    )
  }
}



# cosine-similarity-function

compute_hourly_similarity <- function(tweets_df, date, fraud_tags) {
  # Filter for the date and fraud hashtags
  tweets_text <- tweets_df |>
    filter(as.Date(created_at) == as.Date(date),
           str_count(hashtags, "\\S+") >= 2,
           str_detect(hashtags, regex(str_c(fraud_tags, collapse = "|"), ignore_case = TRUE))) |>
    mutate(tweet_id = as.character(tweet_id))
  
  if (nrow(tweets_text) < 2) return(NULL)
  # building dfm
  corpus <- corpus(tweets_text, docid_field = "tweet_id", text_field = "hashtags")
  dfm_text <- corpus |>
    tokens(remove_punct = TRUE) |>
    tokens_remove(stopwords("english")) |>
    dfm()
  
  if (nrow(dfm_text) < 2 || ncol(dfm_text) < 2) return(NULL)
  
  # making similarity matrix
  sim_matrix <- as.matrix(textstat_simil(dfm_text, method = "cosine", margin = "documents"))
  
  # joining sim matrix with subsetted data
  sim_long <- as.data.frame(as.table(sim_matrix)) |>
    rename(tweet_id_1 = Var1, tweet_id_2 = Var2, similarity = Freq) |>
    filter(tweet_id_1 != tweet_id_2) |>
    mutate(tweet_id_1 = as.character(tweet_id_1),
           tweet_id_2 = as.character(tweet_id_2)) |>
    left_join(tweets_text |> select(tweet_id, created_at, engagement_score), by = c("tweet_id_1" = "tweet_id")) |>
    rename(time1 = created_at, engagement1 = engagement_score) |>
    left_join(tweets_text |> select(tweet_id, created_at, engagement_score), by = c("tweet_id_2" = "tweet_id")) |>
    rename(time2 = created_at, engagement2 = engagement_score)
  
  sim_long <- sim_long |>
    mutate(hour = floor_date(time1, unit = "hour")) |>
    group_by(hour) |>
    summarize(mean_similarity = mean(similarity, na.rm = TRUE),
              count = n(),
              total_engagement = sum(engagement1, na.rm = TRUE) + sum(engagement2, na.rm = TRUE),
              day = as.Date(date),
              .groups = "drop")
  
  return(sim_long)
}


# functions shared across scripts are placed here