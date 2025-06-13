library(tidyverse)
library(lubridate)
library(tidytext)
library(readr)
library(stringr)
library(tidyr)
library(cld3)
library(bit64)



# original dataset: https://www.kaggle.com/datasets/manchunhui/us-election-2020-tweets

# reading in #joebiden data



## cleaning process ##

# record hashtags into separate dataset
# remove hashtags, symbols, links, and @'s
# select only english languages
# tokenize and analyze


tweets_biden <- read_csv("data/data_raw/tweets_csv/hashtag_joebiden.csv.gz",
                         na = "NA") |>
  mutate(created_at = ymd_hms(created_at),
         date_simple = format(created_at, "%m-%d"),
         user_join_date = ymd_hms(user_join_date),
         join_date_num = as.numeric(format(user_join_date, "%m-%d-%Y-%H"))) |>
  select(tweet, 
          likes, 
          retweet_count,
          user_screen_name,
          user_followers_count,
          user_id,
          tweet_id,
          user_join_date,
          date_simple,
          created_at,
          join_date_num
          )


tweets_trump <- read_csv("data/data_raw/tweets_csv/hashtag_donaldtrump.csv.gz",
                         na = "NA") |>
  mutate(created_at = ymd_hms(created_at),
         date_simple = format(created_at, "%m-%d"),
         user_join_date = ymd_hms(user_join_date),
         join_date_num = as.numeric(format(user_join_date, "%m-%d-%Y-%H"))) |>
  select(tweet, 
         likes, 
         retweet_count,
         user_screen_name,
         user_followers_count,
         user_id,
         tweet_id,
         user_join_date,
         date_simple,
         created_at,
         join_date_num
  ) |>
  filter(retweet_count > 0)
  
table(tweets_trump$date_simple)


# *10-16: FBI investigate joe/hunter emails with business dealings in ukraine
# *10-19: SCOTUS splits on mail-in ballot deadline decision in PA / debate microphones
# 10-21 to 10-23: 
  # 10-21: Iranian and Russian calls threaten swing state voters to vote trump
  # *+10-22: SCOTUS makes favorable ruling on curbside voting ban in AL
  # 10-23: The final presidential debate is held in U of Nashville, TN
# 10-26 to 10-28: 
  # *10-26: Biden travel scandal / jared kushner scandal / SCOTUS block absentee
  # ballot extension in WI / senate confirms ACB to SCOTUS / Walter Wallace is shot in philly
  # *+10-27: biden, harris campaign in GA and NV
  # *+10-28: WH science officer accidentally releases press that notes the end of the pandemic / 
  # SCOTUS allows ballot extensions to stand in NC and PA / 
# 11-01 to 11-08: presidential election
# * = trump rally
# + = obama/biden/kamala rally

sent
tt_1_8 <- tweets_trump |>
  filter(month(created_at) == 11)

tt_1_8 <- tweets_trump |>
  filter(month(created_at) == 11)

table(tweets_trump$date_simple)

tweets_biden11 <- tweets_biden |>
  filter(month(created_at) == 11)


twitter_epoch <- 1288834974657

set.seed(12345)

## taking a sample ##

biden_sample <- tweets_acb |> 
  slice_sample(n=6000)
    
trump_sample <- tweets_trump #|> 
  #slice_sample(n=200000)


# cleaning our samples

# biden sample cleaning
biden_sample <- biden_sample |>
  mutate(
    tweet = str_remove_all(tweet, "#\\w+"),
    tweet = str_remove_all(tweet, "(\\s#\\w+)+$"),
    tweet = str_remove_all(tweet, "https?://\\S+"),
    tweet = str_remove_all(tweet, "@\\w+"),
    tweet = str_remove_all(tweet, "[^\\w\\s]"),
    tweet = str_remove_all(tweet, "[\U0001F600-\U0001F64F]"),  
    tweet = str_remove_all(tweet, "[\U0001F300-\U0001F5FF]"), 
    tweet = str_remove_all(tweet, "[\U0001F680-\U0001F6FF]"), 
    tweet = str_remove_all(tweet, "[\U0001F1E0-\U0001F1FF]"),
    tweet = str_squish(tweet),
    language = detect_language(tweet)
  ) |>
  filter(language == "en")

## trump sample cleaning ##

trump_sample <- trump_sample |>
  mutate(
    hashtags = str_extract_all(tweet, "#\\w+"),
    tweet = str_remove_all(tweet, "#\\w+"),
    tweet = str_remove_all(tweet, "(\\s#\\w+)+$"),
    tweet = str_remove_all(tweet, "https?://\\S+"),
    tweet = str_remove_all(tweet, "@\\w+"),
    tweet = str_remove_all(tweet, "[^\\w\\s]"),
    tweet = str_remove_all(tweet, "[\U0001F600-\U0001F64F]"),  
    tweet = str_remove_all(tweet, "[\U0001F300-\U0001F5FF]"), 
    tweet = str_remove_all(tweet, "[\U0001F680-\U0001F6FF]"), 
    tweet = str_remove_all(tweet, "[\U0001F1E0-\U0001F1FF]"),
    tweet = str_squish(tweet),
    language = detect_language(tweet)
  ) |>
  filter(language == "en")

# simple heuristic bot cleaning
trump_sample <- trump_sample |>
  mutate(
    only_hashtags = str_detect(tweet, "^#\\w+(\\s+#\\w+)*$"),
    only_url = str_detect(tweet, "^https?://"),
    low_content = nchar(tweet) < 10,
    suspected_bot = only_hashtags | only_url | low_content
  ) |>
  filter(!suspected_bot)




## SENTIMENT ANALYSIS ##

tweets_tokens <- trump_sample |>
  select(tweet_id, tweet, likes, retweet_count, user_followers_count, date_simple) |>
  unnest_tokens(word, tweet) |>
  anti_join(stop_words)


tweets_sentiment <- tweets_tokens |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(tweet_id, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(sentiment = positive - negative)
  
tweet_sample <- trump_sample |>
  select(tweet_id, tweet, likes, retweet_count, user_followers_count, date_simple) |>
  left_join(tweets_sentiment, by = "tweet_id") |>
  mutate(sentiment = ifelse(is.na(sentiment), 0, sentiment),
         has_sentiment = sentiment != 0)

sentiment_has <- tweet_sample[tweet_sample$has_sentiment == TRUE,]

model_1 <- lm(retweet_count~sentiment+likes+user_followers_count, data = tweet_sample)
model_2 <- lm(retweet_count~sentiment+likes+user_followers_count, data = sentiment_has)

summary(model_1)
summary(model_2)

model_3 <- lm(sentiment~likes+user_followers_count+retweet_count, data = tweet_sample)
model_4 <- lm(sentiment~likes+user_followers_count+retweet_count, data = sentiment_has)

summary(model_3)
summary(model_4)

data("stop_words")


retweets_day <- tweet_sample |>
  group_by(date_simple) |>
  summarise(total_retweets = sum(retweet_count),
            total_tweets = n(),
            sentiment_total = sum(sentiment),
            retweet_ratio = total_retweets/total_tweets,
            sentiment_ratio = sentiment_total/total_tweets) |>
  arrange(date_simple)

print(retweets_day)

summary(lm(retweet_ratio~sentiment_total, data = retweets_day))

summary(lm(total_retweets~sentiment_total+total_tweets, data = retweets_day))


tweets_trump$retweet_bin <- cut(tweets_trump$retweet_count,
                                breaks = c(-1, 0, 1, 10, 100, 1000, Inf),
                                labels = c("0", "1", "2–10", "11–100", "101–1000", "1000+"))

table_by_day <- table(tweets_trump$date_simple, tweets_trump$retweet_bin)

print(table_by_day)


ggplot(retweets_day, aes(x = as.Date(date_simple, format="%m-%d"))) +
  geom_line(aes(y = retweet_ratio), color = "blue") +
  geom_line(aes(y = sentiment_total / 1000), color = "red") +
  labs(y = "Retweet Ratio / Scaled Sentiment", x = "Date",
       title = "Daily Tweet Engagement and Sentiment") +
  theme_minimal()


## DATA FEATURE MIXTURE ANALYSIS ##
library(quanteda)
library(topicmodels)
library(tidytext)

# hashtag analysis

corpus_hash <- corpus(clean_sample,
                      docid_field = "tweet_id",
                      text_field = "hashtags")

tokens_hash <- corpus_hash |>
  tokens(remove_punct = TRUE)

dfm_hash <- dfm(tokens_hash)

dfm_hash <- dfm_tfidf(dfm_hash)

hash_df <- tidy(dfm_hash)
tfidf_summary <- hash_df |>
  group_by(term) |>
  summarise(tfidf = sum(count), .groups = "drop") |>
  arrange(desc(tfidf))

print(n = 40, head(tfidf_summary, 40))

dfm_hash <- dfm_remove(dfm_hash, stop_tags)
print(topfeatures(dfm_hash, 45))

dfm_top <- topfeatures(dfm_hash, 50)

trump_stop <- c("trump", "donaldtrump", "trump2020", "trump2020landslide", "trumptrain",
              "trump2020tosaveamerica",)
biden_stop <- c("biden","joebiden")

stop_tags <- c("trump", "biden", "donaldtrump", "election2020", "elections2020",
               "joebiden","covid19","electionday","gop","america","usa","news",
               "electionresults2020","uselection2020","coronavirus","election",
               "republicans","electionnight","uselection","potus","covid",
               "debates2020", "democrats", "republicans", "democrat","republican",
               "vote2020", "politics", "kamalaharris", "uselections", "cnn", "foxnews",
               "georgia", "2020election", "obama", "elections", "uselections2020",
               "usaelections2020", "2020elections", "uselectionresults2020", "twitter",
               "texas","americadecides2020", "democracy","bidenharris", "debate",
               "debate2020", "pandemic", "trump2020","vote","bidenharris2020","biden2020",
               "bidenharis2020", "nevada", "debatetonight", "pence", "uselectionresults", "voteblue",
               "us","americans","whitehouse","election2020results","usaelection2020",
               "presidentialdebate2020","trumpvsbiden","pennsylvania","pa","michigan",
               "mi","northcarolina","nc","arizona","az","wisconsin","wi","president",
               "florida", "american", "joebiden2020", "electionday2020", "votebidenharris",
               "uspresidentialelections2020", "harris", "media", "scotus", "presidentialelection",
               "twitter", "poll", "israel", "china", "ohio", "china", "russia", "breakingnews", "debates",
               "breaking", "china", "russia", "polls", "voteblue2020", "mcconell", "votebiden",
               "unitedstates", "electionresults", "trumppence2020", "elecciones2020",
               "presidentialelection2020", "philadelphia", "maga", "maga2020", "kag", "kag2020",
               "4moreyears", "trumptrain", "trump2020landslide","trump2020tosaveamerica",
               "trumpisalaughingstock", "trumpmeltdown", "trumptantrum", "votes", "uk", "msnbc",
               "voting", "electoral college", "votebidenharristosaveamerica", "bidenharris2020tosaveamerica",
               "borisjohnson", "putin","india", "joebidenkamalaharris2020", "presidentelectjoe")


dfm_hash <- dfm_trim(dfm_hash, min_termfreq = 0.5, min_docfreq = 50, verbose = TRUE)

kmeans.results.10 <- kmeans(dfm_hash, centers = 6, nstart = 25)

clusterLabels(dfm = dfm_hash, results = kmeans.results.10, 10)



retweets <- trump_sample$retweet_count
retweets_log <- log1p(retweets)


hash_matrix <- convert(dfm_hash, to = "data.frame")


hash_matrix$retweets <- trump_sample

dfm_hash 

dfm_hash <- dfm_trim(mydfm, min_termfreq = 0.5, min_docfreq = 50, verbose = TRUE)

dfm_hash <- dfm_hash[rowSums(dfm_hash) >0,]
hash_lda <- LDA(dfm_hash, k=2, control = list(seed = 1234))

hash_topics <- tidy(hash_lda, matrix = "beta")

hash_top_terms <- hash_topics |>
  group_by(topic) |>
  slice_max(beta, n=10) |>
  ungroup() |>
  arrange(topic, -beta)

hash_top_terms |>
  mutate(term = reorder_within(term, beta, topic)) |>
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()








corpus <- corpus(trump_sample,
                 docid_field = "tweet_id",
                 text_field = "tweet")



new_stop_words <- c(c("amp"), stopwords(language = "english"))
tokens <- corpus |>
  tokens(remove_punct = TRUE) |>
  tokens_select(new_stop_words, selection = "remove") |>
  tokens_remove(stop_words)

tokens_bigrams <- tokens_ngrams(tokens, n=2)

tokens_trigrams <- tokens_ngrams(tokens, n=3)


mydfm <- dfm(tokens)

mydfm <- dfm(tokens_bigrams)

mydfm <- dfm(tokens_trigrams)

## remove tokens 
# tokens <- tokens_remove()

mydfm <- dfm_trim(mydfm, min_termfreq = 5, min_docfreq = 20, verbose = TRUE)

mydfm <- dfm_trim(mydfm, min_termfreq = 5, max_docfreq = 0.5, docfreq_type = "prop")

mydfm_tfidf <- dfm_tfidf(mydfm)

set.seed(12345)

## k-means analysis ##

kmeans.results.10 <- kmeans(mydfm_tfidf, centers = 5, nstart = 10)

clusterLabels(dfm = mydfm_tfidf, results = kmeans.results.10, 10)


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