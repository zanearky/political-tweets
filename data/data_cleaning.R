library(tidyverse)
library(lubridate)
library(tidytext)
library(stringr)
library(cld3)
library(bit64)

# original dataset: https://www.kaggle.com/datasets/manchunhui/us-election-2020-tweets

# reading in #joebiden data

tweets_biden <- read.csv("data/data_raw/tweets_csv/hashtag_joebiden.csv.gz",
                         na = "NA") |>
  select(tweet_id, 
         tweet, 
         likes, 
         retweet_count,
         user_id,
         user_screen_name,
         user_followers_count,
         country,
         user_join_date,
         created_at,
         source
  ) |>
  mutate(
    likes = as.numeric(likes),
    user_followers_count = as.numeric(user_followers_count),
    created_at = ymd_hms(created_at),
    date_simple = format(created_at, "%m-%d")
  ) |>
  filter(
    likes > 50 | retweet_count > 50,
  )




## CLEANING PROCESS ##

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
          join_date_num
          )

twitter_epoch <- 1288834974657

biden_sample <- tweets_biden |> 
  slice_sample(n=6000) |>
  mutate(
    user_id = as.integer64(user_id),
    
    timestamp_ms = (user_id %/% as.integer64(2^22)) + twitter_epoch,
    user_created_at = as.POSIXct(as.numeric(timestamp_ms) / 1000, origin = "1970-01-01", tz = "UTC"),
    
    user_created_at = ymd_hms(user_created_at),
    user_created_num = as.numeric(format(user_created_at, "%m-%d-%Y-%H")),
    reply = ifelse(join_date_num == user_created_num, FALSE, TRUE)
  )
    




hashtags <- biden_sample |>
  mutate(hashtags = str_squish(str_extract_all(tweet, "#\\S+"))) |>
  select(user_screen_name, hashtags)

addresses <- biden_sample |>
  mutate(address = str_squish(str_extract_all(tweet, "@\\S+")),
         reply = ifelse()) |>
  select(user_id)

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


data("stop_words")

biden_words <- biden_sample |>
  unnest_tokens(word, tweet) |>
  anti_join(stop_words) |>
  count(user_screen_name, word, sort=TRUE)

total_words <- biden_words |>
  group_by(user_screen_name) |>
  summarize(total = sum(n))

biden_words <- left_join(biden_words, total_words)

biden_tf_idf <- biden_words |>
  bind_tf_idf(word, user_screen_name, n)

biden_tf_idf |>
  select(-total) |>
  arrange(desc(tf_idf))

bing <- get_sentiments("bing")

bing_biden <- bind_rows(
  biden_words |>
    inner_join(bing) |>
    mutate(method = "Bing et al.")
) |>
  count(method, index = user_screen_name, sentiment) |>
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) |>
  mutate(sentiment = positive - negative)



biden_words |>
  inner_join(get_sentiments("afinn")) |>
  group_by(index = user_screen_name) |>
  summarise(sentiment = sum(value)) |>
  mutate(method = "AFINN")


replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"


tidy_biden <- tweets_biden |>
  mutate(language = textcat(tweet)) |>
  filter(language == "")
  filter(!str_detect(tweet, "^RT")) |>
  mutate(tweet = str_replace_all(tweet, replace_reg, "")) |>
  unnest_tokens(word, tweet, token = "regex", pattern = unnest_reg) |>
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))








# reading in #donaldtrump data

tweets_trump <- read.csv("data/data_raw/tweets_csv/hashtag_donaldtrump.csv.gz",
                         na = "NA") |>
  select(tweet_id, 
         tweet, 
         likes, 
         retweet_count,
         user_id,
         user_screen_name,
         user_followers_count,
         country,
         user_join_date,
         created_at,
         source
  ) |>
  mutate(
    likes = as.numeric(likes),
    user_followers_count = as.numeric(user_followers_count),
    created_at = ymd_hms(created_at),
    date_simple = format(created_at, "%m-%d")
  ) |>
  filter(
    likes > 50 | retweet_count > 50,
  )

table(tweets_trump$date_simple)
table(tweets_biden$date_simple)

trump_tokens <- tibble(text = tweets_trump$tweet) |>
  unnest_tokens(word, text)

stop <- c("trump",
          "biden",
          "https",
          "de",
          "la",
          "en",
          "le",
          "donaldtrump")

stop_pop <- tibble(text = stop)

data("stop_words")

trump_tokens <- trump_tokens |>
  anti_join(stop_words) |>
  anti_join(stop_pop)

trump_tokens |>
  count(word, sort=TRUE)

trump_word_counts

trump_word_counts |>
  group_by(sentiment) |>
  slice_max(n, n = 10) |>
  ungroup() |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

ggplot(tweets_trump, aes(x = date_simple)) +
  geom_bar()

ggplot(tweets_biden, aes(x = date_simple)) +
  geom_bar()

