library(tidyverse)
library(lubridate)
library(tidytext)

#tweets <- bzfile("final_input/dataverse_files/unitetheright.txt.bz2")

#lines <- readLines(tweets, n = 10)

#print(lines)

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

