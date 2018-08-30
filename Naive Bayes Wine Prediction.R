library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(tidytext)
library(tm)
library(e1071)

wine <- read_csv("docs/Wine 130k reviews.csv") %>%
  select(-X1) %>% # this is a row number column and can be dropped
  distinct()

long_descriptions <- wine %>%
  filter(nchar(description) >= 140)

top_reviewed <- long_descriptions %>%
  group_by(variety) %>%
  count() %>%
  filter(n >= 100) %>%
  pull(variety)

popular_long_descriptions <- long_descriptions %>%
  filter(variety %in% top_reviewed) %>%
  select(variety, description)

train_data <- popular_long_descriptions %>%
  group_by(variety) %>%
  sample_frac(size = 0.75, replace = FALSE) %>%
  ungroup()

test_data <- popular_long_descriptions %>%
  anti_join(train_data) %>%
  mutate(Row = row_number())

train_tokens <- train_data %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(regex(word, ignore_case = TRUE), regex(variety, ignore_case = TRUE))) %>%
  count(variety, word) %>%
  ungroup()

priors <- train_data %>% 
  group_by(variety) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(Frac = n/sum(n)) %>% 
  arrange(-n)

conditionals <- train_tokens %>%   
  filter(n > 50)  %>%
  group_by(word) %>%
  mutate(N = sum(n),
         Frac = n/N) %>%
  filter(n != N)

num_varieties <- priors %>%
  distinct(variety, .keep_all = TRUE) %>%
  select(variety, Frac)

count_var <- nrow(num_varieties)
count_words <- train_tokens %>%
  distinct(word) %>%
  nrow()

new_prob <- 1/(count_var + count_words + 1)

accuracy_vector <- vector(mode = "numeric", length = nrow(test_data))

wine_types <- c(unique(tolower(wine$variety)), 
                "barolo", "barbaresco", "cab", "sauv", "pinot", "noir", 
                "sb", "blanc", "syrahs", "zin", "zins", "zin's")

for (j in 1:100) {
  
  one_test <- test_data %>%
    filter(Row == j) %>%
    unnest_tokens(word, description) %>%
    anti_join(stop_words) %>%
    filter(!word %in% wine_types)
  
  given_words <- conditionals %>%
    filter(word %in% one_test$word)
  
  variety_vector <- vector(mode = "numeric", length = nrow(num_varieties))
  names(variety_vector) <- num_varieties$variety
  
  for (i in 1:nrow(num_varieties)) {
    found_words <- given_words %>%
      filter(variety == names(variety_vector[i])) %>%
      pull(Frac)
    
    not_found_words <- vector(mode = "numeric", length = (length(one_test$word) - length(found_words)))
    not_found_words[] <- new_prob
    
    together_words <- c(found_words, not_found_words)
    
    variety_vector[i] <- prod(together_words)
  }
  
  variety_vector_priors <- variety_vector * num_varieties$Frac
  
  print(paste0("Guessed: ", names(variety_vector[which(variety_vector == max(variety_vector))])))
  print(paste0("Actually: ", one_test$variety[1]))
  
  if (names(variety_vector[which(variety_vector == max(variety_vector))]) == one_test$variety[1]) {
    print("hooray!")
    print(j)
    accuracy_vector[j] <- 1
  }
  
}
sum(accuracy_vector[1:100])/100


my_test <- test_data %>%
  filter(Row %in% 1:100) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  filter(!word %in% wine_types)

  

