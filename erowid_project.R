library(tidyverse)

summaries <- readr::read_csv("summaries.csv")
dose_data <- readr::read_csv("doses_data.csv")
experience_data <- readr::read_csv("experience_data.csv")
experience_urls <- readr::read_csv("experience_urls.csv")

joined_summaries_dose <- dose_data %>%
  inner_join(summaries, by = "experience_id") %>%
  group_by(substance) %>%
  filter(substance %in% c("DMT"))

joined_summaries_dose <- joined_summaries_dose %>%
  select(-X1, -index)

joined_summaries_dose_counts <- joined_summaries_dose %>% 
  group_by(amount) %>% 
  tally()

clean_joined_summaries_dose <- na.omit(joined_summaries_dose)

all_text_summary <- paste(clean_joined_summaries_dose$summary, collapse = " ")

summary_words <- unlist(strsplit(tolower(all_text_summary), "\\W+"))

summary_words <- summary_words[summary_words != ""]

summary_words_counts <- sort(table(summary_words), decreasing = TRUE)

summary_word_counts_tibble <- as.data.frame(summary_words_counts)


