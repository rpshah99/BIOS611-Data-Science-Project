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

summary_word_counts_clean <- summary_word_counts_tibble %>% 
  filter(summary_words != "the") %>%
  filter(summary_words != "and") %>%
  filter(summary_words != "a") %>%
  filter(summary_words != "to") %>%
  filter(summary_words != "of") %>%
  filter(summary_words != "they") %>%
  filter(summary_words != "participant") %>%
  filter(summary_words != "their") %>%
  filter(summary_words != "with") %>%
  filter(summary_words != "an") %>%
  filter(summary_words != "by") %>%
  filter(summary_words != "this") %>%
  filter(summary_words != "that") %>%
  filter(summary_words != "was") %>%
  filter(summary_words != "after") %>%
  filter(summary_words != "substance") %>%
  filter(summary_words != "experienced") %>%
  filter(summary_words != "but") %>%
  filter(summary_words != "like") %>%
  filter(summary_words != "them") %>%
  filter(summary_words != "into") %>%
  filter(summary_words != "were") %>%
  filter(summary_words != "on") %>%
  filter(summary_words != "for") %>%
  filter(summary_words != "in") %>%
  filter(summary_words != "as") %>%
  filter(summary_words != "experience") %>%
  filter(summary_words != "from") %>%
  filter(summary_words != "it") %>%
  filter(summary_words != "at") %>%
  filter(summary_words != "or") %>%
  filter(summary_words != "s") %>%
  filter(summary_words != "then") %>%
  filter(summary_words != "had") %>%
  filter(summary_words != "these")

top_words_DMT <- summary_word_counts_clean %>%
  arrange(desc(Freq)) %>%
  head(20)

ggplot(top_words_DMT, aes(x = reorder(summary_words, Freq), y = Freq, size = Freq)) +
  geom_point(color = "firebrick1", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top Words Associated With DMT Trips",
       x = "Word",
       y = "Frequency") +
  theme_grey()

