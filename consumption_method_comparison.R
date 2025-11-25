library(tidyverse)
library(tidytext)
library(stopwords)
library(DBI)
library(stringi)
library(plotly)


summaries <- readr::read_csv("summaries.csv")
dose_data <- readr::read_csv("doses_data.csv")
experience_data <- readr::read_csv("experience_data.csv")
experience_urls <- readr::read_csv("experience_urls.csv")

joined_summaries_dose <- dose_data %>%
  inner_join(summaries, by = "experience_id") %>%
  group_by(substance) %>%
  filter(substance %in% c("DMT"))

joined_summaries_dose <- joined_summaries_dose %>%
  select(-index)

joined_summaries_dose_counts <- joined_summaries_dose %>% 
  group_by(amount) %>% 
  tally()

clean_joined_summaries_dose <- na.omit(joined_summaries_dose)

clean_summaries_method <- clean_joined_summaries_dose %>%
  select(-experience_id, -...1, -substance, -amount)

summaries_method_word_counts <- clean_summaries_method %>%
  mutate(word_count = str_count(summary, "\\S+"))

summaries_method_word_counts <- summaries_method_word_counts %>%
  select(-summary)

consumption_method_box_plot <- ggplot(summaries_method_word_counts, 
                                      aes(x = method, y = word_count, 
                                         fill = method)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  labs(title = "Word Count of Summaries by Method",
       x = "Method of Consumption",
       y = "Word Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("consumption_method_box_plot.png", plot = consumption_method_box_plot,
       width = 10, height = 5, dpi = 300)