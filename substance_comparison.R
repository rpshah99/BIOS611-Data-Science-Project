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

joined_summaries_substance_dose <- dose_data %>%
  inner_join(summaries, by = "experience_id", 
             relationship = "many-to-many") %>%
  group_by(substance)

joined_summaries_substance_dose <- dose_data %>%
  inner_join(summaries, by = "experience_id", 
             relationship = "many-to-many") %>%
  group_by(substance)

clean_joined_summaries_substance <- na.omit(joined_summaries_substance_dose)

clean_summaries_substance <- clean_joined_summaries_substance %>%
  select(-experience_id, -...1, -index, -amount, -method)

clean_summaries_substance_top <- clean_summaries_substance %>%
  filter(substance %in% c("DMT", "Salvia divinorum", "Cannabis", "Alcohol"))

summaries_substance_word_counts <- clean_summaries_substance_top %>%
  mutate(word_count = str_count(summary, "\\S+"))

summaries_substance_word_counts <- summaries_substance_word_counts %>%
  select(-summary)

substance_comparison_box_plot <- ggplot(summaries_substance_word_counts, 
                                        aes(x = substance, y = word_count, 
                                            fill = substance)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  labs(title = "Word Count by Substance",
       x = "Substance",
       y = "Word Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("substance_comparison_box_plot.png", 
       plot = substance_comparison_box_plot,
       width = 10, height = 5, dpi = 300)

