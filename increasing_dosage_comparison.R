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

clean_summaries_dose <- clean_joined_summaries_dose %>%
  select(-experience_id, -...1, -substance, -method)

desc_clean_summaries_dose <- clean_summaries_dose %>%
  arrange(desc(amount))

desc_summaries_dose <- desc_clean_summaries_dose %>%
  filter(!grepl("^[A-Za-z]+$", amount))

desc_summary_dose_counts <- desc_summaries_dose %>%
  mutate(word_count = str_count(summary, "\\S+"))

desc_summary_word_dose_counts <- desc_summary_dose_counts %>%
  select(-summary)

word_count_substance <- data.frame(c("word", "count", "substance"))

dosage_scatter_plot_comparison <- ggplot(desc_summary_word_dose_counts, 
                                      aes(x = amount, y = word_count)) +
  geom_point(size = 3, shape = 19, color = "tomato") +
  labs(title = "Dosage Comparison", x = "Dosage", y = "Word Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 4))

ggsave("dosage_scatter_plot_comparison.png", 
       plot = dosage_scatter_plot_comparison, width = 20, height = 15, 
       dpi = 300)
