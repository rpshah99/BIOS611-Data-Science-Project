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

joined_summaries_dose_salvia <- dose_data %>%
  inner_join(summaries, by = "experience_id") %>%
  group_by(substance) %>%
  filter(substance %in% c("Salvia divinorum"))

joined_summaries_dose_salvia <- joined_summaries_dose_salvia %>%
  select(-index)

joined_summaries_dose_counts_salvia <- joined_summaries_dose_salvia %>% 
  group_by(amount) %>% 
  tally()

clean_joined_summaries_dose_salvia <- na.omit(joined_summaries_dose_salvia)

all_text_summary_salvia <- paste(clean_joined_summaries_dose_salvia$summary, collapse = " ")

summary_words_salvia <- unlist(strsplit(tolower(all_text_summary_salvia), "\\W+"))

summary_words_salvia <- summary_words_salvia[summary_words_salvia != ""]

summary_words_counts_salvia <- sort(table(summary_words_salvia), decreasing = TRUE)

summary_word_counts_tibble_salvia <- 
  data.frame(summary_words_counts_salvia, 
                summary_words = names(summary_words_counts_salvia),
                Freq = as.numeric(summary_words_counts_salvia))

summary_word_counts_clean_salvia <- summary_word_counts_tibble_salvia %>% 
  filter(summary_words_salvia != "the") %>%
  filter(summary_words_salvia != "and") %>%
  filter(summary_words_salvia != "a") %>%
  filter(summary_words_salvia != "to") %>%
  filter(summary_words_salvia != "of") %>%
  filter(summary_words_salvia != "they") %>%
  filter(summary_words_salvia != "participant") %>%
  filter(summary_words_salvia != "their") %>%
  filter(summary_words_salvia != "with") %>%
  filter(summary_words_salvia != "an") %>%
  filter(summary_words_salvia != "by") %>%
  filter(summary_words_salvia != "this") %>%
  filter(summary_words_salvia != "that") %>%
  filter(summary_words_salvia != "was") %>%
  filter(summary_words_salvia != "after") %>%
  filter(summary_words_salvia != "substance") %>%
  filter(summary_words_salvia != "experienced") %>%
  filter(summary_words_salvia != "but") %>%
  filter(summary_words_salvia != "like") %>%
  filter(summary_words_salvia != "them") %>%
  filter(summary_words_salvia != "into") %>%
  filter(summary_words_salvia != "were") %>%
  filter(summary_words_salvia != "on") %>%
  filter(summary_words_salvia != "for") %>%
  filter(summary_words_salvia != "in") %>%
  filter(summary_words_salvia != "as") %>%
  filter(summary_words_salvia != "experience") %>%
  filter(summary_words_salvia != "from") %>%
  filter(summary_words_salvia != "it") %>%
  filter(summary_words_salvia != "at") %>%
  filter(summary_words_salvia != "or") %>%
  filter(summary_words_salvia != "s") %>%
  filter(summary_words_salvia != "then") %>%
  filter(summary_words_salvia != "had") %>%
  filter(summary_words_salvia != "these") %>%
  filter(summary_words_salvia != "then") %>%
  filter(summary_words_salvia != "about") %>%
  filter(summary_words_salvia != "upon") %>%
  filter(summary_words_salvia != "felt") %>%
  filter(summary_words_salvia != "during") %>%
  filter(summary_words_salvia != "began") %>%
  filter(summary_words_salvia != "aboard") %>%
  filter(summary_words_salvia != "about") %>%
  filter(summary_words_salvia != "above") %>%
  filter(summary_words_salvia != "across") %>%
  filter(summary_words_salvia != "after") %>%
  filter(summary_words_salvia != "against") %>%
  filter(summary_words_salvia != "along") %>%
  filter(summary_words_salvia != "amid") %>%
  filter(summary_words_salvia != "among") %>%
  filter(summary_words_salvia != "anti") %>%
  filter(summary_words_salvia != "around") %>%
  filter(summary_words_salvia != "as") %>%
  filter(summary_words_salvia != "at") %>%
  filter(summary_words_salvia != "before") %>%
  filter(summary_words_salvia != "behind") %>%
  filter(summary_words_salvia != "below") %>%
  filter(summary_words_salvia != "beneath") %>%
  filter(summary_words_salvia != "beside") %>%
  filter(summary_words_salvia != "besides") %>%
  filter(summary_words_salvia != "between") %>%
  filter(summary_words_salvia != "beyond") %>%
  filter(summary_words_salvia != "but") %>%
  filter(summary_words_salvia != "by") %>%
  filter(summary_words_salvia != "concerning") %>%
  filter(summary_words_salvia != "considering") %>%
  filter(summary_words_salvia != "despite") %>%
  filter(summary_words_salvia != "down") %>%
  filter(summary_words_salvia != "during") %>%
  filter(summary_words_salvia != "except") %>%
  filter(summary_words_salvia != "excepting") %>%
  filter(summary_words_salvia != "excluding") %>%
  filter(summary_words_salvia != "following") %>%
  filter(summary_words_salvia != "for") %>%
  filter(summary_words_salvia != "from") %>%
  filter(summary_words_salvia != "in") %>%
  filter(summary_words_salvia != "inside") %>%
  filter(summary_words_salvia != "into") %>%
  filter(summary_words_salvia != "like") %>%
  filter(summary_words_salvia != "minus") %>%
  filter(summary_words_salvia != "near") %>%
  filter(summary_words_salvia != "of") %>%
  filter(summary_words_salvia != "off") %>%
  filter(summary_words_salvia != "on") %>%
  filter(summary_words_salvia != "onto") %>%
  filter(summary_words_salvia != "opposite") %>%
  filter(summary_words_salvia != "outside") %>%
  filter(summary_words_salvia != "over") %>%
  filter(summary_words_salvia != "past") %>%
  filter(summary_words_salvia != "per") %>%
  filter(summary_words_salvia != "plus") %>%
  filter(summary_words_salvia != "regarding") %>%
  filter(summary_words_salvia != "round") %>%
  filter(summary_words_salvia != "save") %>%
  filter(summary_words_salvia != "since") %>%
  filter(summary_words_salvia != "than") %>%
  filter(summary_words_salvia != "through") %>%
  filter(summary_words_salvia != "to") %>%
  filter(summary_words_salvia != "toward") %>%
  filter(summary_words_salvia != "towards") %>%
  filter(summary_words_salvia != "under") %>%
  filter(summary_words_salvia != "underneath") %>%
  filter(summary_words_salvia != "unlike") %>%
  filter(summary_words_salvia != "until") %>%
  filter(summary_words_salvia != "up") %>%
  filter(summary_words_salvia != "upon") %>%
  filter(summary_words_salvia != "versus") %>%
  filter(summary_words_salvia != "via") %>%
  filter(summary_words_salvia != "with") %>%
  filter(summary_words_salvia != "within") %>%
  filter(summary_words_salvia != "without")

top_words_salvia <- summary_word_counts_clean_salvia %>%
  arrange(desc(Freq)) %>%
  head(20)

top_words_bubble_plot_salvia <- ggplot(top_words_salvia, 
                                aes(x = reorder(summary_words_salvia, Freq), y = Freq, 
                                    size = Freq)) +
  geom_point(color = "firebrick1", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top Words Associated With Salvia divinorum Trips",
       x = "Word",
       y = "Frequency") +
  theme_grey()

ggsave("plots/top_words_bubble_plot_salvia.png", 
       plot = top_words_bubble_plot_salvia,
       width = 10, height = 5, dpi = 300)
