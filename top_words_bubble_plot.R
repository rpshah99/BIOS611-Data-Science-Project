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
  filter(summary_words != "these") %>%
  filter(summary_words != "then") %>%
  filter(summary_words != "about") %>%
  filter(summary_words != "upon") %>%
  filter(summary_words != "felt") %>%
  filter(summary_words != "during") %>%
  filter(summary_words != "began") %>%
  filter(summary_words != "aboard") %>%
  filter(summary_words != "about") %>%
  filter(summary_words != "above") %>%
  filter(summary_words != "across") %>%
  filter(summary_words != "after") %>%
  filter(summary_words != "against") %>%
  filter(summary_words != "along") %>%
  filter(summary_words != "amid") %>%
  filter(summary_words != "among") %>%
  filter(summary_words != "anti") %>%
  filter(summary_words != "around") %>%
  filter(summary_words != "as") %>%
  filter(summary_words != "at") %>%
  filter(summary_words != "before") %>%
  filter(summary_words != "behind") %>%
  filter(summary_words != "below") %>%
  filter(summary_words != "beneath") %>%
  filter(summary_words != "beside") %>%
  filter(summary_words != "besides") %>%
  filter(summary_words != "between") %>%
  filter(summary_words != "beyond") %>%
  filter(summary_words != "but") %>%
  filter(summary_words != "by") %>%
  filter(summary_words != "concerning") %>%
  filter(summary_words != "considering") %>%
  filter(summary_words != "despite") %>%
  filter(summary_words != "down") %>%
  filter(summary_words != "during") %>%
  filter(summary_words != "except") %>%
  filter(summary_words != "excepting") %>%
  filter(summary_words != "excluding") %>%
  filter(summary_words != "following") %>%
  filter(summary_words != "for") %>%
  filter(summary_words != "from") %>%
  filter(summary_words != "in") %>%
  filter(summary_words != "inside") %>%
  filter(summary_words != "into") %>%
  filter(summary_words != "like") %>%
  filter(summary_words != "minus") %>%
  filter(summary_words != "near") %>%
  filter(summary_words != "of") %>%
  filter(summary_words != "off") %>%
  filter(summary_words != "on") %>%
  filter(summary_words != "onto") %>%
  filter(summary_words != "opposite") %>%
  filter(summary_words != "outside") %>%
  filter(summary_words != "over") %>%
  filter(summary_words != "past") %>%
  filter(summary_words != "per") %>%
  filter(summary_words != "plus") %>%
  filter(summary_words != "regarding") %>%
  filter(summary_words != "round") %>%
  filter(summary_words != "save") %>%
  filter(summary_words != "since") %>%
  filter(summary_words != "than") %>%
  filter(summary_words != "through") %>%
  filter(summary_words != "to") %>%
  filter(summary_words != "toward") %>%
  filter(summary_words != "towards") %>%
  filter(summary_words != "under") %>%
  filter(summary_words != "underneath") %>%
  filter(summary_words != "unlike") %>%
  filter(summary_words != "until") %>%
  filter(summary_words != "up") %>%
  filter(summary_words != "upon") %>%
  filter(summary_words != "versus") %>%
  filter(summary_words != "via") %>%
  filter(summary_words != "with") %>%
  filter(summary_words != "within") %>%
  filter(summary_words != "without")

top_words_DMT <- summary_word_counts_clean %>%
  arrange(desc(Freq)) %>%
  head(20)

top_words_bubble_plot <- ggplot(top_words_DMT, 
                                aes(x = reorder(summary_words, Freq), y = Freq, 
                                    size = Freq)) +
  geom_point(color = "firebrick1", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top Words Associated With DMT Trips",
       x = "Word",
       y = "Frequency") +
  theme_grey()

ggsave("top_words_bubble_plot.png", plot = top_words_bubble_plot, width = 7,
       height = 5, dpi = 300)