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

joined_summaries_dose_DMT <- joined_summaries_dose %>%
  select(-index)

joined_summaries_dose_counts_DMT <- joined_summaries_dose_DMT %>% 
  group_by(amount) %>% 
  tally()

clean_joined_summaries_dose_DMT <- na.omit(joined_summaries_dose_counts_DMT)

all_text_summary_DMT <- paste(clean_joined_summaries_dose_DMT$summary, collapse = " ")

summary_words_DMT <- unlist(strsplit(tolower(all_text_summary_DMT), "\\W+"))

summary_words_DMT <- summary_words_DMT[summary_words_DMT != ""]

summary_words_counts_DMT <- sort(table(summary_words_DMT), decreasing = TRUE)

summary_word_counts_tibble_DMT <- 
  data.frame(summary_words_counts_DMT, 
             summary_words_DMT = names(summary_words_counts_DMT),
             Freq = as.numeric(summary_words_counts_DMT))

summary_word_counts_clean_DMT <- summary_word_counts_tibble_DMT %>% 
  filter(summary_words_DMT != "the") %>%
  filter(summary_words_DMT != "and") %>%
  filter(summary_words_DMT != "a") %>%
  filter(summary_words_DMT != "to") %>%
  filter(summary_words_DMT != "of") %>%
  filter(summary_words_DMT != "they") %>%
  filter(summary_words_DMT != "participant") %>%
  filter(summary_words_DMT != "their") %>%
  filter(summary_words_DMT != "with") %>%
  filter(summary_words_DMT != "an") %>%
  filter(summary_words_DMT != "by") %>%
  filter(summary_words_DMT != "this") %>%
  filter(summary_words_DMT != "that") %>%
  filter(summary_words_DMT != "was") %>%
  filter(summary_words_DMT != "after") %>%
  filter(summary_words_DMT != "substance") %>%
  filter(summary_words_DMT != "experienced") %>%
  filter(summary_words_DMT != "but") %>%
  filter(summary_words_DMT != "like") %>%
  filter(summary_words_DMT != "them") %>%
  filter(summary_words_DMT != "into") %>%
  filter(summary_words_DMT != "were") %>%
  filter(summary_words_DMT != "on") %>%
  filter(summary_words_DMT != "for") %>%
  filter(summary_words_DMT != "in") %>%
  filter(summary_words_DMT != "as") %>%
  filter(summary_words_DMT != "experience") %>%
  filter(summary_words_DMT != "from") %>%
  filter(summary_words_DMT != "it") %>%
  filter(summary_words_DMT != "at") %>%
  filter(summary_words_DMT != "or") %>%
  filter(summary_words_DMT != "s") %>%
  filter(summary_words_DMT != "then") %>%
  filter(summary_words_DMT != "had") %>%
  filter(summary_words_DMT != "these") %>%
  filter(summary_words_DMT != "then") %>%
  filter(summary_words_DMT != "about") %>%
  filter(summary_words_DMT != "upon") %>%
  filter(summary_words_DMT != "felt") %>%
  filter(summary_words_DMT != "during") %>%
  filter(summary_words_DMT != "began") %>%
  filter(summary_words_DMT != "aboard") %>%
  filter(summary_words_DMT != "about") %>%
  filter(summary_words_DMT != "above") %>%
  filter(summary_words_DMT != "across") %>%
  filter(summary_words_DMT != "after") %>%
  filter(summary_words_DMT != "against") %>%
  filter(summary_words_DMT != "along") %>%
  filter(summary_words_DMT != "amid") %>%
  filter(summary_words_DMT != "among") %>%
  filter(summary_words_DMT != "anti") %>%
  filter(summary_words_DMT != "around") %>%
  filter(summary_words_DMT != "as") %>%
  filter(summary_words_DMT != "at") %>%
  filter(summary_words_DMT != "before") %>%
  filter(summary_words_DMT != "behind") %>%
  filter(summary_words_DMT != "below") %>%
  filter(summary_words_DMT != "beneath") %>%
  filter(summary_words_DMT != "beside") %>%
  filter(summary_words_DMT != "besides") %>%
  filter(summary_words_DMT != "between") %>%
  filter(summary_words_DMT != "beyond") %>%
  filter(summary_words_DMT != "but") %>%
  filter(summary_words_DMT != "by") %>%
  filter(summary_words_DMT != "concerning") %>%
  filter(summary_words_DMT != "considering") %>%
  filter(summary_words_DMT != "despite") %>%
  filter(summary_words_DMT != "down") %>%
  filter(summary_words_DMT != "during") %>%
  filter(summary_words_DMT != "except") %>%
  filter(summary_words_DMT != "excepting") %>%
  filter(summary_words_DMT != "excluding") %>%
  filter(summary_words_DMT != "following") %>%
  filter(summary_words_DMT != "for") %>%
  filter(summary_words_DMT != "from") %>%
  filter(summary_words_DMT != "in") %>%
  filter(summary_words_DMT != "inside") %>%
  filter(summary_words_DMT != "into") %>%
  filter(summary_words_DMT != "like") %>%
  filter(summary_words_DMT != "minus") %>%
  filter(summary_words_DMT != "near") %>%
  filter(summary_words_DMT != "of") %>%
  filter(summary_words_DMT != "off") %>%
  filter(summary_words_DMT != "on") %>%
  filter(summary_words_DMT != "onto") %>%
  filter(summary_words_DMT != "opposite") %>%
  filter(summary_words_DMT != "outside") %>%
  filter(summary_words_DMT != "over") %>%
  filter(summary_words_DMT != "past") %>%
  filter(summary_words_DMT != "per") %>%
  filter(summary_words_DMT != "plus") %>%
  filter(summary_words_DMT != "regarding") %>%
  filter(summary_words_DMT != "round") %>%
  filter(summary_words_DMT != "save") %>%
  filter(summary_words_DMT != "since") %>%
  filter(summary_words_DMT != "than") %>%
  filter(summary_words_DMT != "through") %>%
  filter(summary_words_DMT != "to") %>%
  filter(summary_words_DMT != "toward") %>%
  filter(summary_words_DMT != "towards") %>%
  filter(summary_words_DMT != "under") %>%
  filter(summary_words_DMT != "underneath") %>%
  filter(summary_words_DMT != "unlike") %>%
  filter(summary_words_DMT != "until") %>%
  filter(summary_words_DMT != "up") %>%
  filter(summary_words_DMT != "upon") %>%
  filter(summary_words_DMT != "versus") %>%
  filter(summary_words_DMT != "via") %>%
  filter(summary_words_DMT != "with") %>%
  filter(summary_words_DMT != "within") %>%
  filter(summary_words_DMT != "without")

top_words_DMT <- summary_word_counts_clean_DMT %>%
  arrange(desc(Freq)) %>%
  head(20)

top_words_bubble_plot_DMT <- ggplot(top_words_DMT, 
                                aes(x = reorder(summary_words_DMT, Freq), y = Freq, 
                                    size = Freq)) +
  geom_point(color = "firebrick1", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top Words Associated With DMT Trips",
       x = "Word",
       y = "Frequency") +
  theme_grey()

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

ggsave("plots/top_words_bubble_plot_DMT.png", 
       plot = top_words_bubble_plot_DMT,
       width = 10, height = 5, dpi = 300)

ggsave("plots/top_words_bubble_plot_salvia.png", 
       plot = top_words_bubble_plot_salvia,
       width = 10, height = 5, dpi = 300)
