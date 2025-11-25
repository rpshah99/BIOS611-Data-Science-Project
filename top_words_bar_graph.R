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

heat_joined_summaries_dose <- clean_joined_summaries_dose %>%
  select(-amount, -method, -experience_id, -...1)

heat_all_text_summary <- paste(heat_joined_summaries_dose$summary, 
                               collapse = " ")

heat_summary_words <- unlist(strsplit(tolower(heat_all_text_summary), "\\W+"))

heat_summary_words <- heat_summary_words[heat_summary_words != ""]

heat_summary_words_counts <- sort(table(heat_summary_words), decreasing = TRUE)

heat_summary_word_counts_tibble <- as.data.frame(heat_summary_words_counts)

heat_summary_word_counts_clean <- heat_summary_word_counts_tibble %>% 
  filter(heat_summary_words != "the") %>%
  filter(heat_summary_words != "and") %>%
  filter(heat_summary_words != "a") %>%
  filter(heat_summary_words != "to") %>%
  filter(heat_summary_words != "of") %>%
  filter(heat_summary_words != "they") %>%
  filter(heat_summary_words != "participant") %>%
  filter(heat_summary_words != "their") %>%
  filter(heat_summary_words != "with") %>%
  filter(heat_summary_words != "an") %>%
  filter(heat_summary_words != "by") %>%
  filter(heat_summary_words != "this") %>%
  filter(heat_summary_words != "that") %>%
  filter(heat_summary_words != "was") %>%
  filter(heat_summary_words != "after") %>%
  filter(heat_summary_words != "substance") %>%
  filter(heat_summary_words != "experienced") %>%
  filter(heat_summary_words != "but") %>%
  filter(heat_summary_words != "like") %>%
  filter(heat_summary_words != "them") %>%
  filter(heat_summary_words != "into") %>%
  filter(heat_summary_words != "were") %>%
  filter(heat_summary_words != "on") %>%
  filter(heat_summary_words != "for") %>%
  filter(heat_summary_words != "in") %>%
  filter(heat_summary_words != "as") %>%
  filter(heat_summary_words != "experience") %>%
  filter(heat_summary_words != "from") %>%
  filter(heat_summary_words != "it") %>%
  filter(heat_summary_words != "at") %>%
  filter(heat_summary_words != "or") %>%
  filter(heat_summary_words != "s") %>%
  filter(heat_summary_words != "then") %>%
  filter(heat_summary_words != "had") %>%
  filter(heat_summary_words != "these") %>%
  filter(heat_summary_words != "about") %>%
  filter(heat_summary_words != "upon") %>%
  filter(heat_summary_words != "felt") %>%
  filter(heat_summary_words != "during") %>%
  filter(heat_summary_words != "began") %>%
  filter(heat_summary_words != "aboard") %>%
  filter(heat_summary_words != "about") %>%
  filter(heat_summary_words != "above") %>%
  filter(heat_summary_words != "across") %>%
  filter(heat_summary_words != "after") %>%
  filter(heat_summary_words != "against") %>%
  filter(heat_summary_words != "along") %>%
  filter(heat_summary_words != "amid") %>%
  filter(heat_summary_words != "among") %>%
  filter(heat_summary_words != "anti") %>%
  filter(heat_summary_words != "around") %>%
  filter(heat_summary_words != "as") %>%
  filter(heat_summary_words != "at") %>%
  filter(heat_summary_words != "before") %>%
  filter(heat_summary_words != "behind") %>%
  filter(heat_summary_words != "below") %>%
  filter(heat_summary_words != "beneath") %>%
  filter(heat_summary_words != "beside") %>%
  filter(heat_summary_words != "besides") %>%
  filter(heat_summary_words != "between") %>%
  filter(heat_summary_words != "beyond") %>%
  filter(heat_summary_words != "but") %>%
  filter(heat_summary_words != "by") %>%
  filter(heat_summary_words != "concerning") %>%
  filter(heat_summary_words != "considering") %>%
  filter(heat_summary_words != "despite") %>%
  filter(heat_summary_words != "down") %>%
  filter(heat_summary_words != "during") %>%
  filter(heat_summary_words != "except") %>%
  filter(heat_summary_words != "excepting") %>%
  filter(heat_summary_words != "excluding") %>%
  filter(heat_summary_words != "following") %>%
  filter(heat_summary_words != "for") %>%
  filter(heat_summary_words != "from") %>%
  filter(heat_summary_words != "in") %>%
  filter(heat_summary_words != "inside") %>%
  filter(heat_summary_words != "into") %>%
  filter(heat_summary_words != "like") %>%
  filter(heat_summary_words != "minus") %>%
  filter(heat_summary_words != "near") %>%
  filter(heat_summary_words != "of") %>%
  filter(heat_summary_words != "off") %>%
  filter(heat_summary_words != "on") %>%
  filter(heat_summary_words != "onto") %>%
  filter(heat_summary_words != "opposite") %>%
  filter(heat_summary_words != "outside") %>%
  filter(heat_summary_words != "over") %>%
  filter(heat_summary_words != "past") %>%
  filter(heat_summary_words != "per") %>%
  filter(heat_summary_words != "plus") %>%
  filter(heat_summary_words != "regarding") %>%
  filter(heat_summary_words != "round") %>%
  filter(heat_summary_words != "save") %>%
  filter(heat_summary_words != "since") %>%
  filter(heat_summary_words != "than") %>%
  filter(heat_summary_words != "through") %>%
  filter(heat_summary_words != "to") %>%
  filter(heat_summary_words != "toward") %>%
  filter(heat_summary_words != "towards") %>%
  filter(heat_summary_words != "under") %>%
  filter(heat_summary_words != "underneath") %>%
  filter(heat_summary_words != "unlike") %>%
  filter(heat_summary_words != "until") %>%
  filter(heat_summary_words != "up") %>%
  filter(heat_summary_words != "upon") %>%
  filter(heat_summary_words != "versus") %>%
  filter(heat_summary_words != "via") %>%
  filter(heat_summary_words != "with") %>%
  filter(heat_summary_words != "within") %>%
  filter(heat_summary_words != "without")

heat_summary_word_counts_clean$substance <- "DMT"

heat_top_words_DMT <- heat_summary_word_counts_clean %>%
  arrange(desc(Freq)) %>%
  head(20)

heatmap_top_words <- heat_top_words_DMT %>%
  filter(heat_summary_words %in% heat_top_words_DMT) %>%
  mutate(
    heat_summary_words = factor(heat_summary_words, levels = heat_top_words_DMT),
    Freq = factor(Freq)
  )

top_words_bar_plot <- ggplot(heat_top_words_DMT, aes(x = heat_summary_words, 
                               y = Freq)) +
  geom_bar(fill = "springgreen2", stat = "identity") +
  labs(x = "Word", y = "Frequency") +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("top_words_bar_plot.png", plot = top_words_bar_plot, width =  7,
       height = 5, dpi = 300)