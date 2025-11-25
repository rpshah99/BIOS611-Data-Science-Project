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
  filter(heat_summary_words != "began")

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

heat_joined_summaries_dose <- clean_joined_summaries_dose %>%
  select(-amount, -method, -experience_id, -...1)

heat_all_text_summary <- paste(heat_joined_summaries_dose$summary, 
                               collapse = " ")

heat_summary_words <- unlist(strsplit(tolower(heat_all_text_summary), "\\W+"))

heat_summary_words <- heat_summary_words[heat_summary_words != ""]

heat_summary_words_counts <- sort(table(heat_summary_words), decreasing = TRUE)

heat_summary_word_counts_tibble <- as.data.frame(heat_summary_words_counts)

list_prepositions <- c("aboard", "about", "above", "across", "after", "against",
                       "along", "amid", "among", "anti", "around", "as", "at",
                       "before", "behind", "below", "beneath", "beside",
                       "besides", "between", "beyond", "but", "by", 
                       "concerning", "considering", "despite", "down", "during",
                       "except", "excepting", "excluding", "following", "for",
                       "from", "in", "inside", "into", "like", "minus", "near",
                       "of", "off", "on", "onto", "opposite", "outside", "over",
                       "past", "per", "plus", "regarding", "round", "save",
                       "since", "than", "through", "to", "toward", "towards",
                       "under", "underneath", "unlike", "until", "up", "upon", 
                       "versus", "via", "with", "within", "without")
  filter(!(heat_summary_words %in% list_of_prepositions))

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
  filter(heat_summary_words != "began")

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

ggplot(heat_top_words_DMT, aes(x = heat_summary_words, 
                               y = Freq)) +
  geom_bar(fill = "black", stat = "identity") +
  labs(x = "Word", y = "Frequency") +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

three_d_summaries_desc_dose <- plot_ly(
  desc_summary_word_dose_counts,
  x = ~substance,      
  y = ~amount,       
  z = ~word_count,      
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 5, color = ~word_count, colorscale = "Viridis", 
                showscale = TRUE, title = "Comparison of Increasing DMT
                Dosage and Word Count of Trip Summaries")
)

clean_summaries_method <- clean_joined_summaries_dose %>%
  select(-experience_id, -...1, -substance, -amount)

summaries_method_word_counts <- clean_summaries_method %>%
  mutate(word_count = str_count(summary, "\\S+"))

summaries_method_word_counts <- summaries_method_word_counts %>%
  select(-summary)

ggplot(summaries_method_word_counts, aes(x = method, y = word_count, 
                                         fill = method)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  labs(title = "Word Count of Summaries by Method",
       x = "Method of Consumption",
       y = "Word Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

ggplot(summaries_substance_word_counts, aes(x = substance, y = word_count, 
                                            fill = substance)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
  labs(title = "Word Count by Substance",
       x = "Substance",
       y = "Word Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

