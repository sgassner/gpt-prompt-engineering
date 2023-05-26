#------------------------------------------------------------------------------#
# Master-Arbeit
#
# Titel:        GPT Prompt Engineering zur Kategorisierung der Stimmung 
#               von Anlegerinnen und Anlegern
# Autor:        Sandro Gassner
# Datum:        21.08.2023
#------------------------------------------------------------------------------#

# Definition des Working Directories
setwd("~/Documents/R/MA")

# Packages laden
library(tidyverse)
library(httr) # Zugriff auf das OpenAI API
library(cld2) # Sprachermittlung der Posts
library(tidytext) # NLP Funktionen
library(textdata) # NLP Funktionen
library(SnowballC) # Stemming

################################################################################
### Daten laden
################################################################################

# Gelabelte Tweets laden
data_labeled <- read.csv("tweets_aapl_labeled.csv")

# Datensatz als Tibble formatieren
data_labeled <- as_tibble(data_labeled)

################################################################################
### Bereinigung der Daten
################################################################################

# Struktur und Zusammenfassung der Daten
str(data_labeled)
summary(data_labeled)

# Nicht benötigte Spalten entfernen
data_labeled <- data_labeled %>% select(c("X_unit_id", "text", "sentiment"))

# ID und Sentiment Spalte umbenennen
colnames(data_labeled)[which(colnames(data_labeled) == "X_unit_id")] <- "ID"
colnames(data_labeled)[which(colnames(data_labeled) == "sentiment")] <- "label"

# Text Spalte in UTF-8 konvertieren
data_labeled$text <- iconv(data_labeled$text, to = "UTF-8//IGNORE")

# URLs entfernen
data_labeled$text <- gsub("(https?://)?(www\\.)?\\S+\\.\\S+", "", 
                          data_labeled$text)

# Hashtags entfernen
data_labeled$text <- gsub("#", "", data_labeled$text)

# Dollarzeichen vor Ticker entfernen
data_labeled$text <- gsub("\\$([A-Za-z]+)", "\\1", data_labeled$text)

# Markierungen von Personen (@XYZ) entfernen
data_labeled$text <- gsub("@\\S+", "", data_labeled$text)

# Sprachermittlung der Posts
data_labeled$language <- detect_language(data_labeled$text, plain_text = TRUE)

# Englischsprachige Posts herausfiltern
data_labeled <- data_labeled %>% filter(language == "en")
data_labeled <- select(data_labeled, -language)

# Doppelte Leerzeichen entfernen
data_labeled$text <- gsub("  ", " ", data_labeled$text)

# Nur klar poitive oder neaative Kategorisierungen herausfiltern
data_labeled <- data_labeled[grepl("1|5", data_labeled$label), ]

# Kategorisierung in "1 = positiv" und "0 = negativ" umbenennen
data_labeled$label <- gsub("1", "0", data_labeled$label)
data_labeled$label <- gsub("5", "1", data_labeled$label)
data_labeled$label <- as.numeric(data_labeled$label)

# Anzahl der manuell gelabelten Sentiments zählen
data_labeled %>% group_by(label) %>% count()

################################################################################
### Sentiment-Analyse mit konventionellen Methoden
################################################################################

#------------------------------------------------------------------------------#
# NLP Pre-Processing der gelabelten Daten
#------------------------------------------------------------------------------#

# Tokenization und Normalization (lower-casing)
data_labeled_tokens <- data_labeled %>% 
  unnest_tokens(output = "word", token = "words", input = text) 

# Entfernung von Stop Words
data_labeled_tokens <- data_labeled_tokens %>% anti_join(stop_words)

# Übersicht der erstellten Tokens ohne Stop Words
print(data_labeled_tokens %>% count(word, sort = TRUE), n = 50)

# Stemming
data_labeled_tokens <- data_labeled_tokens %>% mutate(word = wordStem(word))

# Übersicht der erstellten Tokens nach Stemming
print(data_labeled_tokens %>% count(word, sort = TRUE), n = 10)

#------------------------------------------------------------------------------#
# Loughran and McDonald Dictionary (L&M)
#------------------------------------------------------------------------------#

# L&M Dictionary laden
lm_dict <- get_sentiments("loughran")

# Stemming des L&M Dictionary
lm_dict <- lm_dict %>% mutate(word = wordStem(word))

# Tokens mit L&M Dictionary verbinden
lm_nlp <- data_labeled_tokens %>% inner_join(lm_dict, by = "word")

# Sentiment Score für jeden Post berechnen
lm_sentiment_score <- lm_nlp %>% group_by(ID, sentiment) %>% count()

# In wide-format konvertieren
lm_sentiment_score_wide <- lm_sentiment_score %>% 
  pivot_wider(names_from = sentiment, values_from = n)

# NA's mit 0 ersetzen
lm_sentiment_score_wide[is.na(lm_sentiment_score_wide)] <- 0

# Wenn mehr positiv als negativ = 1, sonst 0
lm_sentiment_score_wide$pos_neg <- lm_sentiment_score_wide$positive -
  lm_sentiment_score_wide$negative
lm_sentiment_score_wide$lm_label <- NA
lm_sentiment_score_wide$lm_label[lm_sentiment_score_wide$pos_neg > 0] <- 1
lm_sentiment_score_wide$lm_label[lm_sentiment_score_wide$pos_neg < 0] <- 0
lm_sentiment_score_wide <- 
  lm_sentiment_score_wide[complete.cases(lm_sentiment_score_wide), ]

# Nur ID und L&M Kategorisierung herausfiltern
lm_label <- lm_sentiment_score_wide %>% select(c("ID", "lm_label"))

#------------------------------------------------------------------------------#
# NRC Word-Emotion Association Lexicon
#------------------------------------------------------------------------------#

# NRC Dictionary laden
nrc_dict <- get_sentiments("nrc")

# Stemming des NRC Dictionary
nrc_dict <- nrc_dict %>% mutate(word = wordStem(word))

# Tokens mit NRC Dictionary verbinden
nrc_nlp <- data_labeled_tokens %>% inner_join(nrc_dict, by = "word")

# Sentiment Score für jeden Post berechnen
nrc_sentiment_score <- nrc_nlp %>% group_by(ID, sentiment) %>% count()

# In wide-format konvertieren
nrc_sentiment_score_wide <- nrc_sentiment_score %>% 
  pivot_wider(names_from = sentiment, values_from = n)

# NA's mit 0 ersetzen
nrc_sentiment_score_wide[is.na(nrc_sentiment_score_wide)] <- 0

# Wenn mehr positiv als negativ = 1, sonst 0
nrc_sentiment_score_wide$pos_neg <- nrc_sentiment_score_wide$positive -
  nrc_sentiment_score_wide$negative
nrc_sentiment_score_wide$nrc_label <- NA
nrc_sentiment_score_wide$nrc_label[nrc_sentiment_score_wide$pos_neg > 0] <- 1
nrc_sentiment_score_wide$nrc_label[nrc_sentiment_score_wide$pos_neg < 0] <- 0
nrc_sentiment_score_wide <- 
  nrc_sentiment_score_wide[complete.cases(nrc_sentiment_score_wide), ]

# Nur ID und NRC Kategorisierung herausfiltern
nrc_label <- nrc_sentiment_score_wide %>% select(c("ID", "nrc_label"))

#------------------------------------------------------------------------------#
# AFINN Dictionary
#------------------------------------------------------------------------------#

# AFINN Dictionary laden
afinn_dict <- get_sentiments("afinn")

# Stemming des AFINN Dictionary
afinn_dict <- afinn_dict %>% mutate(word = wordStem(word))

# Tokens mit AFINN Dictionary verbinden
afinn_nlp <- data_labeled_tokens %>% inner_join(afinn_dict, by = "word")

# Sentiment Score für jedes Datum und jede Aktie summieren
afinn_sentiment_score <- afinn_nlp %>% group_by(ID) %>% 
  summarise(sentiment = sum(value))

# Wenn mehr positiv als negativ = 1, sonst 0
afinn_sentiment_score$afinn_label <- NA
afinn_sentiment_score$afinn_label[afinn_sentiment_score$sentiment > 0] <- 1
afinn_sentiment_score$afinn_label[afinn_sentiment_score$sentiment < 0] <- 0
afinn_sentiment_score <- 
  afinn_sentiment_score[complete.cases(afinn_sentiment_score), ]

# Nur ID und AFINN Kategorisierung herausfiltern
afinn_label <- afinn_sentiment_score %>% select(c("ID", "afinn_label"))

#------------------------------------------------------------------------------#
# BING
#------------------------------------------------------------------------------#

# Dictionary laden
bing_dict <- get_sentiments("bing")

# Stemming des BING Dictionary
bing_dict <- bing_dict %>% mutate(word = wordStem(word))

# Tokens mit BING Dictionary verbinden
bing_nlp <- data_labeled_tokens %>% inner_join(bing_dict, by = "word")

# Sentiment Score für jedes Datum und jede Aktie berechnen
bing_sentiment_score <- bing_nlp %>% group_by(ID) %>% count(sentiment)

# In wide-format konvertieren
bing_sentiment_score_wide <- bing_sentiment_score %>% 
  pivot_wider(names_from = sentiment, values_from = n)

# NA's mit 0 ersetzen
bing_sentiment_score_wide[is.na(bing_sentiment_score_wide)] <- 0

# Wenn mehr positiv als negativ = 1, sonst 0
bing_sentiment_score_wide$pos_neg <- bing_sentiment_score_wide$positive -
  bing_sentiment_score_wide$negative
bing_sentiment_score_wide$bing_label <- NA
bing_sentiment_score_wide$bing_label[bing_sentiment_score_wide$pos_neg > 0] <- 1
bing_sentiment_score_wide$bing_label[bing_sentiment_score_wide$pos_neg < 0] <- 0
bing_sentiment_score_wide <- 
  bing_sentiment_score_wide[complete.cases(bing_sentiment_score_wide), ]

# Nur ID und BING Kategorisierung herausfiltern
bing_label <- bing_sentiment_score_wide %>% select(c("ID", "bing_label"))

################################################################################
### Sentiment Analysis with GPT
################################################################################

#------------------------------------------------------------------------------#
# OpenAI API und Prompt-Funktion einrichten
#------------------------------------------------------------------------------#

# OpenAI API Key definieren
api_key <- "KEY"

# Prompt Funktion erstellen
ask_gpt <- function(prompt) {
  response <- POST(url = "https://api.openai.com/v1/chat/completions",
                   add_headers(Authorization = paste("Bearer", api_key)),
                   content_type_json(),
                   encode = "json",
                   body = list(model = "gpt-3.5-turbo",
                               max_tokens = 10,
                               temperature = 0,
                               messages = list(list(role = "user",
                                                    content = prompt
                               ))
                   )
  )

  # Falls Completion = 0 -> "Keine Antwort von API"
  if (!is.null(content(response)$choices) &&
      length(content(response)$choices) > 0) {
    str_trim(content(response)$choices[[1]]$message$content)
  } else {
    # Text, falls Completion = 0
    "Keine Antwort von API"
  }
}

#------------------------------------------------------------------------------#
# GPT (0S)
#------------------------------------------------------------------------------#

# Prompt definieren
prompt_0s <- paste("The following tweet contains an opinion on a stock, a company, a product or the market. Classify the tweet into positive or negative. Only return positive or negative.",
                   "Tweet about AAPL:", sep = "\n")

# Tabelle mit Prompts erstellen
gpt_sentiments <- data_labeled
gpt_sentiments$prompt_0s <- paste(paste(prompt_0s, gpt_sentiments$text), 
                                  "Sentiment:", sep = "\n")

# Leere Spalte für GPT Klassifikation erstellen
gpt_sentiments$completion_0s <- NA

# Jeden Post mit GPT klassifizieren
for (i in 1:length(gpt_sentiments$prompt_0s)) {
  # Completion als Character speichern
  compl <- as.character(ask_gpt(gpt_sentiments$prompt_0s[i]))
  # Zeilennummer und Completion anzeigen (Fortschrittsanzeige)
  print(paste(i, compl))
  # Completion im Data Frame speichern
  gpt_sentiments$completion_0s[i] <- compl
}

# Format der Completions anpassen
gpt_sentiments$completion_0s <- tolower(gpt_sentiments$completion_0s)

# Datensatz mit Completions speichern
write_csv(gpt_sentiments, "gpt_sentiments_labeled_temp_00.csv")

# Art der Completions anschauen
unique(gpt_sentiments$completion_0s)

# Positive und negative Klassifikationen herausfiltern
gpt_0s_label <- gpt_sentiments %>% select(c("ID", "label", "completion_0s"))
gpt_0s_label <- gpt_0s_label %>% 
  filter(completion_0s %in% c("positive", "negative"))

# Positive = 1, Negative = 0
gpt_0s_label$completion_0s[gpt_0s_label$completion_0s == "positive"] <- 1
gpt_0s_label$completion_0s[gpt_0s_label$completion_0s == "negative"] <- 0
gpt_0s_label$completion_0s <- as.numeric(gpt_0s_label$completion_0s)

#------------------------------------------------------------------------------#
# GPT (1S)
#------------------------------------------------------------------------------#

# Prompt definieren
prompt_1s <- paste("The following tweet contains an opinion on a stock, a company, a product or the market. Classify the tweet into positive or negative. Only return positive or negative.",
                   "Tweet 1 about AAPL: I am bullish on Apple.",
                   "Sentiment: positive",
                   "##",
                   "Tweet 2 about AAPL:",
                   sep = "\n")

# Spalte mit Prompts erstellen
gpt_sentiments$prompt_1s <- paste(paste(prompt_1s, gpt_sentiments$text), 
                                  "Sentiment:", sep = "\n")

# Leere Spalte für GPT Klassifikation erstellen
gpt_sentiments$completion_1s <- NA

# Jeden Post mit GPT klassifizieren
for (i in 1:length(gpt_sentiments$prompt_1s)) {
  # Completion als Character speichern
  compl <- as.character(ask_gpt(gpt_sentiments$prompt_1s[i]))
  # Zeilennummer und Completion anzeigen (Fortschrittsanzeige)
  print(paste(i, compl))
  # Completion im Data Frame speichern
  gpt_sentiments$completion_1s[i] <- compl
}

# Format der Completions anpassen
gpt_sentiments$completion_1s <- tolower(gpt_sentiments$completion_1s)

# Datensatz mit Completions speichern
write_csv(gpt_sentiments, "gpt_sentiments_labeled.csv")

# Art der Completions anschauen
unique(gpt_sentiments$completion_1s)

# Positive und negative Klassifikationen herausfiltern
gpt_1s_label <- gpt_sentiments %>% select(c("ID", "label", "completion_1s"))
gpt_1s_label <- gpt_1s_label %>% 
  filter(completion_1s %in% c("positive", "negative"))

# Positive = 1, Negative = 0
gpt_1s_label$completion_1s[gpt_1s_label$completion_1s == "positive"] <- 1
gpt_1s_label$completion_1s[gpt_1s_label$completion_1s == "negative"] <- 0
gpt_1s_label$completion_1s <- as.numeric(gpt_1s_label$completion_1s)

#------------------------------------------------------------------------------#
# GPT (FS)
#------------------------------------------------------------------------------#

# Prompt definieren
prompt_fs <- paste("The following tweet contains an opinion on a stock, a company, a product or the market. Classify the tweet into positive or negative. Only return positive or negative.",
                   "Tweet 1 about AAPL: I am bullish on Apple.",
                   "Sentiment: positive",
                   "##",
                   "Tweet 2 about AAPL: I hate the new iPhone 14.",
                   "Sentiment: negative",
                   "##",
                   "Tweet 3 about AAPL:",
                   sep = "\n")

# Spalte mit Prompts erstellen
gpt_sentiments$prompt_fs <- paste(paste(prompt_fs, gpt_sentiments$text), 
                                  "Sentiment:", sep = "\n")

# Leere Spalte für GPT Klassifikation erstellen
gpt_sentiments$completion_fs <- NA

# Jeden Post mit GPT klassifizieren
for (i in 1:length(gpt_sentiments$prompt_fs)) {
  # Completion als Character speichern
  compl <- as.character(ask_gpt(gpt_sentiments$prompt_fs[i]))
  # Zeilennummer und Completion anzeigen (Fortschrittsanzeige)
  print(paste(i, compl))
  # Completion im Data Frame speichern
  gpt_sentiments$completion_fs[i] <- compl
}

# Format der Completions anpassen
gpt_sentiments$completion_fs <- tolower(gpt_sentiments$completion_fs)

# Datensatz mit Completions speichern
write_csv(gpt_sentiments, "gpt_sentiments_labeled.csv")

# Art der Completions anschauen
unique(gpt_sentiments$completion_fs)

# Positive und negative Klassifikationen herausfiltern
gpt_fs_label <- gpt_sentiments %>% select(c("ID", "label", "completion_fs"))
gpt_fs_label <- gpt_fs_label %>% 
  filter(completion_fs %in% c("positive", "negative"))

# Positive = 1, Negative = 0
gpt_fs_label$completion_fs[gpt_fs_label$completion_fs == "positive"] <- 1
gpt_fs_label$completion_fs[gpt_fs_label$completion_fs == "negative"] <- 0
gpt_fs_label$completion_fs <- as.numeric(gpt_fs_label$completion_fs)

################################################################################
### Validierung der unterschiedlichen Methoden
################################################################################

#------------------------------------------------------------------------------#
# Loughran and McDonald Dictionary (L&M) Validierung
#------------------------------------------------------------------------------#

# Prozent der analysierbaren Posts berechnen
length(lm_label$lm_label) / length(data_labeled$label) * 100

# Anzahl positiven und negativen Posts gemäss L&M
lm_label %>% group_by(lm_label) %>% count()

# L&M Kategorisierung mit gelabeltem Datensatz zusammenführen
lm_label <- merge(data_labeled, lm_label, by = "ID")

# Differenz zwischen L&M und korrektem Label berechnen
lm_label$diff <- lm_label$label - lm_label$lm_label

# Differenz als korrekt, falsch-positiv oder falsch-negativ benennen
lm_label$diff[lm_label$diff == 0] <- "korrekt"
lm_label$diff[lm_label$diff == -1] <- "falsch-positiv"
lm_label$diff[lm_label$diff == 1] <- "falsch-negativ"

# Anzahl korrekter Kategorisierungen
lm_valid <- lm_label %>% group_by(diff) %>% count()
lm_valid$n_perc <- lm_valid$n / sum(lm_valid$n) * 100
lm_valid

#------------------------------------------------------------------------------#
# NRC Word-Emotion Association Lexicon Validierung
#------------------------------------------------------------------------------#

# Prozent der analysierbaren Posts berechnen
length(nrc_label$nrc_label) / length(data_labeled$label) * 100

# Anzahl positiven und negativen Posts gemäss NRC
nrc_label %>% group_by(nrc_label) %>% count()

# NRC Kategorisierung mit gelabeltem Datensatz zusammenführen
nrc_label <- merge(data_labeled, nrc_label, by = "ID")

# Differenz zwischen NRC und korrektem Label berechnen
nrc_label$diff <- nrc_label$label - nrc_label$nrc_label

# Differenz als korrekt, falsch-positiv oder falsch-negativ benennen
nrc_label$diff[nrc_label$diff == 0] <- "korrekt"
nrc_label$diff[nrc_label$diff == -1] <- "falsch-positiv"
nrc_label$diff[nrc_label$diff == 1] <- "falsch-negativ"

# Anzahl korrekter Kategorisierungen
nrc_valid <- nrc_label %>% group_by(diff) %>% count()
nrc_valid$n_perc <- nrc_valid$n / sum(nrc_valid$n) * 100
nrc_valid

#------------------------------------------------------------------------------#
# AFINN Dictionary Validierung
#------------------------------------------------------------------------------#

# Prozent der analysierbaren Posts berechnen
length(afinn_label$afinn_label) / length(data_labeled$label) * 100

# Anzahl positiven und negativen Posts gemäss AFINN
afinn_label %>% group_by(afinn_label) %>% count()

# AFINN Kategorisierung mit gelabeltem Datensatz zusammenführen
afinn_label <- merge(data_labeled, afinn_label, by = "ID")

# Differenz zwischen AFINN und korrektem Label berechnen
afinn_label$diff <- afinn_label$label - afinn_label$afinn_label

# Differenz als korrekt, falsch-positiv oder falsch-negativ benennen
afinn_label$diff[afinn_label$diff == 0] <- "korrekt"
afinn_label$diff[afinn_label$diff == -1] <- "falsch-positiv"
afinn_label$diff[afinn_label$diff == 1] <- "falsch-negativ"

# Anzahl korrekter Kategorisierungen
afinn_valid <- afinn_label %>% group_by(diff) %>% count()
afinn_valid$n_perc <- afinn_valid$n / sum(afinn_valid$n) * 100
afinn_valid

#------------------------------------------------------------------------------#
# BING Dictionary Validierung
#------------------------------------------------------------------------------#

# Prozent der analysierbaren Posts berechnen
length(bing_label$bing_label) / length(data_labeled$label) * 100

# Anzahl positiven und negativen Posts gemäss BING
bing_label %>% group_by(bing_label) %>% count()

# BING Kategorisierung mit gelabeltem Datensatz zusammenführen
bing_label <- merge(data_labeled, bing_label, by = "ID")

# Differenz zwischen BING und korrektem Label berechnen
bing_label$diff <- bing_label$label - bing_label$bing_label

# Differenz als korrekt, falsch-positiv oder falsch-negativ benennen
bing_label$diff[bing_label$diff == 0] <- "korrekt"
bing_label$diff[bing_label$diff == -1] <- "falsch-positiv"
bing_label$diff[bing_label$diff == 1] <- "falsch-negativ"

# Anzahl korrekter Kategorisierungen
bing_valid <- bing_label %>% group_by(diff) %>% count()
bing_valid$n_perc <- bing_valid$n / sum(bing_valid$n) * 100
bing_valid

#------------------------------------------------------------------------------#
# GPT (0S) Validierung
#------------------------------------------------------------------------------#

# Prozent der analysierbaren Posts berechnen
length(gpt_0s_label$completion_0s) / length(data_labeled$label) * 100

# Anzahl positiven und negativen Posts gemäss GPT (0S)
gpt_0s_label %>% group_by(completion_0s) %>% count()

# Differenz zwischen GPT (0S) und korrektem Label berechnen
gpt_0s_label$diff <- gpt_0s_label$label - gpt_0s_label$completion_0s

# Differenz als korrekt, falsch-positiv oder falsch-negativ benennen
gpt_0s_label$diff[gpt_0s_label$diff == 0] <- "korrekt"
gpt_0s_label$diff[gpt_0s_label$diff == -1] <- "falsch-positiv"
gpt_0s_label$diff[gpt_0s_label$diff == 1] <- "falsch-negativ"

# Anzahl korrekter Kategorisierungen
gpt_0s_valid <- gpt_0s_label %>% group_by(diff) %>% count()
gpt_0s_valid$n_perc <- gpt_0s_valid$n / sum(gpt_0s_valid$n) * 100
gpt_0s_valid

#------------------------------------------------------------------------------#
# GPT (1S) Validierung
#------------------------------------------------------------------------------#

# Prozent der analysierbaren Posts berechnen
length(gpt_1s_label$completion_1s) / length(data_labeled$label) * 100

# Anzahl positiven und negativen Posts gemäss GPT (1S)
gpt_1s_label %>% group_by(completion_1s) %>% count()

# Differenz zwischen GPT (1S) und korrektem Label berechnen
gpt_1s_label$diff <- gpt_1s_label$label - gpt_1s_label$completion_1s

# Differenz als korrekt, falsch-positiv oder falsch-negativ benennen
gpt_1s_label$diff[gpt_1s_label$diff == 0] <- "korrekt"
gpt_1s_label$diff[gpt_1s_label$diff == -1] <- "falsch-positiv"
gpt_1s_label$diff[gpt_1s_label$diff == 1] <- "falsch-negativ"

# Anzahl korrekter Kategorisierungen
gpt_1s_valid <- gpt_1s_label %>% group_by(diff) %>% count()
gpt_1s_valid$n_perc <- gpt_1s_valid$n / sum(gpt_1s_valid$n) * 100
gpt_1s_valid

#------------------------------------------------------------------------------#
# GPT (FS) Validierung
#------------------------------------------------------------------------------#

# Prozent der analysierbaren Posts berechnen
length(gpt_fs_label$completion_fs) / length(data_labeled$label) * 100

# Anzahl positiven und negativen Posts gemäss GPT (FS)
gpt_fs_label %>% group_by(completion_fs) %>% count()

# Differenz zwischen GPT (FS) und korrektem Label berechnen
gpt_fs_label$diff <- gpt_fs_label$label - gpt_fs_label$completion_fs

# Differenz als korrekt, falsch-positiv oder falsch-negativ benennen
gpt_fs_label$diff[gpt_fs_label$diff == 0] <- "korrekt"
gpt_fs_label$diff[gpt_fs_label$diff == -1] <- "falsch-positiv"
gpt_fs_label$diff[gpt_fs_label$diff == 1] <- "falsch-negativ"

# Anzahl korrekter Kategorisierungen
gpt_fs_valid <- gpt_fs_label %>% group_by(diff) %>% count()
gpt_fs_valid$n_perc <- gpt_fs_valid$n / sum(gpt_fs_valid$n) * 100
gpt_fs_valid
