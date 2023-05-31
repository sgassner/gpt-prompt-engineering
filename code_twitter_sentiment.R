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
library(cld2) # Sprachermittlung der Posts
library(tidytext) # NLP Funktionen
library(textdata) # NLP Funktionen
library(SnowballC) # Stemming
library(corrplot) # Korrelationsplot
library(stargazer) # für Regressions-Outputs
library(zoo)

################################################################################
### Daten laden
################################################################################

#------------------------------------------------------------------------------#
# Twitter Daten laden
#------------------------------------------------------------------------------#

# Working Directory definieren
setwd("~/Documents/R/MA/twitter")

# Datensätze laden
company_tweet <- read.csv("Company_Tweet.csv")
tweet <- read.csv("Tweet.csv")

# Datensätze zusammenführen basierend auf ID
data_twitter <- merge(company_tweet, tweet, by = "tweet_id")
rm(tweet, company_tweet)

# Working Directory zurücksetzen
setwd("~/Documents/R/MA")

# Entfernung nicht benötigter Spalten
data_twitter <- select(data_twitter, -c("writer"))

# Datensatz als Tibble formatieren
data_twitter <- as_tibble(data_twitter)

#------------------------------------------------------------------------------#
# Aktienpreise laden
#------------------------------------------------------------------------------#

# Working Directory definieren
setwd("~/Documents/R/MA/stocks")

# Datensätze laden
data_stocks <- read.csv("CompanyValues.csv")

# Working Directory zurücksetzen
setwd("~/Documents/R/MA")

#------------------------------------------------------------------------------#
# NASDAQ Index Daten laden
#------------------------------------------------------------------------------#

# NASDAQ Daten laden
data_nasdaq <- read.csv("NASDAQ.csv")

#------------------------------------------------------------------------------#
# Risk-free Rate (10Y Treasury Yield) laden
#------------------------------------------------------------------------------#

# Risk-free Rate Daten laden
data_rf <- read.csv("riskfree_rate.csv") 

################################################################################
### Bereinigung der Daten
################################################################################

#------------------------------------------------------------------------------#
# Twitter Daten bereinigen für GPT Ansatz
#------------------------------------------------------------------------------#

# Struktur und Zusammenfassung der Daten
str(data_twitter)
summary(data_twitter)

# Tweets pro Aktientitel
data_twitter %>% group_by(ticker_symbol) %>% count()

# Datum neu formatieren
data_twitter$post_date <- as.POSIXct(data_twitter$post_date, 
                                     origin = "1970-01-01", tz = "UTC")
data_twitter$post_date <- as.Date(data_twitter$post_date, "%Y-%m-%d")

# Aufsteigend nach Datum sortieren
data_twitter <- data_twitter %>% arrange(post_date)

# Sprachermittlung der Posts
data_twitter$language <- detect_language(data_twitter$body, plain_text = TRUE)

# Englischsprachige Posts herausfiltern
data_twitter <- data_twitter %>% filter(language == "en")
data_twitter <- select(data_twitter, -language)

# Als Datensatz für GPT speichern
data_gpt <- data_twitter

#------------------------------------------------------------------------------#
# Twitter Daten weiter bereinigen für konventionelle Methoden
#------------------------------------------------------------------------------#

# URLs entfernen
data_twitter$body <- gsub("(https?://)?(www\\.)?\\S+\\.\\S+", "", 
                          data_twitter$body)

# Hashtags entfernen
data_twitter$body <- gsub("#", "", data_twitter$body)

# Dollarzeichen vor Ticker entfernen
data_twitter$body <- gsub("\\$([A-Za-z]+)", "\\1", data_twitter$body)

# Markierungen von Personen (@XYZ) entfernen
data_twitter$body <- gsub("@\\S+", "", data_twitter$body)

# Doppelte Leerzeichen entfernen
data_twitter$body <- gsub("  ", " ", data_twitter$body)

#------------------------------------------------------------------------------#
# Aktienpreise bereinigen
#------------------------------------------------------------------------------#

# Struktur und Zusammenfassung der Daten
str(data_stocks)
summary(data_stocks)

# Entfernung nicht benötigter Spalten
data_stocks <- select(data_stocks, -c("open_value", "high_value", "low_value"))

# Datum neu formatieren
data_stocks$day_date <- as.Date(data_stocks$day_date, "%Y-%m-%d")

# Nach Datum sortieren
data_stocks <- data_stocks %>% arrange(day_date)

#------------------------------------------------------------------------------#
# NASDAQ Index Daten bereinigen
#------------------------------------------------------------------------------#

# Nicht benötigte Spalten entfernen
data_nasdaq <- data_nasdaq %>% select(-c("Volume", "Open", "High", "Low"))

# Datum neu formatieren und aufsteigend sortieren
data_nasdaq$Date <- as.Date(data_nasdaq$Date, format = "%m/%d/%Y")
data_nasdaq <- data_nasdaq %>% arrange(Date)

#------------------------------------------------------------------------------#
# Risk-free Rate (10Y Treasury Yield) bereinigen
#------------------------------------------------------------------------------#

# Datum neu formatieren und aufsteigend sortieren
data_rf$DATE <- as.Date(data_rf$DATE, format = "%Y-%m-%d")
data_rf <- data_rf %>% arrange(DATE)

# Fehlende Werte mit Wert darüber füllen
data_rf$DGS10[data_rf$DGS10 == "."] <- NA
data_rf <- zoo::na.locf(data_rf)

################################################################################
### Sentiment-Analyse mit konventionellen Methoden
################################################################################

#------------------------------------------------------------------------------#
# NLP Pre-Processing
#------------------------------------------------------------------------------#

# Tokenization und Normalization (lower-casing)
data_twitter_tokens <- data_twitter %>% 
  unnest_tokens(output = "word", token = "words", input = body) 

# Entfernung von Stop Words
data_twitter_tokens <- data_twitter_tokens %>% anti_join(stop_words)

# Übersicht der erstellten Tokens ohne Stop Words
print(data_twitter_tokens %>% count(word, sort = TRUE), n = 50)

# Stemming
data_twitter_tokens <- data_twitter_tokens %>% mutate(word = wordStem(word))

# Übersicht der erstellten Tokens nach Stemming
print(data_twitter_tokens %>% count(word, sort = TRUE), n = 50)

#------------------------------------------------------------------------------#
# Loughran and McDonald Dictionary (L&M)
#------------------------------------------------------------------------------#

# L&M Dictionary laden
lm_dict <- get_sentiments("loughran")

# Stemming des L&M Dictionary
lm_dict <- lm_dict %>% mutate(word = wordStem(word))

# Tokens mit L&M Dictionary verbinden
lm_nlp <- data_twitter_tokens %>% inner_join(lm_dict, by = "word")

# Sentiment Score für jedes Datum und jede Aktie berechnen
lm_sentiment_score <- lm_nlp %>% 
  group_by(post_date, ticker_symbol, sentiment) %>% count()

# In wide-format konvertieren
lm_sentiment_score_wide <- lm_sentiment_score %>% 
  pivot_wider(names_from = sentiment, values_from = n)

# NA's mit 0 ersetzen
lm_sentiment_score_wide[is.na(lm_sentiment_score_wide)] <- 0

# Summe aller Sentiment-Matches berechnen
lm_sentiment_score_wide$total <- rowSums(lm_sentiment_score_wide[,3:8])

# Anteil der Sentiments für jedes Datum und jede Aktie in Prozent
lm_sentiment_score_wide_perc <- lm_sentiment_score_wide[,1:8]
for (i in unique(lm_dict$sentiment)) {
  lm_sentiment_score_wide_perc[[i]] <- 
    lm_sentiment_score_wide[[i]] / lm_sentiment_score_wide$total * 100
}

# In long-format zurückkonvertieren
lm_sentiment_score_perc <- pivot_longer(lm_sentiment_score_wide_perc, 
                                        cols = unique(lm_dict$sentiment), 
                                        names_to = "sentiment", 
                                        values_to = "n")

# Liste der Sentiment-Scores nach Aktie erstellen
lm_list <- split(lm_sentiment_score_perc, 
                 lm_sentiment_score_perc$ticker_symbol)
lm_list_wide <- split(lm_sentiment_score_wide_perc, 
                      lm_sentiment_score_wide_perc$ticker_symbol)

# Beispielplot für L&M mit Apple erstellen
ggplot(lm_list[["AAPL"]], aes(x = post_date, y = n, group = sentiment)) +
  geom_line(aes(color = sentiment)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "L&M Sentiments der Tweets mit Bezug zu Apple (AAPL)", 
       x = "Datum", 
       y = "Anteil der Stimmungen (in %)") +
  theme(text = element_text(size = 16))

#------------------------------------------------------------------------------#
# NRC Word-Emotion Association Lexicon
#------------------------------------------------------------------------------#

# NRC Dictionary laden
nrc_dict <- get_sentiments("nrc")

# Stemming des NRC Dictionary
nrc_dict <- nrc_dict %>% mutate(word = wordStem(word))

# Tokens mit NRC Dictionary verbinden
nrc_nlp <- data_twitter_tokens %>% inner_join(nrc_dict, by = "word")

# Sentiment Score für jedes Datum und jede Aktie berechnen
nrc_sentiment_score <- nrc_nlp %>% 
  group_by(post_date, ticker_symbol, sentiment) %>% count()

# In wide-format konvertieren
nrc_sentiment_score_wide <- nrc_sentiment_score %>% 
  pivot_wider(names_from = sentiment, values_from = n)

# NA's mit 0 ersetzen
nrc_sentiment_score_wide[is.na(nrc_sentiment_score_wide)] <- 0

# Summe aller Sentiment-Matches berechnen
nrc_sentiment_score_wide$total <- rowSums(nrc_sentiment_score_wide[,3:12])

#  Anteil der Sentiments für jedes Datum und jede Aktie in Prozent
nrc_sentiment_score_wide_perc <- nrc_sentiment_score_wide[,1:12]
for (i in unique(nrc_dict$sentiment)) {
  nrc_sentiment_score_wide_perc[[i]] <- 
    nrc_sentiment_score_wide[[i]] / nrc_sentiment_score_wide$total * 100
}

# In long-format zurückkonvertieren
nrc_sentiment_score_perc <- pivot_longer(nrc_sentiment_score_wide_perc, 
                                         cols = unique(nrc_dict$sentiment), 
                                         names_to = "sentiment", 
                                         values_to = "n")

# Liste der Sentiment-Scores nach Aktie erstellen
nrc_list <- split(nrc_sentiment_score_perc, 
                  nrc_sentiment_score_perc$ticker_symbol)
nrc_list_wide <- split(nrc_sentiment_score_wide_perc, 
                       nrc_sentiment_score_wide_perc$ticker_symbol)

# Beispielplot für NRC mit Apple erstellen
ggplot(nrc_list[["AAPL"]], aes(x = post_date, y = n, group = sentiment)) +
  geom_line(aes(color = sentiment)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "NRC Sentiments der Tweets mit Bezug zu Apple (AAPL)", 
       x = "Datum", 
       y = "Anteil der Stimmungen (in %)") +
  theme(text = element_text(size = 16))

#------------------------------------------------------------------------------#
# AFINN Dictionary
#------------------------------------------------------------------------------#

# AFINN Dictionary laden
afinn_dict <- get_sentiments("afinn")

# Stemming des AFINN Dictionary
afinn_dict <- afinn_dict %>% mutate(word = wordStem(word))

# Tokens mit AFINN Dictionary verbinden
afinn_nlp <- data_twitter_tokens %>% inner_join(afinn_dict, by = "word")

# Sentiment Score für jedes Datum und jede Aktie summieren
afinn_sentiment_score <- afinn_nlp %>% 
  group_by(post_date, ticker_symbol) %>% summarise(sentiment = sum(value))

# Sentiment Score berechnen (% vom maximalen, absoluten Score)
afinn_sentiment_score$sentiment_score <- 
  (100 / max(abs(afinn_sentiment_score$sentiment))) * 
  afinn_sentiment_score$sentiment

# Liste der Sentiment-Scores nach Aktie erstellen
afinn_list <- split(afinn_sentiment_score, afinn_sentiment_score$ticker_symbol)

# Beispielplot für AFINN mit Apple erstellen
ggplot(afinn_list[["AAPL"]], aes(x = post_date, y = sentiment_score)) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ylim(-100, 100) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(title = "AFINN Sentiment der Tweets mit Bezug zu Apple (AAPL)", 
       x = "Datum", 
       y = "AFINN Sentiment-Score (in %)") +
  theme(text = element_text(size = 16))

#------------------------------------------------------------------------------#
# BING
#------------------------------------------------------------------------------#

# Dictionary laden
bing_dict <- get_sentiments("bing")

# Stemming des BING Dictionary
bing_dict <- bing_dict %>% mutate(word = wordStem(word))

# Tokens mit BING Dictionary verbinden
bing_nlp <- data_twitter_tokens %>% inner_join(bing_dict, by = "word")

# Sentiment Score für jedes Datum und jede Aktie berechnen
bing_sentiment_score <- bing_nlp %>% 
  group_by(post_date, ticker_symbol) %>% count(sentiment)

# In wide-format konvertieren
bing_sentiment_score_wide <- bing_sentiment_score %>% 
  pivot_wider(names_from = sentiment, values_from = n)

# Summe aller Sentiment-Matches berechnen
bing_sentiment_score_wide$total <- rowSums(bing_sentiment_score_wide[,3:4])

# Anteil der Sentiments für jedes Datum und jede Aktie in Prozent
bing_sentiment_score_wide_perc <- bing_sentiment_score_wide[,1:4]
for (i in c("positive", "negative")) {
  bing_sentiment_score_wide_perc[[i]] <- 
    bing_sentiment_score_wide[[i]] / bing_sentiment_score_wide$total * 100
}

# In long-format zurückkonvertieren
bing_sentiment_score_perc <- pivot_longer(bing_sentiment_score_wide_perc, 
                                        cols = c("positive", "negative"), 
                                        names_to = "sentiment", 
                                        values_to = "n")

# Liste der Sentiment-Scores nach Aktie erstellen
bing_list <- split(bing_sentiment_score_perc, 
                   bing_sentiment_score_perc$ticker_symbol)
bing_list_wide <- split(bing_sentiment_score_wide_perc, 
                        bing_sentiment_score_wide_perc$ticker_symbol)

# Anteil positiver Posts für Apple herausfiltern
bing_AAPL_pos <- bing_list[["AAPL"]] %>% filter(sentiment == "positive")

# Beispielplot für BING mit Apple erstellen
ggplot(bing_AAPL_pos, aes(x = post_date, y = n)) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  ylim(0, 100) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 1) +
  labs(title = "BING Sentiment der Tweets mit Bezug zu Apple (AAPL)", 
       x = "Datum", 
       y = "Anteil positiver Tokens (in %)") +
  theme(text = element_text(size = 16))

################################################################################
### Sentiment-Analyse mit GPT
################################################################################

#------------------------------------------------------------------------------#
# Input Datensatz mit Prompts für Analyse mit GPT erstellen
#------------------------------------------------------------------------------#

# Task für GPT definieren
prompt_task <- "The following tweet contains an opinion on a stock, a company, a product, or the market. Classify the tweet into positive, negative, or neutral. Only return positive, negative, or neutral."

# Spalte für Prompts erstellen
data_gpt$prompt <- NA

# Prompt für jeden Tweet erstellen
data_gpt$prompt_tweet <- paste0("Tweet about ", 
                                data_gpt$ticker_symbol, 
                                    ": ", 
                                data_gpt$body)
data_gpt$prompt <- paste(prompt_task, 
                         data_gpt$prompt_tweet, 
                             "Sentiment:",
                             sep = "\n")

# Spalte für GPT Klassifikation erstellen
data_gpt$completion <- NA

# Relevante Spalten für GPT auswählen
data_gpt <- data_gpt %>% select(c("tweet_id", "ticker_symbol", "post_date",
                                  "prompt", "completion"))

# Zufällig 1 Mio. Tweets herausfiltern
set.seed(42)
data_gpt <- data_gpt %>% sample_n(1000000)

# Inputdaten in 100 kleinere Data Frames aufsplitten
split_data <- split(data_gpt, 
                    rep(1:100, each = ceiling(nrow(data_gpt) / 100), 
                        length.out = nrow(data_gpt)))

# Input Data Frames speichern
for (i in 1:100) {
  setwd("~/Documents/R/MA/input_gpt")
  filename <- paste0("input_data_gpt_", i, ".csv")
  write.csv(split_data[[i]], file = filename, row.names = FALSE)
  setwd("~/Documents/R/MA")
}

#------------------------------------------------------------------------------#
# Kategorisierung der Tweets mit GPT
#------------------------------------------------------------------------------#

# Die Kategorisierung mit GPT erfolgt durch ein anderes Skript.
# Aufgrund der langen Laufzeit wird dieses auf einem Server ausgeführt.
# Siehe R-Skript: code_gpt_api_server.R

#------------------------------------------------------------------------------#
# GPT Output Datensatz mit Completions laden
#------------------------------------------------------------------------------#

# Leeren Dataframe erstellen, um Outputdaten zu laden
gpt_sentiments <- data.frame()

# Loop um alle Outputdaten zu öffnen und zu verbinden
for (i in 1:100) {
  
  # Ordner mit Outputdaten definieren
  setwd("~/Documents/R/MA/output_gpt")
  
  # Dateinamen definieren
  filename <- paste0("output_data_gpt_", i, ".csv")
  
  # CSV der Outputdaten laden
  df <- read.csv(filename)
  
  # Zu einem Dataframe kombinieren
  gpt_sentiments <- rbind(gpt_sentiments, df)
  
  # WD zurücksetzen
  setwd("~/Documents/R/MA")
}

# Indexspalte entfernen
gpt_sentiments <- gpt_sentiments %>% select(-X)

# Format der Completions anpassen
gpt_sentiments$completion <- tolower(gpt_sentiments$completion)

# Aufsteigend nach Datum sortieren
gpt_sentiments$post_date <- as.Date(gpt_sentiments$post_date, "%Y-%m-%d")
gpt_sentiments <- gpt_sentiments %>% arrange(post_date)

# Art der Completions anschauen
gpt_sentiments_share <- gpt_sentiments %>% 
  group_by(completion) %>% count() %>% arrange(desc(n))
gpt_sentiments_share$perc <- 
  round((100 / nrow(gpt_sentiments) * gpt_sentiments_share$n), 2)
gpt_sentiments_share

# Positive und negative Klassifikationen herausfiltern
gpt_label <- gpt_sentiments %>% select(c("tweet_id", "ticker_symbol", 
                                         "post_date","completion"))
gpt_label <- gpt_label %>% 
  filter(completion %in% c("positive", "negative", "neutral"))

# Sentiment Score für jedes Datum und jede Aktie berechnen
gpt_sentiment_score <- gpt_label %>% 
  group_by(post_date, ticker_symbol, completion) %>% count()

# In wide-format konvertieren
gpt_sentiment_score_wide <- gpt_sentiment_score %>% 
  pivot_wider(names_from = completion, values_from = n)

# NA's mit 0 ersetzen
gpt_sentiment_score_wide[is.na(gpt_sentiment_score_wide)] <- 0

# Summe aller Sentiment-Matches berechnen
gpt_sentiment_score_wide$total <- rowSums(gpt_sentiment_score_wide[,3:5])

# Anteil der Sentiments für jedes Datum und jede Aktie in Prozent
gpt_sentiment_score_wide_perc <- gpt_sentiment_score_wide[,1:5]
for (i in unique(gpt_label$completion)) {
  gpt_sentiment_score_wide_perc[[i]] <- 
    gpt_sentiment_score_wide[[i]] / gpt_sentiment_score_wide$total * 100
}

# In long-format zurückkonvertieren
gpt_sentiment_score_perc <- pivot_longer(gpt_sentiment_score_wide_perc, 
                                         cols = unique(gpt_label$completion), 
                                         names_to = "completion", 
                                         values_to = "n")

# Liste der Sentiment-Scores nach Aktie erstellen
gpt_list <- split(gpt_sentiment_score_perc, 
                  gpt_sentiment_score_perc$ticker_symbol)
gpt_list_wide <- split(gpt_sentiment_score_wide_perc, 
                       gpt_sentiment_score_wide_perc$ticker_symbol)

# Beispielplot für GPT mit Apple erstellen
ggplot(gpt_list[["AAPL"]], aes(x = post_date, y = n, group = completion)) +
  geom_line(aes(color = completion)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "GPT (0S) Sentiments der Tweets mit Bezug zu Apple (AAPL)", 
       x = "Datum", 
       y = "Anteil der Stimmungen (in %)") +
  theme(text = element_text(size = 16))

################################################################################
### Zusammenhang von Stimmung und Aktienperformance
################################################################################

#------------------------------------------------------------------------------#
# CAPM Parameter berechnen
#------------------------------------------------------------------------------#

# Tagesrendite des Marktes berechnen
data_nasdaq <- data_nasdaq %>% 
  mutate(return_market = (Close.Last - lag(Close.Last)) / lag(Close.Last) * 100)

# Tagesrendite Risk-Free berechnen
data_rf$DGS10 <- as.numeric(data_rf$DGS10)
data_rf <- data_rf %>% mutate(return_rf = (DGS10 / 360))
data_rf <- data_rf %>% select(-DGS10)

# Index Return & Risk-Free Rate zusammenführen
data_nasdaq <- merge(data_nasdaq, data_rf, by.x = "Date", by.y = "DATE")

# Market Excess Return berechnen
data_nasdaq <- data_nasdaq %>% mutate(market_excess_return = 
                                        return_market - return_rf)
# CAPM Parameter speichern
data_capm <- data_nasdaq %>% select(-c("Close.Last", "return_market"))

# CAPM Parameter zu Aktiendaten hinzufügen
data_stocks <- merge(data_stocks, data_capm, by.x = "day_date", by.y = "Date")

#------------------------------------------------------------------------------#
# Renditen der Aktientitel berechnen
#------------------------------------------------------------------------------#

# Funktion für Renditeberechnung erstellen
return_fun <- function(data) {
  data %>% select(-c("ticker_symbol", "volume")) %>%
    # Return der Aktientitel berechenen
    mutate(return = (close_value - lag(close_value)) / 
             lag(close_value) * 100) %>%
    # Excess Return der Aktientitel berechnen
    mutate(excess_return = return - return_rf)
}

# Liste mit Rendite nach Aktie erstellen
return_list <- data_stocks %>% split(.$ticker_symbol) %>% lapply(return_fun)

# Aktientitel definieren
symbols <- c("AAPL", "AMZN", "GOOG", "GOOGL", "MSFT", "TSLA")

#------------------------------------------------------------------------------#
# L&M + CAPM Regressionen
#------------------------------------------------------------------------------#

# Liste für Aktienrenditen und L&M Sentiment Scores erstellen
return_lm_list <- list()

# Aktienrenditen und L&M Sentiment Scores in Liste zusammenführen
for (symbol in symbols) {
  
  # Renditen und L&M Scores herziehen
  return_df <- lag(return_list[[symbol]]) # Time-Lag = 1 Tag
  lm_df <- lm_list_wide[[symbol]]
  
  # Renditen und L&M Scores herziehen in Liste speichern
  return_lm_list[[symbol]] <- merge(lm_df, return_df, by.x = "post_date", 
                                    by.y = "day_date")
  
  # Data Frames aus Memory entfernen
  rm(lm_df, return_df)
}

# Korrelation der L&M Sentiments überprüfen
lm_cor <- round(cor(lm_sentiment_score_wide_perc[,3:8]), 4)
lm_cor
corrplot(lm_cor)

# OLS Formel definieren
formula <- excess_return ~ market_excess_return + positive + uncertainty + 
  litigious + constraining + superfluous
# formula <- return ~ market_excess_return + negative

####### Alle Sentiments
# positive + negative + uncertainty + litigious + constraining + superfluous

# Liste erstellen um Resultate zu speichern
ols_lm_list <- list()

# L&M OLS Modelle für alle Aktientitel mit Loop erstellen
for (symbol in symbols) {
  ols_lm_list[[symbol]] <- lm(formula, data = return_lm_list[[symbol]])
}

# OLS Resultate für alle Titel anzeigen
stargazer(ols_lm_list, type="text", digits = 4)
#stargazer(ols_lm_list, type="html", digits = 4)

#------------------------------------------------------------------------------#
# NRC + CAPM Regressionen
#------------------------------------------------------------------------------#

# Liste für Aktienrenditen und NRC Sentiment Scores erstellen
return_nrc_list <- list()

# Aktienrenditen und NRC Sentiment Scores in Liste zusammenführen
for (symbol in symbols) {
  
  # Renditen und NRC Scores herziehen
  return_df <- lag(return_list[[symbol]]) # Time-Lag = 1 Tag
  nrc_df <- nrc_list_wide[[symbol]]
  
  # Renditen und NRC Scores herziehen in Liste speichern
  return_nrc_list[[symbol]] <- merge(nrc_df, return_df, by.x = "post_date", 
                                     by.y = "day_date")
  
  # Data Frames aus Memory entfernen
  rm(nrc_df, return_df)
}

# Korrelation der NRC Sentiments überprüfen
nrc_cor <- round(cor(nrc_sentiment_score_wide_perc[,3:12]), 4)
nrc_cor
corrplot(nrc_cor)

# OLS Formel definieren
formula <- excess_return ~ market_excess_return + positive + negative + 
  anger + anticipation + disgust + fear + joy + sadness + surprise

####### Alle Sentiments
# anger + anticipation + disgust + fear + joy + negative + positive + sadness +
# surprise + trust

# Liste erstellen um Resultate zu speichern
ols_nrc_list <- list()

# NRC OLS Modelle für alle Aktientitel mit Loop erstellen
for (symbol in symbols) {
  ols_nrc_list[[symbol]] <- lm(formula, data = return_nrc_list[[symbol]])
}

# OLS Resultate Tablle anzeigen
stargazer(ols_nrc_list, type="text", digits = 4)
# stargazer(ols_nrc_list, type="html", digits = 4)

#------------------------------------------------------------------------------#
# AFINN + CAPM Regressionen
#------------------------------------------------------------------------------#

# Liste für Aktienrenditen und AFINN Sentiment Scores erstellen
return_afinn_list <- list()

# Aktienrenditen und AFINN Sentiment Scores in Liste zusammenführen
for (symbol in symbols) {
  
  # Renditen und AFINN Scores herziehen
  return_df <- lag(return_list[[symbol]]) # Time-Lag = 1 Tag
  afinn_df <- afinn_list[[symbol]]
  
  # Renditen und AFINN Scores herziehen in Liste speichern
  return_afinn_list[[symbol]] <- merge(afinn_df, return_df, by.x = "post_date", 
                                       by.y = "day_date")
  
  # Data Frames aus Memory entfernen
  rm(afinn_df, return_df)
}

# OLS Formel definieren
formula <- excess_return ~ market_excess_return + sentiment_score

# Liste erstellen um Resultate zu speichern
ols_afinn_list <- list()

# AFINN OLS Modelle für alle Aktientitel mit Loop erstellen
for (symbol in symbols) {
  ols_afinn_list[[symbol]] <- lm(formula, data = return_afinn_list[[symbol]])
}

# OLS Resultate für alle Titel anzeigen
stargazer(ols_afinn_list, type="text", digits = 4)
# stargazer(ols_afinn_list, type="html", digits = 4)

#------------------------------------------------------------------------------#
# BING + CAPM Regressionen
#------------------------------------------------------------------------------#

# Liste für Aktienrenditen und BING Sentiment Scores erstellen
return_bing_list <- list()

# Aktienrenditen und BING Sentiment Scores in Liste zusammenführen
for (symbol in symbols) {
  
  # Renditen und BING Scores herziehen
  return_df <- lag(return_list[[symbol]]) # Time-Lag = 1 Tag
  bing_df <- bing_list_wide[[symbol]]
  
  # Renditen und BING Scores herziehen in Liste speichern
  return_bing_list[[symbol]] <- merge(bing_df, return_df, by.x = "post_date", 
                                      by.y = "day_date")
  
  # Data Frames aus Memory entfernen
  rm(bing_df, return_df)
}

# OLS Formel definieren
formula <- excess_return ~ market_excess_return + positive
# formula <- return ~ market_excess_return + negative

####### Alle Sentiments
# positive + negative

# Liste erstellen um Resultate zu speichern
ols_bing_list <- list()

# NRC OLS Modelle für alle Aktientitel mit Loop erstellen
for (symbol in symbols) {
  ols_bing_list[[symbol]] <- lm(formula, data = return_bing_list[[symbol]])
}

# OLS Resultate für alle Titel anzeigen
stargazer(ols_bing_list, type="text", digits = 4)
# stargazer(ols_bing_list, type="html", digits = 4)

#------------------------------------------------------------------------------#
# GPT + CAPM Regressionen
#------------------------------------------------------------------------------#

# Liste für Aktienrenditen und GPT Sentiment Scores erstellen
return_gpt_list <- list()

# Aktienrenditen und GPT Sentiment Scores in Liste zusammenführen
for (symbol in symbols) {
  
  # Renditen und GPT Scores herziehen
  return_df <- lag(return_list[[symbol]], 1) # Time-Lag = 1 Tag
  gpt_df <- gpt_list_wide[[symbol]]
  
  # Renditen und GPT Scores herziehen in Liste speichern
  return_gpt_list[[symbol]] <- merge(gpt_df, return_df, by.x = "post_date", 
                                     by.y = "day_date")
  
  # Data Frames aus Memory entfernen
  rm(gpt_df, return_df)
}

# OLS Formel definieren
formula <- excess_return ~ market_excess_return + positive + negative

# Liste erstellen um Resultate zu speichern
ols_gpt_list <- list()

# GPT OLS Modelle für alle Aktientitel mit Loop erstellen
for (symbol in symbols) {
  ols_gpt_list[[symbol]] <- lm(formula, data = return_gpt_list[[symbol]])
}

# OLS Resultate für alle Titel anzeigen
stargazer(ols_gpt_list, type="text", digits = 4)
# stargazer(ols_gpt_list, type="html", digits = 4)

#------------------------------------------------------------------------------#
# Ende des Skripts
#------------------------------------------------------------------------------#
