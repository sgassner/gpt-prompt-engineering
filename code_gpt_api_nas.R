#------------------------------------------------------------------------------#
# Master-Arbeit
#
# Titel:        GPT Prompt Engineering zur Kategorisierung der Stimmung 
#               von Anlegerinnen und Anlegern
# Autor:        Sandro Gassner
# Datum:        21.08.2023
#------------------------------------------------------------------------------#

# Definition des Working Directories
setwd("~/local/lib/R/MA")

# Packages laden
library(httr)
library(stringr)
library(R.utils) # für Zeitlimit der Prompts

#------------------------------------------------------------------------------#
# Datensatz mit Prompts laden
#------------------------------------------------------------------------------#
input_data_gpt <- read.csv("input_data_gpt.csv")

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
# Funktion erstellen um langsame Abfragen zu überspringen
#------------------------------------------------------------------------------#

# Falls Laufzeit > time.limit --> "Zeitlimit erreicht" retournieren
interruptor <- function(FUN, args, time.limit) {
  results <- tryCatch({
    withTimeout({ FUN(args) }, timeout = time.limit)
  }, TimeoutException = function(e) {
    "Zeitlimit erreicht"
  }, error = function(e) {
    "Zeitlimit erreicht"
  })

  return(results)
}

#------------------------------------------------------------------------------#
# Sentiment Analyse mit GPT
#------------------------------------------------------------------------------#

# Jeden Post mit GPT klassifizieren
for (i in 1:length(input_data_gpt$prompt)) {
  # Completion als Character speichern (Zeitlimit = 5 Sekunden)
  compl <- as.character(interruptor(FUN = ask_gpt, 
                                    args = input_data_gpt$prompt[i],
                                    time.limit = 5))
  # Zeilennummer und Completion anzeigen (Fortschrittsanzeige)
  print(paste(i, compl))
  # Completion im Data Frame speichern
  input_data_gpt$completion[i] <- compl
}

#------------------------------------------------------------------------------#
# Kategorisierten Datensatz speicher
#------------------------------------------------------------------------------#

# CSV erstellen
write.csv(input_data_gpt, "output_data_gpt.csv")

# Die weitere Bearbeitung und Analyse erfolgt lokal
# Siehe R-Skript: code_twitter_sentiment.R

#------------------------------------------------------------------------------#
# Ende des Skripts
#------------------------------------------------------------------------------#
