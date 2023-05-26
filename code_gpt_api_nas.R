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
                               temperature = 0.3,
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
# Sentiment Analyse mit GPT
#------------------------------------------------------------------------------#

# Jeden Post mit GPT klassifizieren
for (i in 1:100) {
  # Completion als Character speichern
  compl <- as.character(ask_gpt(input_data_gpt$prompt[i]))
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
