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
# OpenAI API und Prompt-Funktion einrichten
#------------------------------------------------------------------------------#

# OpenAI API Key definieren
api_key <- "HIER KEY EINFÜGEN"

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

# Inputdaten öffnen, kategorisieren & wieder speichern
for (i in 1:100) {
  
  # Dateiname der Inputdaten
  filename_input <- paste0("input_data_gpt_", i, ".csv")
  
  # Inputdaten laden
  df <- read.csv(filename_input)
  
  # Jeden Post mit GPT kategorisieren
  for (j in 1:length(df$prompt)) {
    
    # Completion als Character speichern (Zeitlimit = 5 Sekunden)
    compl <- as.character(interruptor(FUN = ask_gpt, 
                                      args = df$prompt[j],
                                      time.limit = 5))
    
    # Fortschrittsanzeige (Inputdaten i, Zeile j, Completion j & Zeit)
    print(paste(i, j, compl, Sys.time()))
    
    # Completion im Data Frame speichern
    df$completion[j] <- compl
  }
  
  # Dateiname der Outputdaten
  filename_output <- paste0("output_data_gpt_", i, ".csv")
  
  # Output als CSV speichern
  write.csv(df, filename_output)
}

#------------------------------------------------------------------------------#
# Zusammenhang von Stimmung und Aktienperformance
#------------------------------------------------------------------------------#

# Die weitere Bearbeitung und Analyse erfolgt wieder lokal.
# Siehe R-Skript: code_twitter_sentiment.R

#------------------------------------------------------------------------------#
# Ende des Skripts
#------------------------------------------------------------------------------#
