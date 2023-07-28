# Projektname: GPT Prompt Engineering zur Kategorisierung der Stimmung von Anlegerinnen und Anlegern

Dieses Repository enthält alle Skripts für die Arbeit: "GPT Prompt Engineering zur Kategorisierung der Stimmung von Anlegerinnen und Anlegern". 

## Überblick

Diese Arbeit untersucht, inwiefern sich die Stimmung von Anlegerinnen und Anlegern auf Twitter durch GPT Prompt Engineering kategorisieren lässt. Die Kategorisierungsperformance wird dabei mit konventionellen, wörterbuchbasierten Kategorisierungsmethoden (Loughran & McDonald, NRC, AFINN und BING) verglichen. Weiter werden CAPM-Erweiterungen mit den kategoriserten Stimmungen erstellt, um den Zusammenhang von Stimmung und Aktienperformance zu untersuchen. 

## Inhalt des Repositorys

- `code_twitter_sentiment_labeled.R`: Dieses Skript wurde verwendet, um die Performance der unterschiedlichen Kategorisierungsmethoden zu ermitteln. Die durch Crowdsourcing gelabelten Tweets zur Apple-Aktie (Ground Truth) werden hierzu dem NLP-Pre-Processing unterzogen und anhand der vier wörterbuchbasierten Kategorisierungsmethoden kategorisiert. Weiter werden GPT-Prompts erstellt (0S, 1S und FS) und die Tweets werden über das OpenAI-API kategorisiert. Abschliessend wird die Kategorisierungsperformance aller Methoden ausgewertet.
- `code_twitter_sentiment.R`: In diesem Skript werden die ungelabelten Tweets durch die wörterbuchba-sierten Methoden kategorisiert. Die Kategorisierungen durch GPT werden im untenstehenden, externen Skript vorgenommen. Hierfür wurden Input-Files mit den GPT-Prompts für 1 Mio. Tweets erstellt. Die von GPT generierten Output-Files mit den Kategorisierungen der Tweets werden schliesslich wieder in diesem Skript geladen. Anschliessend werden die CAPM-Erweiterungen mit den Stimmungen aller Kategorisierungsmethoden modelliert. Schliesslich werden die Modelle im Rahmen einer Fehleranalyse auf die Grundannahmen des linearen Regressionsmodells geprüft.
- `code_gpt_api_server.R`: Da sich die Kategorisierung mit GPT als eher zeitintensiv erwiesen hat, wurde ein separates Skript erstellt, welches auf einem Server ausgeführt werden kann. Das Skript beinhaltet eine eigens erstellte Prompt-Funktion, welche auf das OpenAI-API zugreift und eine Fortschrittsanzeige beinhaltet. Aus den im obenstehenden Skript erstellten Inputfiles mit den 1 Mio. Tweets und Prompts werden so Output-Files generiert, welche die Kategorisierungen beinhalten und dann wieder im Skript `code_twitter_sentiment.R` verwendet werden.

## Verwendete Daten
- `tweets_aapl_labeled.csv`: Die gelabelten Tweets zur Apple-Aktie (Ground Truth) sind unter folgendem Link verfügbar: https://data.world/crowdflower/apple-twitter-sentiment
- `Company_Tweet.csv` & `Tweet.csv`: Die ungelabelten Tweets zu den sechs betrachteten NASDAQ-Titeln sind hier verfügbar: https://www.kaggle.com/datasets/omermetinn/tweets-about-the-top-companies-from-2015-to-2020?resource=download
- `CompanyValues.csv`: Die Aktienpreise zu den betrachteten Titeln stammen von hier: https://www.kaggle.com/datasets/omermetinn/values-of-top-nasdaq-copanies-from-2010-to-2020
- `NASDAQ.csv`: Die Werte des NASDAQ, welche für die Modellierung des CAPM verwendet wurden finden sich unter: https://www.nasdaq.com/market-activity/index/comp/historical
- `riskfree_rate.csv`: Weiter wurden für die CAPM-Erweiterungen die 10Y Treasury Yields als Approximation der risikofreien Rendite verwendet, welch sich unter folgendem Link finden lassen: https://fred.stlouisfed.org/series/DGS10

## Software und Bibliotheken

Dieses Projekt verwendet die R-Version 4.3.0 in RStudio 2023.03.0 Build 386. Hier sind die Bibliotheken, die in diesem Projekt verwendet wurden und deren Verwendungszweck:

- `tidyverse`: Data Handling
- `cld2`: Sprachermittlung der Posts
- `tidytext`: NLP Funktionen
- `textdata`: NLP Funktionen
- `SnowballC`: Stemming
- `corrplot`: Korrelationsplots
- `stargazer`: Regressions-Outputs
- `zoo`: Umgang mit Missing Values
- `lmtest`: Statistische Fehleranalyse
- `sandwich`: Heteroskedastie-Robuste Standardfehler
- `gridExtra`: Visualisierung von mehreren Plots
- `httr`: Zugriff auf das OpenAI API
- `R.utils`: Zeitlimits der Prompts
