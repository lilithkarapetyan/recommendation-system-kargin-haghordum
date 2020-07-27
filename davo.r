library(shinydashboard)
library(ggplot2)
library(shiny)
library(dplyr)
library(RCurl)
library(RecordLinkage)
library(wesanderson)
library(shinythemes)
library(rio)
library(readxl)
library(utf8)
library(purrr)
library(stringi)
library(stringr)


download.file(url = "https://arcane-castle-95125.herokuapp.com/kargin.xlsx", destfile = 'kargin.xlsx', mode="wb")

data <- data.frame(read_excel("kargin.xlsx"))
moodViews <- setNames(aggregate(data[,2:2], list(data$Mood), mean), c("Mood", "AverageViews")) %>%
  mutate(prop = round(AverageViews, digits = 1)) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(moodViews, aes(x="", y=AverageViews, fill=Mood)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = ypos, label = prop), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")
