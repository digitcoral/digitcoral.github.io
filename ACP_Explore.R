# ACP explorations


install.packages(c('devtools','curl'))
install.packages("googlesheet4")
install.packages("dplyr")

library(devtools)
library(curl)
library(googlesheets4)

gs4_deauth()
rw_ac_poems <- read_sheet("https://docs.google.com/spreadsheets/d/1aKXxr4WZh0FOCivLzTFdYkcCHaulDlG9jtPpoDEUvo0/edit?usp=sharing", sheet = "Poems")

p_lines <- nrow(rw_ac_poems)
p_lines
