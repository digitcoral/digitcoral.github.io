Sys.setlocale("LC_ALL","Chinese") #<--set for showing Chinese 
library(googlesheets4)
library(dplyr) #<-- for "select" function
library(RColorBrewer)
library("wordcloud")

gs4_deauth() #<--use only for googlesheets shared as 'anyone with the link'

rw_ac_poems <- read_sheet("https://docs.google.com/spreadsheets/d/1aKXxr4WZh0FOCivLzTFdYkcCHaulDlG9jtPpoDEUvo0/edit?usp=sharing", sheet = "Poems")

acp_cl <- rw_ac_poems
#acp_cl$cl_Time <- sapply(strsplit(na.omit(acp_cl$Time)," "), '[', 1)

# ---------- noun part ----------

cols_n <- max(stringr::str_count(na.omit(acp_cl$Noun_Pro),"/")) + 1

colm_n <- paste("col_n",1:cols_n)

acp_cl <-
  tidyr::separate(
    data = acp_cl,
    col = Noun_Pro,
    sep = "/",
    into = colm_n,
    remove = FALSE
  )

Noun_only <- select(acp_cl,starts_with("col_n"))
Nouns <- data.frame(Noun_Pro=na.omit(unlist(Noun_only))) #<--attach columns of values into one column
Nouns$Noun_Pro <- factor(Nouns$Noun_Pro,levels=as.character(names(sort(table(Nouns$Noun_Pro),decreasing=T)))) 

Noun_freq <- as.data.frame(table(Nouns$Noun_Pro))

wordcloud(words=Noun_freq$Var1,freq=Noun_freq$Freq,min.freq=1,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8,'Dark2'))

# ----------- verb part -----------

cols_v <- max(stringr::str_count(na.omit(acp_cl$Verb),"/")) + 1

colm_v <- paste("col_v",1:cols_v)

acp_cl <-
  tidyr::separate(
    data = acp_cl,
    col = Verb,
    sep = "/",
    into = colm_v,
    remove = FALSE
  )
Verb_only <- select(acp_cl,starts_with("col_v"))
Verbs <- data.frame(Verbs=na.omit(unlist(Verb_only))) #<--attach columns of values into one column
Verbs$Verb <- factor(Verbs$Verb,levels=as.character(names(sort(table(Verbs$Verb),decreasing=T))))

Verb_freq <- as.data.frame(table(Verbs$Verb))

wordcloud(words=Verb_freq$Var1,freq=Verb_freq$Freq,min.freq=1,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8,'Dark2'))
