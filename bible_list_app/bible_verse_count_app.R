### This program get the summary count of chapters & verses from bible

#install.packages("googlesheets4")
#install.packages("ggplot2")
#install.packages("shiny")
#install.packages("shinylive","httpuv")
Sys.setlocale("LC_ALL","Chinese")

library(googlesheets4)
library(dplyr)
library(shiny)
library(DT)
library(rsconnect)
library(shinylive)

gs4_deauth() #<--use only for googlesheets shared as 'anyone with the link'

bok_indx <- read_sheet("https://docs.google.com/spreadsheets/d/1RDLRFS7o2lSLsIRJAjd-cVXQDt_Q1XiDxIQ_ZC6_VDs/edit?usp=sharing", sheet ="Index")

tot_chaps <- prettyNum(sum(bok_indx$Total_Chapters),big.mark=',',scientific=F)
tot_vers <- prettyNum(sum(bok_indx$Total_Verses),big.mark=',',scientific=F)

#echo=FALSE, message=FALSE, warning=FALSE

#### Global.R part ####

books <- list( "01-创世记 | Genesis"             = "01-创世记",
               "02-出埃及记 | Exodus"            = "02-出埃及记",
               "03-利未记 | Leviticus"           = "03-利未记",
               "04-民数记 | Numbers"             = "04-民数记",
               "05-申命记 | Deuteronomy"         = "05-申命记",
               "06-约书亚记 | Joshua"            = "06-约书亚记",
               "07-士师记 | Judges"              = "07-士师记",
               "08-路得记 | Ruth"                = "08-路得记",
               "09-撒母耳记上 | First Samuel"    = "09-撒母耳记上",
               "10-撒母耳记下 | Second Samuel"   = "10-撒母耳记下",
               "11-列王纪上 | First Kings"       = "11-列王纪上",
               "12-列王纪下 | Second Kings"      = "12-列王纪下",
               "13-历代志上 | First Chronicles"  = "13-历代志上",
               "14-历代志下 | Second Chronicles" = "14-历代志下",
               "15-以斯拉记 | Ezra"              = "15-以斯拉记",
               "16-尼希米记 | Nehemiah"          = "16-尼希米记",
               "17-以斯帖记 | Esther"            = "17-以斯帖记",
               "18-约伯记 | Job"                 = "18-约伯记",
               "19-诗篇 | Psalms"                = "19-诗篇",
               "20-箴言 | Proverbs"              = "20-箴言",
               "21-传道书 | Ecclesiastes"        = "21-传道书",
               "22-雅歌 | Song of Solomon"       = "22-雅歌",
               "23-以赛亚书 | Isaiah"            = "23-以赛亚书",
               "24-耶利米书 | Jeremiah"          = "24-耶利米书",
               "25-耶利米哀歌 | Lamentations"    = "25-耶利米哀歌",
               "26-以西结书 | Ezekiel"           = "26-以西结书",
               "27-但以理书 | Daniel"            = "27-但以理书",
               "28-何西阿书 | Hosea"             = "28-何西阿书",
               "29-约珥书 | Joel"                = "29-约珥书",
               "30-阿摩司书 | Amos"              = "30-阿摩司书",
               "31-俄巴底亚书 | Obadiah"         = "31-俄巴底亚书",
               "32-约拿书 | Jonah"               = "32-约拿书",
               "33-弥迦书 | Micah"               = "33-弥迦书",
               "34-那鸿书 | Nahum"               = "34-那鸿书",
               "35-哈巴谷书 | Habakkuk"          = "35-哈巴谷书",
               "36-西番雅书 | Zephaniah"         = "36-西番雅书",
               "37-哈该书 | Haggai"              = "37-哈该书",
               "38-撒迦利亚书 | Zechariah"       = "38-撒迦利亚书",
               "39-玛拉基书 | Malachi"           = "39-玛拉基书",
               "40-马太福音 | Matthew"           = "40-马太福音",
               "41-马可福音 | Mark"              = "41-马可福音",
               "42-路加福音 | Luke"              = "42-路加福音",
               "43-约翰福音 | John"              = "43-约翰福音",
               "44-使徒行传 | Acts"              = "44-使徒行传",
               "45-罗马书 | Romans"                       = "45-罗马书",
               "46-哥林多前书 | First Corinthians"        = "46-哥林多前书",
               "47-哥林多后书 | Second Corinthians"       = "47-哥林多后书",
               "48-加拉太书 | Galatians"                  = "48-加拉太书",
               "49-以弗所书 | Ephesians"                  = "49-以弗所书",
               "50-腓立比书 | Philippians"                = "50-腓立比书",
               "51-歌罗西书 | Colossians"                 = "51-歌罗西书",
               "52-帖撒罗尼迦前书 | First Thessalonians"  = "52-帖撒罗尼迦前书",
               "53-帖撒罗尼迦后书 | Second Thessalonians" = "53-帖撒罗尼迦后书",
               "54-提摩太前书 | First Timothy"            = "54-提摩太前书",
               "55-提摩太后书 | Second Timothy"           = "55-提摩太后书",
               "56-提多书 | Titus"                        = "56-提多书",
               "57-腓利门书 | Philemon"                   = "57-腓利门书",
               "58-希伯来书 | Hebrews"                    = "58-希伯来书",
               "59-雅各书 | James"                        = "59-雅各书",
               "60-彼得前书 | First Peter"                = "60-彼得前书",
               "61-彼得后书 | Second Peter"               = "61-彼得后书",
               "62-约翰一书 | First John"                 = "62-约翰一书",
               "63-约翰二书 | second John"                = "63-约翰二书",
               "64-约翰三书 | Third John"                 = "64-约翰三书",
               "65-犹大书 | Jude"                         = "65-犹大书",
               "66-启示录 | Revelation"                   = "66-启示录"
)

server = function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- bok_indx[,c("Testament","Book","Total_Chapters","Total_Verses")]
    if (input$testa != "All") {
      data <- data[data$Testament == input$testa,]
    }
    if (input$bok != "All") {
      data <- data[data$Book == input$bok,]
    }
    data[,c("Book","Total_Chapters","Total_Verses")]
  }))
}

ui = fluidPage(
  titlePanel("Bible Chapters & Verses Count"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(6,
           selectInput("testa",
                       "Testament:",
                       c("All",
                         unique(as.character(bok_indx$Testament))))
    ),
    column(6,
           selectInput("bok",
                       "Book:",
                       c("All",
                         unique(as.character(bok_indx$Book))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

shinyApp(ui,server)