### Testing codes to the idea of when click a data row, then show some detail contents.

library(googlesheets4)
library(dplyr)
library(shiny)
library(data.table)

gs4_deauth() #<--use only for googlesheets shared as 'anyone with the link'

bok_indx <- read_sheet("https://docs.google.com/spreadsheets/d/1RDLRFS7o2lSLsIRJAjd-cVXQDt_Q1XiDxIQ_ZC6_VDs/edit?usp=sharing", sheet ="Index")
bk42     <- read_sheet("https://docs.google.com/spreadsheets/d/1RDLRFS7o2lSLsIRJAjd-cVXQDt_Q1XiDxIQ_ZC6_VDs/edit?usp=sharing", sheet ="42-路-路加福音")
  
tot_chaps <- prettyNum(sum(bok_indx$Total_Chapters),big.mark=',',scientific=F)
tot_vers <- prettyNum(sum(bok_indx$Total_Verses),big.mark=',',scientific=F)
titl <- quote(as.character(bok_indx[42,"Book"]))

DT <- copy(bk42)
setDT(DT)

ui <- fluidPage(
  titlePanel(".(titl)"),
  mainPanel(
    tableOutput("table")
  )
)

server <- function(input, output) {
  
  output$table <- renderTable({
    DT[, inputId := paste0("gear_input_", seq_len(.N))][, gear_links := as.character(actionLink(inputId = inputId, label = inputId, onclick = sprintf("Shiny.setInputValue(id = 'gear_click', value = %s);", gear))), by = inputId][, inputId := NULL]
  }, sanitize.text.function = function(x){x})
  
  observeEvent(input$gear_click, {
    showModal(modalDialog(
      title = "Gear filter",
      tableOutput("filtered_table"),
      size = "xl"
    ))
  })
  
  output$filtered_table <- renderTable({
    req(input$gear_click)
    DT[gear == input$gear_click][, c("gear_links", "vs") := NULL]
  })
  
}

shinyApp(ui = ui, server = server)