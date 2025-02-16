### Testing codes to the idea of when click a data row, then show some detail contents.

library(shiny)
# library(dplyr)
library(data.table)

DT <- copy(mtcars)
setDT(DT)

ui <- fluidPage(
  titlePanel("mtcars"),
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