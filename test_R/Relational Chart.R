###########################################
# 做关系型图（含多个父node生成一个子node）
###########################################

#install.packages("igraph")
#install.packages("googlesheets4")

library(shiny)
library(visNetwork)
library(dplyr)
library(tidyr)
library(igraph)
library(googlesheets4)

#packageVersion("visNetwork")

############################
# 原始Concept数据
############################

gs4_deauth()

concepts <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1mxEeLBperiYnlYuRiUw0qom_TwIx7hUSAWIjwmFKJxU/edit?usp=sharing",
  sheet = "Maths"
)
concepts <- concepts %>%
  mutate(across(everything(), as.character))

############################
# 转换成Edge Table
############################

edges <- concepts %>%
  filter(!is.na(UpperLevel),
         UpperLevel!="") %>%
  separate_rows(UpperLevel, sep="\\s*&\\s*") %>%
  transmute(
    from = UpperLevel,
    to = Concept
  )

############################
# Node Table
############################

nodes <- data.frame(
  
  id = concepts$Concept,
  
  label = concepts$Concept,
  
  title = concepts$Definition,
  
  stringsAsFactors = FALSE
  
)

############################
# UI
############################

ui <- fluidPage(
  
  titlePanel("Math Concept Graph"),
  
  fluidRow(
    
    column(
      width = 8,
      
      visNetworkOutput(
        "conceptGraph",
        height = "750px"
      )
    ),
    
    column(
      width = 4,
      
      h3("Selected Concept"),
      
      hr(),
      
      h4(textOutput("concept_name")),
      
      h5("Definition"),
      
      htmlOutput("definition")
    )
    
  )
)

############################
# Server
############################

server <- function(input, output){
  
  ###########################################
  # 默认显示
  ###########################################
  
  output$concept_name <- renderText({
    
    "Click a concept"
    
  })
  
  output$definition <- renderUI({
    
    HTML("Select a node on the graph to view its definition.")
    
  })
  
  
  ###########################################
  # 绘图
  ###########################################
  
  output$conceptGraph <- renderVisNetwork({
    
    visNetwork(
      nodes,
      edges,
      width = "100%",
      height = "700px"
    ) %>%
      
      visNodes(
        shape = "box",
        widthConstraint = list(maximum = 150),
        font = list(size = 18),
        color = list(
          background = "#DCEEFF",
          border = "#4477AA",
          highlight = "#FFD966"
        )
      ) %>%
      
      visEdges(
        arrows = "to",
        smooth = TRUE
      ) %>%
      
      visIgraphLayout(
        layout = "layout_with_sugiyama"
      ) %>%
      
      visInteraction(
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE
      ) %>%
      
      visPhysics(
        enabled = FALSE
      ) %>%
      
      visEvents(
        
        select = "
        function(params){

            if(params.nodes.length > 0){

                Shiny.setInputValue(
                    'selected_node',
                    params.nodes[0],
                    {priority:'event'}
                );

            }

        }"
        
      )
    
  })
  
  
  ###########################################
  # 点击节点后更新Definition
  ###########################################
  
  observeEvent(input$selected_node, {
    
    selected <- concepts %>%
      filter(Concept == input$selected_node) %>%
      slice(1)
    
    output$concept_name <- renderText({
      
      selected$Concept
      
    })
    
    output$definition <- renderUI({
      
      def <- selected$Definition[1]
      
      if (is.na(def) || trimws(def) == "") {
        
        HTML("<i>No definition available.</i>")
        
      } else {
        
        HTML(def)
        
      }
      
    })
    
  })
  
}

shinyApp(ui, server)
