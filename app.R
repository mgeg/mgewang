library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(reshape2)

#get selectInput
civ = fromJSON("https://age-of-empires-2-api.herokuapp.com/api/v1/civilizations")
building = fromJSON("https://age-of-empires-2-api.herokuapp.com/api/v1/structures")

ui <- fluidPage(titlePanel("Age of Empires 2"),
                
                sidebarPanel(
                  
                  #input for civilizations
                  selectInput(inputId = "civi", label = strong("Civilization"),
                              choices = civ[["civilizations"]][["name"]], 
                              selected = "-"),
                  
                  #input for structures
                  selectInput(inputId = "struct1", label = strong("Structure 1"),
                              choices = building[["structures"]][["name"]], 
                              selected = "-"),
                  numericInput(inputId = "count1", label = strong("Count"),
                               value = 1, min = 0, max = 20, step = 1),
                  
                  selectInput(inputId = "struct2", label = strong("Structure 2"),
                              choices = building[["structures"]][["name"]], 
                              selected = "-"),
                  numericInput(inputId = "count2", label = strong("Count"),
                               value = 1, min = 0, max = 20, step = 1),
                  
                  selectInput(inputId = "struct3", label = strong("Structure 3"),
                              choices = building[["structures"]][["name"]], 
                              selected = "-"),
                  numericInput(inputId = "count3", label = strong("Count"),
                              value = 1, min = 0, max = 20, step = 1),
                  
                ),
                mainPanel(
                  tabsetPanel(
                    
                    tabPanel("Civilization", p("Key Word"),
                             verbatimTextOutput(outputId = "key"),
                             p("Civilization Bonus"),
                             verbatimTextOutput(outputId = "bonus"),
                             p("Unique Unit"),
                             verbatimTextOutput(outputId = "uunit"),
                             p("Unique Technologies"),
                             verbatimTextOutput(outputId = "utech")),
                    
                    tabPanel("Cost", plotOutput(outputId ="plot"),
                             verbatimTextOutput(outputId ="cost"))
                  )
                ))


server <- function(input, output) {
  #only get url from reactive, otherwise JSON will fail
  inp1 = reactive({
    str_glue("https://age-of-empires-2-api.herokuapp.com/api/v1/civilization/",
             input$civi) %>% URLencode()
  })
  
  inp2 = reactive({
    str_glue("https://age-of-empires-2-api.herokuapp.com/api/v1/structure/",
                  input$struct1) %>% URLencode()
  })
  
  inp3 = reactive({
    str_glue("https://age-of-empires-2-api.herokuapp.com/api/v1/structure/",
             input$struct2) %>% URLencode()
  })
  
  inp4 = reactive({
    str_glue("https://age-of-empires-2-api.herokuapp.com/api/v1/structure/",
             input$struct3) %>% URLencode()
  })
  
  output$bonus <- renderPrint({
    fromJSON(inp1())$civilization_bonus
  })
  
  output$uunit <- renderPrint({
    url_unit = fromJSON(inp1())$unique_unit %>% URLencode()
    out = fromJSON(url_unit)
    list("Name" = out$name,"Description" = out$description,
         "Cost" = out$cost,"Attack Bonus" = out$attack_bonus)
  })
  
  
  output$utech <- renderPrint({
    url_utech = fromJSON(inp1())$unique_tech %>% URLencode()
    out = fromJSON(url_utech)
    list("Name" = out$name,"Description" = out$description,
         "Cost" = out$cost)
  })
  
  
  output$key <- renderPrint({
    
    url_unit = fromJSON(inp1())$unique_unit %>% URLencode()
    url_utech = fromJSON(inp1())$unique_tech %>% URLencode()
    
    #use intermidrate variables to reduce running time(less fromJSON())
    uni = fromJSON(url_unit)
    utech = fromJSON(url_utech)
    
    t1 = fromJSON(inp1())$civilization_bonus
    
    #use text mining to extract key words
    txt = c(t1,uni$description,uni$attack_bonus,utech$description)
    df <- tibble(line = 1:length(txt), text = txt)
    out = df %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words, by = "word") %>% count(word, sort = TRUE)
    
    out$word[1:3]

  })

  output$cost <- renderPrint({
    st1 = fromJSON(inp2())
    s1 = st1$cost %>% as.data.frame() 
    c1 = input$count1
    s1 = s1*c1
    s1$Structure = st1$name
    
    st2 = fromJSON(inp3())
    s2 = st2$cost %>% as.data.frame() 
    c2 = input$count2
    s2 = s2*c2
    s2$Structure = st2$name
    
    st3 = fromJSON(inp4())
    s3 = st3$cost %>% as.data.frame() 
    c3 = input$count3
    s3 = s3*c3
    s3$Structure = st3$name
    
    s1 = dplyr::bind_rows(s1, s2)
    s1 = dplyr::bind_rows(s1, s3)
    
    s1 = s1[!duplicated(s1),]
    
    s1[,c("Structure",setdiff(names(s1),"Structure"))]
  })
  
  output$plot <- renderPlot({
    #fromJSON only works in render{}
    st1 = fromJSON(inp2())
    s1 = st1$cost %>% as.data.frame() 
    c1 = input$count1
    s1 = s1*c1
    s1$Structure = st1$name
    
    st2 = fromJSON(inp3())
    s2 = st2$cost %>% as.data.frame() 
    c2 = input$count2
    s2 = s2*c2
    s2$Structure = st2$name
    
    st3 = fromJSON(inp4())
    s3 = st3$cost %>% as.data.frame() 
    c3 = input$count3
    s3 = s3*c3
    s3$Structure = st3$name
    
    s1 = dplyr::bind_rows(s1, s2)
    s1 = dplyr::bind_rows(s1, s3)
    
    s1 = s1[!duplicated(s1),]
    
    s1 = s1[,c("Structure",setdiff(names(s1),"Structure"))]
    
    dfm <- melt(s1[,colnames(s1)],id.vars = 1)
    
    ggplot(dfm,aes(x = Structure,y = value)) + 
      geom_bar(aes(fill = variable),stat = "identity",position = "dodge") + 
      scale_y_log10()+ylab('Cost')
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)

#API from:
#https://age-of-empires-2-api.herokuapp.com/docs/#/
#Code Reference:
#https://stackoverflow.com/questions/3369959/moving-columns-within-a-data-frame-without-retyping




