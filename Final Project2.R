#Final Project
#Group 7
#11/29/2018
#Brandon Harden, Dominique Dongoh, Kudzai Mundava 

library(patentsview)
library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)
setwd("C:/CIS4730")

#query data
my_query <- qry_funs$and(
  qry_funs$gte(patent_date = "2018-01-01"),
  qry_funs$lte(patent_date = "2018-03-31")
)
#put cuery results in variable 
results = search_pv(
  query = my_query, 
  fields = c("inventor_id", "patent_number","inventor_city",
      "assignee_lastknown_state", "patent_id","patent_date",
       "inventor_last_name", "assignee_id","inventor_country",
      "assignee_organization"),
  all_pages = T
)


#unnested results 
unnested_results =results$data$patent %>%
unnest(inventors, .drop = FALSE) %>%
unnest(assignees)

#created filepath to pull saved data from local file
write_csv(unnested_results, "patentsviews2.csv")
file.path("patentsview2.csv")
unnested_results = read_csv("patentsviews2.csv")

#create variable to hold unnested results for sums
list <- (unnested_results)

#sum variables
x <- unique(list$patent_id)
y <- unique(list$assignee_id)
z <- unique(list$inventor_id)


#userinterface part

my_ui <- fluidPage(
  
  titlePanel("Assignee organizations with more than 10 patents"), 
  dataTableOutput(outputId = "tenpatents"),
  
  titlePanel("Patent Data Table from Jan 1, 2018 - March 31, 2018"),
  selectInput(inputId = "select_input",
              label = "State of Assignee Organizations",
              choices = c("All", unique(as.character(unnested_results$assignee_lastknown_state))),
              selected = "All"
              
  ),
  
  
  textInput(inputId = "text_input",
            label = "Inventor's Last Name",
            value = ""),
  
  

  htmlOutput("sum"),
  dataTableOutput("table_output"),
  plotOutput(outputId = "fourassignees"),
  plotOutput(outputId = "fiveinventors"),
  plotOutput(outputId = "fivecountries")
  
)


#Server
my_server <- function(input, output) {
  
  
  output$fourassignees <- renderPlot({
    
    
    fourassignee <- rev(sort(table(unnested_results$assignee_organization)))[1:4]
    
    barplot(fourassignee,
            main = "Top 4 Assignee Organizations",
            xlab = "List of Assignees",
            col = "gray",
            ylab = "Number of patents"
    )
  })
  
  output$fiveinventors <- renderPlot({
    
    proinventors <- rev(sort(table(unnested_results$inventor_last_name)))[1:5]
    
    barplot(proinventors,
            main = " 5 Most Prolific Inventors",
            xlab = "List of Inventors",
            col = "pink",
            ylab = "Number of patents"
    )
  })
  
  output$fivecountries <- renderPlot({
    
    topcountries <- rev(sort(table(unnested_results$inventor_country)))[1:5]
    
    barplot(topcountries,
            main = "Top 5 Countries",
            xlab = "List of Countries",
            col = "orange",
            ylab = "Number of patents"
            
    )
  })
  
  
  #DataTable
  output$table_output = renderDataTable({
    
    main_table <- unnested_results %>%
      
      select("patent_number",
             "patent_date",
             "inventor_last_name",
             "inventor_city",
             "assignee_organization",
             "assignee_lastknown_state")
    main_table
    
    if(input$select_input != "All") {
      main_table <- main_table %>%
        filter(assignee_lastknown_state == input$select_input)
    }
    
    if(str_length(input$text_input) > 0){
      main_table <- main_table %>%
        filter(tolower(inventor_last_name) == tolower(input$text_input))
    }
    
    
    print(main_table)
    main_table
    
  })
  #sum information
  output$sum = renderPrint({
    paste("Number of Patents: ", length(x),
          "
          Number of Assignees: ", length(y),
          "
          Number of Inventors: ", length(z))})
  #tenpatents data table
  output$tenpatents = renderDataTable({
    
    top10patents = as.data.frame(table(unnested_results$assignee_organization))
    
    return(top10patents) %>%
      filter(top10patents$Freq > 10) 
  })
  }


shinyApp(ui = my_ui, server = my_server)
