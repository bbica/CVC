#Libraries
library(shiny)
library(tidyverse)
library(caret)
library(randomForest)
library(dplyr)
library(shiny)
library(data.table)
library(ggplot2)
library(shinyjs)
library(DT)

# Define UI
ui <- fluidPage(
    tags$head(
        
        # Note the wrapping of the string in HTML()
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@400..900&display=swap');
      body {
        background-color: lightgrey;
        color: black;
      }
      h2 {
        font-family:'Orbitron', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }"))
    ),
    shinyjs::useShinyjs(),
    div(
        id="myapp", #Start of header
        shiny::titlePanel(title = div(
            shiny::splitLayout(
                cellWidths = NULL,
                h4("CVC - Clinical Variant Classifier", align = "left"),
                div(
                    style = "position:absolute;top:1em; right:1em;",
                    a(img(height = 60, width = 60, src="www/logo.png"), href="https://github.com/bbica/") ,
                ))
        ))
        
    ),#end of div
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            fileInput("file", "Upload File"),
            actionButton("predict_call", "Predict")
        ),
        mainPanel( 
            h5("Historical data from MongoDB"),
            DTOutput("historical_table"),
            br(),
            DTOutput("prediction_table")
           
        )
    )
)

# Define server logic
server <- function(input, output) {
    # Read data from uploaded file
    #Initiate variables
    variable_data<-reactiveValues(historical_data = readRDS("./www/data_training.RDS"), model= readRDS("./www/model.RDS"))
    user_data<-reactiveValues(df=NULL)
    #Observe event Start
    output$historical_table<-renderDT(variable_data$historical_data)
    
    
    observeEvent(input$file, {
        req(input$file)
        infile <- input$file
        print(infile$datapath)
        user_data$df <- data.table::fread(infile$datapath, stringsAsFactors = FALSE)
        # user_data$df<-as_tibble(user_data$df)
        # user_data$df$functional_annotation <- as.factor(user_data$df$functional_annotation)
        # user_data$df$CADD<-as.double
        
        
        output$prediction_table<-renderDT(user_data$df)
        
    })#end observeEvent input
    
    observeEvent(input$predict_call, {
        user_data$df<-rbind(variable_data$historical_data[1,-7], user_data$df)
        user_data$df<-user_data$df[-1,]
        predictions <- predict(variable_data$model, newdata = user_data$df) 
        user_data$df$prediction<-predictions
        output$prediction_table<-renderDT(user_data$df)
    })#
    
    
    
    #colnames(data)<-janitor::make_clean_names(colnames(data))
    # Make predictions based on user input
    #  observeEvent(input$predict_btn, { #button to upload example data (DRAM-v)
    #     model <- randomForest(disease_related ~ allele_frequency + conservation_score + functional_annotation + predicted_impact, 
    #                           data = data, 
    #                           ntree = 1000, 
    #                           mtry = 3, 
    #                           importance = TRUE)
    #      predictions <- predict(model, newdata = data)
    #     },    
    #     )
    # 
    #    # Simulating predictions  
    # 
    # conf_matrix <- confusionMatrix(predictions, test_data$disease_related)
    # print(conf_matrix)
    #     
    #     #predictions <- data() %>%
    #     #    predict(model)
    #     
    #     # Display prediction results in a table
    #     output$prediction_table <- renderTable({
    #         predictions[, c("variant_id", "likelihood_of_disease")]
    #     })
    #     
    #     # Create a scatter plot
    #     output$prediction_plot <- renderPlot({
    #         ggplot(predictions, aes(x = variant_id, y = likelihood_of_disease)) +
    #             geom_point() +
    #             labs(x = "Variant ID", y = "Likelihood of Disease")
    #     })
     }

#showNotification("Done")
# Run the Shiny app
shinyApp(ui, server)