library(shiny)
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)

updated_data <- suppressWarnings(read_csv("updated_data.csv"))




drugs_list <- updated_data %>%
    pull(ndc_description) %>%
    unique()


# Define UI for app ----
ui <- fluidPage(
    
    
    navbarPage("Drug Price Trends",
               
               # Sets up panels ----
               tabPanel("Main",
                        mainPanel(
                            
                            # Input: select for the brand ----
                            selectInput(inputId = "selected_brand",
                                        label = "Brand:",
                                        choices = drugs_list),
                            
                            textOutput("brand_message"),
                            plotOutput("all_brand_plot"),
                            plotOutput("select_brand_plot")
                        ),
                        
               ),
               tabPanel("About",
                        p("Up to this point I have drug acquisition prices for some of 
                          the most prescribed drugs in the US. Acquisition prices are the prices that local 
                          pharmacies pay for a unit of the drug. This generally is a good proxy for prices payed by patients as pharmacy 
                          prices for the drug will reflect changes in the drug acquisition price. The next steps would be finding data on R&D 
                          costs and seeing if they have also gone up in relation to drug prices. I also wanna be able to group certain 
                          drugs based on the conditions they treat to see if there are general trends for drug prices by condition."))
               
               
    )
)

server <- function(input, output, session) {
    output$state_message <- renderText({
        paste0("This is the brand you chose: ", 
               input$selected_brand)      
    })
    
    output$all_brand_plot <- renderPlot({
        updated_data %>% 
            ggplot(aes(x = effective_date, y = mean_price, 
                       color = ndc_description)) +
            geom_line() +
            theme_classic() +
            labs(title = "Drug Acquisition Price per Unit of the Most Prescribed Drugs in the US",
                 x = "Year",
                 y = "Mean Price") +
            scale_color_manual(values = c("#0b5563", "#A2BCE0", 
                                          "#A5E6BA", "#420039", 
                                          "#FF5400", "#81E979", 
                                          "#524632", "#DEDBD8", 
                                          "#ADBABD", "#6CBEED", 
                                          "#4CE0D2", "#F9DC5C",
                                          "#FC60A8"), name = "Brand Names")
    })
    
    output$select_brand_plot <- renderPlot({
        updated_data %>% 
            filter(ndc_description == input$selected_brand) %>% 
            ggplot(aes(x = effective_date, y = mean_price)) +
            geom_line() +
            theme_classic() +
            labs(title = input$selected_brand,
                 x = "Year",
                 y = "Mean Price")
        
    })
    
}

shinyApp(ui, server)