library(shiny)
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)

updated_data <- suppressWarnings(read_csv("updated_data.csv"))
rd_cost <- read_excel("r&d_costs2010_2019.xlsx", skip = 4) %>%
    clean_names() %>%
    rename(year = x1) %>%
    mutate(extension_cohort = as.numeric(extension_cohort)) %>%
    pivot_longer(cols = -year, names_to = "cohort", values_to = "costs")




drugs_list <- updated_data %>%
    pull(ndc_description) %>%
    unique()


# Define UI for app ----
ui <- fluidPage(
    
    
    navbarPage("Drug Price Trends",
               
               # Sets up panels ----
               tabPanel("Drug Prices",
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
               
               tabPanel("R&D Costs",
                        mainPanel(

                            plotOutput("RD_trend")

                        )

               ),
               
               tabPanel("About",
                        p("Up to this point I have drug acquisition prices for some of 
                          the most prescribed drugs in the US. Acquisition prices are the prices that local 
                          pharmacies pay for a unit of the drug. This generally is a good proxy for prices payed by patients as pharmacy 
                          prices for the drug will reflect changes in the drug acquisition price. This week I added data that tracked 
                          the average R&D cost of developing a drug from 2010 to 2019. The next steps would be finding data that relates R&D cost
                          to revenue earned by Pharma firms in the US due to the US premium for drugs. Drug Prices in the US are often much more
                          inflated, and the justification often is that this premium covers the R&D prices. If this claim is true, the earnings
                          from the premium should be roughly similar to R&D costs. I also want to be able to group certain 
                          drugs based on the conditions they treat to see if there are general trends for drug prices by condition.  
                          \n repo link: https://github.com/YishakAli/final_project"))
               
               
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
                 y = "Mean Price in US Dollars") +
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
            theme_bw() +
            labs(title = input$selected_brand,
                 x = "Year",
                 y = "Mean Price in US Dollars")
        
    })
    
    
    output$RD_trend <- renderPlot({
        rd_cost %>%
            ggplot(aes(year, costs, fill = cohort)) +
                geom_col(position = position_dodge2(preserve = "single")) +
                labs(title = "Average R&D Cost to Develop a Pharmaceutical Compound, 2010-2019",
                     x = "Year", y = "Cost in Billions of US Dollars") +
                scale_fill_manual(name = "Cohort", values = c("#0b5563", "#A5E6BA"))
                
            


    })
    
}

shinyApp(ui, server)