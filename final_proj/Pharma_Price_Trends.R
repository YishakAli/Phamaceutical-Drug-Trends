library(shiny)
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(gganimate)
library(ggthemes)
library(formattable)
library(patchwork)
library(gt)


updated_data <- suppressWarnings(read_csv("updated_data.csv"))

price_growth <- read_csv("price_growth.csv")

rd_cost <- read_excel("r&d_costs2010_2019.xlsx", skip = 4) %>%
    clean_names() %>%
    rename(year = x1) %>%
    mutate(extension_cohort = as.numeric(extension_cohort)) %>%
    pivot_longer(cols = -year, names_to = "cohort", values_to = "costs")

us_premium <- read_excel("US_premium_rd_costs.xlsx") %>% 
    clean_names() %>%
    mutate(company = as.factor(company))

g7_spending <- read_csv("g7_spending_per_cap.csv") %>% 
    clean_names() %>% 
    select(location, time, value) %>% 
    rename(Country = location)

therapeutic_classes <- read.csv("final_proj/therapeutic_classes.csv")

therapeutic_list <- therapeutic_classes %>% 
    pull(therapeutic_class) %>% 
    unique()


drugs_list <- updated_data %>%
    pull(ndc_description) %>%
    unique()


# Define UI for app ----
ui <- fluidPage(
    
    
    navbarPage("Pharmaceutical Drug Price Trends",
               
               # Sets up panels ----
               tabPanel("Drug Prices",
                        mainPanel(
                            
                            
                            plotOutput("all_brand_plot"),
                            
                            # Input: select for the brand ----
                            selectInput(inputId = "selected_brand",
                                        label = "Brand:",
                                        choices = drugs_list),
                            
                            plotOutput("select_brand_plot"),
                            plotOutput("Price_comparisons")
                        ),
                        
                        p("A look at the 12 most prescribed drugs in the US reveals that their unit drug acquistion prices, 
                          the prices at which pharmacies buy a unit of the drug,have been for the most part
                          increasing at a rapid pace in recent years. Acquisition prices are a good proxy for the unit prices
                          paid by US consumers, so this rise in acquisition prices is reflective of a similar rise in the price paid
                          by consumers."), 
                        
                        
                        sidebarPanel(
                            
                            gt_output(outputId = "Price_growth")
                        
                        ),
                        
                        p("This concerning trend of increasing prices in the US is further compounded by the fact that US prices are consistently
                          above that of international prices for the same drugs, raising questions about why prices are increasing in the US and why prices in the 
                          US are inflated relative to elsewhere to begin with."), 
                        
                       sidebarPanel(
                            
                            imageOutput("spending_cap")
                        ),
                        
                        
                        
               ),
               
               tabPanel("R&D Costs",
                        mainPanel(
                            
                            plotOutput("RD_trend"),
                            plotOutput("Excess_profits")
                            
                        ),
                        
                        p("Firms often justify the higher price of pharmacuetical drugs in the US, the US premium, by arguing that the premium helps cover 
                         R&D costs. The data shows that there is some validity in this claim. In two cohorts of pharmaceutical firms, average 
                         R&D cost has been steadily on the rise"),
                        
                        p("Even if costs have been going up, we still have to check if the price these firms charge is reasonable for their R&D costs.
                         If this claim about the US premium being a way to offset the costs of rising R&D is true, then we should see that the 
                         revenue earned by the US premium be roughly equal to the R&D cost needs. The results, however, suggest that firms are taking advantage of the US premium
                         to upmark their products well beyond R&D needs."),
                        
               ),
               
               
               tabPanel("Model",
                        mainPanel(
                            
                            TableOutput("regression")
                            
                        ),
                        
                        p("Model stuff here")
                        
               ),
               
               tabPanel("About",
                        p("Up to this point I have drug acquisition prices for some of 
                          the most prescribed drugs in the US. Acquisition prices are the prices that local 
                          pharmacies pay for a unit of the drug. This generally is a good proxy for prices payed by patients as pharmacy 
                          prices for the drug will reflect changes in the drug acquisition price. Last week, I added data that tracked 
                          the average R&D cost of developing a drug from 2010 to 2019. This week I added a graphic that relates R&D cost
                          to revenue earned by Pharma firms in the US due to the US premium for drugs. Drug Prices in the US are often much more
                          inflated, and the justification often is that this premium covers the R&D prices. If this claim is true, the earnings
                          from the premium should be roughly similar to R&D costs. It turns out this claim is doubtful. The next steps are working on a predictive model
                          to predict the price of a drug in a country based on various predictors like country, healthcare system, condition treated, and abudance of genetic
                          substitutes. I also want to be able to group certain 
                          drugs based on the conditions they treat to see if there are general trends for drug prices by condition.  
                          \n repo link: https://github.com/YishakAli/Phamaceutical-Drug-Trends"))
               
               
    )
)

server <- function(input, output, session) {
    output$all_brand_plot <- renderPlot({
        updated_data %>% 
            ggplot(aes(x = effective_date, y = mean_price, 
                       color = ndc_description)) +
            geom_line() +
            theme_classic() +
            labs(title = "Drug Acquisition Price per Unit of the Most Prescribed Drugs in the US",
                 x = "Year",
                 y = "Mean Price in US Dollars",
                 caption = "Source: First Data Bank Health") +
            scale_color_manual(values = c("#0b5563", "#A2BCE0", 
                                          "#A5E6BA", "#420039", 
                                          "#FF5400", "#81E979", 
                                          "#524632", "#DEDBD8", 
                                          "#ADBABD", "#6CBEED", 
                                          "#4CE0D2", "#F9DC5C",
                                          "#FC60A8"), name = "Brand Names") +
            theme_economist_white() +
            scale_fill_economist() +
            theme(legend.title = element_text(size = 15), 
                  legend.text = element_text(size = 10))
    })
    
    output$Price_growth <- render_gt({
        price_growth %>%
            select(ndc_description, per_chg, avg_rate) %>%
            filter(ndc_description == input$selected_brand) %>% 
            rename(Brand = ndc_description,
               `Percent Change` = per_chg ,
               `Avg Rate of Change (US $)` = avg_rate) %>%
            gt()  %>%

            tab_style(style = list(cell_fill(color = "#EF233C"),
                                   cell_text()),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` < 0)) %>%
            tab_style(style = list(cell_fill(color = "#C7EFCF"),
                                   cell_text()),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` > 0 & `Percent Change` < 17)) %>%
            tab_style(style = list(cell_fill(color = "#88D18A"),
                                   cell_text()),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` > 17 & `Percent Change` < 100)) %>%
            tab_style(style = list(cell_fill(color = "#04724D"),
                                   cell_text()),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` > 100)) %>%
            tab_style(style = list(cell_fill(color = "#EF233C"),
                                   cell_text()),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` < -5)) %>%
            tab_style(style = list(cell_fill(color = "#F4796B"),
                                   cell_text()),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` < 0 & `Percent Change` > -5)) %>% 
            tab_style(style = list(cell_fill(color = "#F4796B"),
                                   cell_text()),
                      locations = cells_body(columns = vars(`Avg Rate of Change (US $)`),
                                             rows = `Avg Rate of Change (US $)` < 0)) %>%
            tab_style(style = list(cell_fill(color = "#04724D"),
                                   cell_text()),
                      locations = cells_body(columns = vars(`Avg Rate of Change (US $)`),
                                             rows = `Avg Rate of Change (US $)` > .4)) %>%
            tab_style(style = list(cell_fill(color = "#88D18A"),
                                   cell_text()),
                      locations = cells_body(columns = vars(`Avg Rate of Change (US $)`),
                                             rows = `Avg Rate of Change (US $)` > .1 & `Avg Rate of Change (US $)` < .4)) %>%
            tab_style(style = list(cell_fill(color = "#C7EFCF"),
                                   cell_text()),
                      locations = cells_body(columns = vars(`Avg Rate of Change (US $)`),
                                             rows = `Avg Rate of Change (US $)` > 0 & `Avg Rate of Change (US $)` < .1))
            
            
            
 
     })
    
    
    output$Price_comparisons <- renderPlot({
        us_premium %>%
            rename(Company = company) %>% 
            ggplot(aes(x = fct_reorder(Company, international_price_us_price), 
                       y = international_price_us_price, 
                       fill = Company)) +
            geom_col() +
            theme(legend.position = "none") +
            labs(title = "International Drug Prices as a Percentage of US Prices, 2015",
                 subtitle = "US prices are consistently above international prices",
                 x = "Firms",
                 y = "Percentage",
                 caption = "Health Affairs Blog") +
            geom_text(aes(label = international_price_us_price*100)) +
            scale_y_continuous(labels = scales::percent_format(),
                               breaks = c(2, 4, 6, 8)) +
            geom_hline(yintercept = mean(us_premium$international_price_us_price),
                       color = "blue") +
            annotate("text", label = "Average percentage for listed firms", x = 2.5, y = .45) +
            theme_economist() +
            theme(axis.text.x = element_text(angle = 90)) +
            theme(legend.title = element_text(size = 10), 
                  legend.text = element_text(size = 10)) 

       
            
    })
    
    output$select_brand_plot <- renderPlot({
        updated_data %>% 
            filter(ndc_description == input$selected_brand) %>% 
            ggplot(aes(x = effective_date, y = mean_price)) +
            geom_line() +
            theme_bw() +
            labs(title = input$selected_brand,
                 x = "Year",
                 y = "Mean Price in US Dollars",
                 caption = "Source: First Data Bank Health") +
            theme_economist_white() +
            scale_color_economist()
        
    })
    
    
    output$RD_trend <- renderPlot({
        rd_cost %>%
            ggplot(aes(year, costs, fill = cohort)) +
            geom_col(position = position_dodge2(preserve = "single")) +
            geom_text(aes(label = costs), position = position_dodge2(.9), color = "blue") +
            labs(title = "Average R&D Cost to Develop a Pharmaceutical Compound, 2010-2019",
                 x = "Year", y = "Cost in Billions of US Dollars",
                 caption = "Source: Deloitte LLP, 2019" ) +
            scale_fill_manual(name = "Cohort", values = c("#A5E6BA", "#E5BEED"))
        
        
        
        
        
        
    })
    
    
    output$Excess_profits <- renderPlot({
        us_premium %>% 
            ggplot(aes(x =fct_reorder(company, revenues_from_us_prmium_as_percent_of_global_research_and_development), 
                       y = revenues_from_us_prmium_as_percent_of_global_research_and_development,
                       fill = company)) +
            geom_col() +
            scale_y_continuous(labels = scales::percent_format()) +
            geom_hline(yintercept = 1, color = "#2E294E") +
            geom_hline(yintercept = mean(us_premium$revenues_from_us_prmium_as_percent_of_global_research_and_development), 
                       color = "#1B998B") +
            annotate("text", label = "1:1; fair US premium revenue to \n R&D cost ratio",
                     x = 2.2, y = 1.2, 
                     size = 3, color = "#2E294E") +
            annotate("text", label = "1.63:1; actual average ratio",
                     x = 2.2, y = 1.75, 
                     size = 3, color = "#1B998B") +
            abline() +
            theme(axis.text.x = element_text(angle = 90)) +
            theme(legend.position = "none") +
            labs(title = "63% Difference, on Average, Between US Premium Revenue and R&D Cost Needs, 2015",
                 subtitle = "Pharmaceutical firms are making excess profits from US premium at expense of patients",
                 x = "Firms",
                 y = "Percentage",
                 caption = "Source: Health Affairs Blog") +
            theme(plot.title = element_text(size = 12.4))
        
        
        
        
    })
    
    
    output$spending_cap <- renderImage({
        outfile <- tempfile(fileext='.gif')
        g7_spending %>% 
            ggplot(aes(time, value, color = Country)) +
            labs(title = "US spending an outlier among similar wealthy, developed nations ",
                 subtitle = "Phamaceutical spending per capita for G7 countries, 1970 - 2019",
                 x = "Year",
                 y = "Spending per Capita in USD",
                 caption = "Source: OECD (2020), Pharmaceutical spending (indicator).\n doi: 10.1787/998febf6-en") +
            scale_y_continuous(labels = scales::dollar_format()) +
            geom_line() +
            geom_point() +
            transition_reveal(time) +
            theme_economist() +
            theme(plot.title = element_text(size = 12.5)) +
            theme(legend.title = element_text(size = 10), 
                  legend.text = element_text(size = 10)) 
        
        anim_save("outfile.gif", animate(p)) 
        
        list(src = "outfile.gif",
             contentType = 'image/gif',
              width = 390,
              height = 400,
              # alt = "This is alternate text"
              deleteFile = TRUE)
        
        
        
    })
    

    
}

shinyApp(ui, server)
