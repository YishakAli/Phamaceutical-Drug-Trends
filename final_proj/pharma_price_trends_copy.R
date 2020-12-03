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
    mutate(Company = as.factor(company)) %>% 
    mutate(Id = 1:15)

g7_spending <- read_csv("g7_spending_per_cap.csv") %>% 
    clean_names() %>% 
    select(location, time, value) %>% 
    rename(Country = location)

therapeutic_classes <- read.csv("therapeutic_classes.csv")

therapeutic_list <- therapeutic_classes %>% 
    pull(therapeutic_class) %>% 
    unique()

year_list <- therapeutic_classes %>% 
    pull(year) %>% 
    unique()


drugs_list <- updated_data %>%
    pull(ndc_description) %>%
    unique()

final_set <- read_csv("international_prices.csv") %>% 
    clean_names() %>% 
    pivot_longer(cols = c("int_price_per_unit", "usa_price_per_unit"),
                 names_to = "location", values_to = "price") %>% 
    select(drug_name, condition, type, total_options, price, location)


# Define UI for app ----
ui <- fluidPage(
    
    
    navbarPage("Pharmaceutical Drug Price Trends",
               
               # Sets up panels ----
               
               # tabsetPanel(
               tabPanel("Drug Prices",
                        mainPanel(
                            
                        tabsetPanel(
                            tabPanel("Most Prescribed", plotOutput("all_brand_plot")),
                            tabPanel("By Therapeutic Class", fluidRow(column(5, plotOutput("t_classes")),
                                     column(3, offset = 2, checkboxGroupInput(inputId = "select_class",
                                                        label = "Therapeutic Class",
                                                         choices = therapeutic_list,
                                                         selected = therapeutic_list)),
                                     column(2, sliderInput(inputId = "select_year",
                                                 label = "Year Range",
                                                 min = 1996,
                                                 max = 2018,
                                                 value = c(1996, 2018)))))

                        )
                        ),
                        
                        sidebarPanel(gt_output(outputId = "drug_condition_treat")),
                        
                        
                            
                            # Input: select for the brand ----
                            selectInput(inputId = "selected_brand",
                                        label = "Brand:",
                                        choices = drugs_list,
                                        selected = drugs_list[5]),
                            
                            
                            fluidRow(
                                column(4,
                                       plotOutput("select_brand_plot")
                                     
                                       
                                        ),
                                
                                column(4,
                                       
                                       gt_output(outputId = "Price_growth")
                      
                                       
                                        ),
                                
                                column(4,
                                       
                                       p("A look at the current 10 most prescribed drugs in the US reveals that their unit drug acquistion prices, 
                          the prices at which pharmacies buy a unit of the drug, have been for the most part
                          increasing at a rapid pace in recent years. Acquisition prices are a good proxy for the unit prices
                          paid by US consumers, so this rise in acquisition prices is reflective of a similar rise in the price paid
                          by consumers.")
                                       
                               
                        #          column(12,
                        #                
                        #                p("This concerning trend of increasing prices in the US is further compounded by the fact that US prices are consistently
                        #   above that of international prices for the same drugs, raising questions about why prices are increasing in the US and why prices in the 
                        # US are inflated relative to elsewhere to begin with.")
                        #                 )
                                       
                                
                                ),
                            
                            fluidRow(
                                column(7,
                                       
                                       p("This concerning trend of increasing prices in the US is further compounded by the fact that US prices are consistently
                          above that of international prices for the same drugs, raising questions about why prices are increasing in the US and why prices in the 
                        US are inflated relative to elsewhere to begin with.")
                                ),
                                
                                column(8,
                                       plotOutput("Price_comparisons")
                                       
                                       
                                ),
                                
                                column(4,
                                       imageOutput("spending_cap")
                                       
                                       
                                ))
                            
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
                            
                            p("This is a predictive model that estimates the average price of a drug given the condition it treats,
                              the number of substitutes, whether the drug is bought in or outside of the US, and whether it's a generic or brand drug.
                              The estimate of the intercept represents the predicted average cost per unit of a brand drug treating acid reflux, with no
                              other substitutes, bought outside of the US. The other estimates correspond to the associated increase or decrease in average
                              cost per unit of the given predictor. Interestingly, every additional substitute for a drug is associated with a decrease of $15
                              in average price. Additionally, generic drugs are estimated to be cheaper on average than brand names. The model also captures the higher
                              drug prices observed in the US relative to international, as reflected by the significant positive estimate associated with buying in the US."),
                            
                            tableOutput("regression"))
                        
                         
                       
               ),
               
               tabPanel("About",
                        p("In recent years there has been outrage about rising pharmaceutical drug costs in the US for many of the most prescribed medications in the US. Pharma companies
                        justify the costs by pointing to rising R&D costs but skeptics believe the elevated costs are outliers compared to international peers and
                        a means of raising profits at the expense of consumers. This project was inspired by this debate and seeks to examine these rising prices
                        in the context of R&D costs to figure out if firms are indeed raising prices beyond R&D needs.
                        
                        
                        The data for the most part confirms the claims of the skeptics. In terms of pharmaceutical spending per capita, domestic drug prices, and nearly all other
                        healthcare associated cost metrics, the US is an outlier: costs are absurdly high and still on the rise. Interestingly, R&D costs have been also on the rise,
                        however, this rise doesn't fully explain the increased revenue made from the US premium. This suggests that the price raises go beyond offsetting R&D costs and actually
                        serve to raise profits for Pharma firms.
                        
                        The model in the app attempts to predict the average cost of a drug based on a number of variables like where the drug is bought and whether it's generic or brand name.
                        As is observed, the model predicts a raise in average cost associated with US purchase and brand name. 

                          \n repo link: https://github.com/YishakAli/Phamaceutical-Drug-Trends"))
               
               
    )
)

server <- function(input, output, session) {
    output$all_brand_plot <- renderPlot({
        updated_data %>%
            filter(!ndc_description %in% c("VICODIN HP 10-300 MG TABLET", "FORTAMET ER 1,000 MG TABLET")) %>% 
            ggplot(aes(x = effective_date, y = mean_price, 
                       color = ndc_description)) +
            geom_line(size = .8) +
            geom_point() +
            labs(title = "Drug Acquisition Price per Unit of the Most Prescribed Drugs in the US",
                 x = "\nYear",
                 y = "Mean Price in US Dollars",
                 caption = "Source: First Data Bank Health") +
            scale_color_manual(values = c("#0b5563", "#A2BCE0", 
                                          "#A5E6BA", "#420039", 
                                          "#FF5400", "#81E979", 
                                          "#524632", "#A288E3", 
                                          "#EFCB68", "#6CBEED", 
                                          "#4CE0D2", "#F9DC5C",
                                          "#FC60A8"), name = "Brand Names") +
            theme_fivethirtyeight() +
            theme(legend.title = element_text(size = 10), 
                  legend.text = element_text(size = 8),
                  plot.title = element_text(color = "darkblue"),
                  axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
    })
    
    output$drug_condition_treat <- render_gt({
        tibble(Brand = c("Amoxicillin", "Cozaar", "Glucophage",
                         "Lipitor", "Neurontin", "Norvasc",
                         "Prilosec", "Prinivil", "Synthroid", "Zocor"),
               `Condition Treated` = c("Bacterial infection","Hypertension", "Diabetes",
                                       "High cholesterol", "Seizures, Nerve pain",
                                       "Hypertension", "Acid reflex/Heartburn, Ulcers",
                                       "Hypertension, Heart Failure", "Hypothyroidism",
                                       "High cholesterol")) %>% 
            gt() %>% 
            tab_style(style = list(cell_fill(color = "lightblue"),
                                   cell_text(weight = "bold")),
                      locations = cells_column_labels(columns = vars(Brand, `Condition Treated`))) %>% 
            cols_align(align = "center",
                       columns = vars(`Condition Treated`))
    })
    
    
    output$Price_growth <- render_gt({
        round_obj <- price_growth %>%
            select(ndc_description, per_chg, avg_rate) %>%
            filter(ndc_description == input$selected_brand) %>% 
            mutate(round_per = round(.$per_chg, digits = 2),
                   round_avg = round(.$avg_rate, digits = 4)) %>%
            select(ndc_description, round_per, round_avg, )
        
        round_obj %>% 
            rename(Brand = ndc_description,
               `Percent Change` = round_per ,
               `Avg Rate of Change (US $)` = round_avg) %>%
            gt()  %>%
            
            tab_style(
                style = list(cell_fill(color = "lightblue"),
                             cell_text(weight = "bold")),
                locations = cells_column_labels(columns = vars(Brand, `Percent Change`, 
                                                               `Avg Rate of Change (US $)`))) %>%
            cols_align(align = "center",
                       columns = vars(`Percent Change`, `Avg Rate of Change (US $)`)) %>% 

            tab_style(style = list(cell_fill(color = "#EF233C"),
                                   cell_text(weight = "bold")),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` < 0)) %>%
            tab_style(style = list(cell_fill(color = "#C7EFCF"),
                                   cell_text(weight = "bold")),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` > 0 & `Percent Change` < 17)) %>%
            tab_style(style = list(cell_fill(color = "#88D18A"),
                                   cell_text(weight = "bold")),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` > 17 & `Percent Change` < 100)) %>%
            tab_style(style = list(cell_fill(color = "#04724D"),
                                   cell_text(weight = "bold")),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` > 100)) %>%
            tab_style(style = list(cell_fill(color = "#EF233C"),
                                   cell_text(weight = "bold")),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` < -5)) %>%
            tab_style(style = list(cell_fill(color = "#F4796B"),
                                   cell_text(weight = "bold")),
                      locations = cells_body(columns = vars(`Percent Change`),
                                             rows = `Percent Change` < 0 & `Percent Change` > -5)) %>% 
            tab_style(style = list(cell_fill(color = "#F4796B"),
                                   cell_text(weight = "bold")),
                      locations = cells_body(columns = vars(`Avg Rate of Change (US $)`),
                                             rows = `Avg Rate of Change (US $)` < 0)) %>%
            tab_style(style = list(cell_fill(color = "#04724D"),
                                   cell_text(weight = "bold")),
                      locations = cells_body(columns = vars(`Avg Rate of Change (US $)`),
                                             rows = `Avg Rate of Change (US $)` > .4)) %>%
            tab_style(style = list(cell_fill(color = "#88D18A"),
                                   cell_text(weight = "bold")),
                      locations = cells_body(columns = vars(`Avg Rate of Change (US $)`),
                                             rows = `Avg Rate of Change (US $)` > .1 & `Avg Rate of Change (US $)` < .4)) %>%
            tab_style(style = list(cell_fill(color = "#C7EFCF"),
                                   cell_text(weight = "bold")),
                      locations = cells_body(columns = vars(`Avg Rate of Change (US $)`),
                                             rows = `Avg Rate of Change (US $)` > 0 & `Avg Rate of Change (US $)` < .1))
            
            
            
 
     })
    
    
    output$Price_comparisons <- renderPlot({
        us_premium %>%
            ggplot(aes(x = fct_reorder(Company, international_price_us_price), 
                       y = international_price_us_price, 
                       fill = Company)) +
            geom_col() +
            theme(legend.position = "none") +
            labs(title = "International Drug Prices as a Percentage of US Prices, 2015\n",
                 subtitle = "US prices are consistently above international prices",
                 x = "Firms",
                 y = "Percentage",
                 caption = "Source: Health Affairs Blog") +
            geom_text(aes(label = international_price_us_price*100)) +
            scale_y_continuous(labels = scales::percent_format(),
                               breaks = c(2, 4, 6, 8)) +
            geom_hline(yintercept = mean(us_premium$international_price_us_price),
                       color = "blue", lty = 2) +
            annotate("text", label = "Average percentage for listed firms", x = 2.5, y = .45) +
            theme_economist() +
            theme(axis.text.x = element_text(angle = 90)) +
            theme(legend.title = element_text(size = 10), 
                  legend.text = element_text(size = 10),
                  plot.title = element_text(color = "#C05746")) 

       
            
    })
    
    output$select_brand_plot <- renderPlot({
        updated_data %>% 
            filter(ndc_description == input$selected_brand) %>%
            filter(!ndc_description %in% c("VICODIN HP 10-300 MG TABLET", "FORTAMET ER 1,000 MG TABLET")) %>%
            ggplot(aes(x = effective_date, y = mean_price)) +
            geom_line(color = "coral") +
            geom_point() +
            theme_bw() +
            labs(title = input$selected_brand,
                 x = "\nYear",
                 y = "Mean Price in US Dollars",
                 caption = "Source: First Data Bank Health") +
            theme_clean() +
            scale_color_economist() +
            theme(plot.title = element_text(color = "darkblue"),
                  axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
           
        
    })
    
    
    output$RD_trend <- renderPlot({
        rd_cost %>%
            ggplot(aes(year, costs, fill = cohort)) +
            geom_col(position = position_dodge2(preserve = "single")) +
            geom_text(aes(label = costs), position = position_dodge2(.9), color = "blue") +
            labs(title = "Average R&D Cost to Develop a Pharmaceutical Compound, 2010-2019",
                 x = "Year", y = "Cost in Billions of US Dollars",
                 caption = "Source: Deloitte LLP, 2019" ) +
            scale_fill_manual(name = "Cohort", values = c("#A5E6BA", "#E5BEED")) +
            theme_clean()
        
        
        
        
        
        
    })
    
    
    output$Excess_profits <- renderPlot({
          us_premium %>%
                ggplot(aes(x =fct_reorder(company, revenues_from_us_prmium_as_percent_of_global_research_and_development),
                           y = revenues_from_us_prmium_as_percent_of_global_research_and_development,
                           fill = Company)) +
                    geom_col() +
                    # transition_reveal(Id) +
                    scale_y_continuous(labels = scales::percent_format()) +
                    geom_hline(yintercept = 1, lty = 2, color = "#2E294E") +
                    geom_hline(yintercept = mean(us_premium$revenues_from_us_prmium_as_percent_of_global_research_and_development),
                               lty = 2, color = "#1B998B") +
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
                    theme_clean() +
                    theme(axis.text.x = element_text(angle = 90),
                          plot.title = element_text(size = 12.4))
        
        # 
        # excess_profits_file <- tempfile(fileext = '.gif')
        # anim_save("excess_profits_file.gif", animate(p))

        # list(src = "excess_profits.gif",
        #      contentType = 'image/gif',
        #      width = 390,
        #      height = 400,
        #      # alt = "This is alternate text"
        #      deleteFile = F)
        
    })
    
    
    output$t_classes <- renderPlot({
        therapeutic_classes %>% 
            filter(therapeutic_class == input$select_class,
                   year == input$select_year) %>% 
            group_by(therapeutic_class, year) %>% 
            summarize(expend_per_purchase = log((expenditure*1000000)/(1000*purchases))) %>% 
            drop_na() %>% 
            ggplot(aes(year, expend_per_purchase, color = therapeutic_class)) +
            geom_line() +
            theme(legend.title = element_text(size = 5), 
                  legend.text = element_text(size = 5),
                  legend.position = "bottom") + 
            stat_summary(fun.y=mean, aes(group=1), geom="line", colour="darkblue", size = 1.2, lty = 3)
        
        
        
        
    })
    
    
    output$spending_cap <- renderImage({
        outfile <- tempfile(fileext='.gif')
        # g7_spending %>%
        #     ggplot(aes(time, value, color = Country)) +
        #     labs(title = "US spending an outlier among similar wealthy, developed nations\n",
        #          subtitle = "\nPhamaceutical spending per capita for G7 countries, 1970 - 2019",
        #          x = "\n Year",
        #          y = "Spending per Capita in USD",
        #          caption = "Source: OECD (2020), Pharmaceutical spending (indicator).\n doi: 10.1787/998febf6-en") +
        #     scale_y_continuous(labels = scales::dollar_format()) +
        #     geom_line() +
        #     geom_point() +
        #     transition_reveal(time) +
        #     theme_economist() +
        #     theme(plot.title = element_text(size = 12.5, 
        #                                     color = "#C05746")) +
        #     theme(legend.title = element_text(size = 10),
        #           legend.text = element_text(size = 10),
        #           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

        # anim_save("outfile.gif", animate(p))
        
        list(src = "outfile.gif",
             contentType = 'image/gif',
              width = 390,
              height = 400,
              # alt = "This is alternate text"
              deleteFile = T)
        
    
    })
    
    output$regression <- renderTable({
        fit_model <- stan_glm(formula = price ~ condition + type + total_options + location,
                              data = final_set, refresh = 0, seed = 10)
        
        
        tbl_regression(fit_model, intercept = T) %>% 
            as_gt() %>% 
            tab_header(title = "Regression of Drug Prices" ,
                       subtitle = "The Effect of Various Predictors on Prices") %>% 
            tab_source_note(md("Source: PharmacyChecker & Good Rx"))
        
        
    })
    
}

shinyApp(ui, server)
