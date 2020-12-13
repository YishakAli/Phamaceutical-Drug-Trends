library(tidyverse)
library(janitor)
library(readxl)
library(gganimate)
library(formattable)
library(ztable)
library(magrittr)
library(condformat)
library(rstanarm)
library(patchwork)
library(ggthemes)
library(gt)
library(gtsummary)
library(broom.mixed)
library(shinythemes)

# Loads in drug acquisition data set

updated_data <- suppressWarnings(read_csv("updated_data.csv"))

price_growth <- read_csv("price_growth.csv")

# Loads in data about R&D cost over the years

rd_cost <- read_excel("r&d_costs2010_2019.xlsx", skip = 4) %>%
    clean_names() %>%
    rename(year = x1) %>%
    mutate(extension_cohort = as.numeric(extension_cohort)) %>%
    pivot_longer(cols = -year, names_to = "cohort", values_to = "costs")

# Loads in the data about US premium revenue

us_premium <- read_excel("US_premium_rd_costs.xlsx") %>% 
    clean_names() %>%
    mutate(Company = as.factor(company)) %>% 
    mutate(Id = 1:15)

# Loads in data about g7 pharma spending per capita

g7_spending <- read_csv("g7_spending_per_cap.csv") %>% 
    clean_names() %>% 
    select(location, time, value) %>% 
    rename(Country = location)

# Reads in data about expenditure per purchase for the different therapeutic classes

therapeutic_classes_final <- read_csv("therapeutic_classes.csv")

therapeutic_list <- therapeutic_classes_final %>% 
    pull(therapeutic_class) %>% 
    unique()

# Creates an object that stores the list of years available to be used in a slider later

year_list <- therapeutic_classes_final %>% 
    pull(year) %>% 
    unique()

# Creates an object that stores the list of brands users can choose from in a later drop down

drugs_list <- updated_data %>%
    pull(ndc_description) %>%
    unique()

# Reads in data used for model

final_set <- read_csv("international_prices.csv") %>% 
    clean_names() %>% 
    pivot_longer(cols = c("int_price_per_unit", "usa_price_per_unit"),
                 names_to = "location", values_to = "price") %>% 
    select(drug_name, condition, type, total_options, num_subs, price, location)

# Fits the model with an intercept, using type, number of alternatives and location as 
# predictors for average price per unit

fit_model <- stan_glm(formula = price ~  type + num_subs + location,
                      data = final_set, refresh = 0, seed = 200)

# All the "obj" objects store hypothetical drugs that can be input into the model 
# to predict their average price

obj <- tibble(type = c("brand", "generic"),
              num_subs = 1, location = "usa_price_per_unit")

# All the "predict" objects create and store predictions for the price per unit of the
# given hypothetical drug

predict <- posterior_predict(fit_model, newdata = obj) %>% 
    as_tibble() %>% 
    mutate_all(as.numeric) %>% 
    rename(brand = 1, generic = 2)

brand_mean <- mean(predict$brand)
generic_mean <- mean(predict$generic)

obj_2 <- tibble(type = "brand",
                num_subs = 1, location = c("usa_price_per_unit", "int_price_per_unit"))

predict_2 <- posterior_predict(fit_model, newdata = obj_2) %>% 
    as_tibble() %>% 
    mutate_all(as.numeric) %>% 
    rename(usa = 1, international = 2)

usa_mean <- mean(predict_2$usa)
int_mean <- mean(predict_2$international)

obj_3 <- tibble(type = "brand",
                num_subs = c(1,2,3), location = "usa_price_per_unit")

predict_3 <- posterior_predict(fit_model, newdata = obj_3) %>% 
    as_tibble() %>% 
    mutate_all(as.numeric) %>% 
    rename(one = 1, two = 2, three = 3) 

one_mean <- mean(predict_3$one)
two_mean <- mean(predict_3$two)
three_mean <- mean(predict_3$three)

# Defines UI for app 

ui <- fluidPage(theme = shinytheme("flatly"),
    
    
    navbarPage("Fair or Exploitative Pricing: Pharmaceutical Drug Price Trends",
               
               # Sets up the home tab and all the objects in it
               
               tabPanel("Home",
                        
                        # Creates and centers the heading 
                        
                        h1("Welcome!", align = "center"),
                        
                        # Prints the image output onto the app
                        
                        imageOutput("graphic"),
                        
                        h3("A Pricing Issue: Introduction"),
                        
                        p("In recent years, there has been outrage about rising pharmaceutical drug costs in the US for many of the most prescribed medications in the US. 
                        This outrage is not baseless. Take insulin for example. The average annual cost of insulin in the US per diabetes patient in 2012 was roughly $3000. 
                        This value has soared to nearly $6000 with a single vial of insulin costing as much as $350 today. A look at other pharmaceutical drugs will yield similar results."),
                        
                        p("These absurd prices have left many patients and their families struggling to pay for life-saving medications. Often times, patients skip taking doses throughout
                         the month to make their medications last longer. This of course has adverse effects on treatment effectiveness and the health of these patients. Morally, patients should not
                         be forced to skip on life-saving medication due to costs, and the fact that this is the reality suggests that there's a critical issue with how drugs are priced in the US."),
                        
                        p("When questioned about these rising prices and the ethical considerations behind their pricing, pharmaceutical companies often justify their prices by pointing to rising 
                        R&D costs, but skeptics believe the elevated prices are outliers compared to international peers and a means of raising profits at the expense of consumers. 
                        This project was inspired by this debate and seeks to examine these rising prices in the context of R&D costs to figure out if firms are indeed raising prices beyond R&D needs.")),
               
               # Sets up a new tab called "Drug Prices'
               
               tabPanel("Drug Prices",
                        mainPanel(
                            
                            # Creates two sub tabs, one for the most prescribed drugs and one for therapeutic classes
                            
                        tabsetPanel(
                            tabPanel("Most Prescribed", plotOutput("all_brand_plot")),
                            
                            # fluidRow and column allows for custom control of page layout 
                            
                            tabPanel("By Therapeutic Class", fluidRow(column(6, plotOutput("t_classes")),
                                     column(3, offset = 1, checkboxGroupInput(inputId = "select_class",
                                                                              
                                                                              # Allows for user to check therapeutic classes to filter by
                                                                              
                                                                              label = "Therapeutic Class",
                                                                              choices = therapeutic_list,
                                                                              selected = therapeutic_list)),
                                     column(2, sliderInput(inputId = "select_year",
                                                           
                                                           # Allows user to filter by year via a slider
                                                           
                                                           label = "Year Range",
                                                           min = 1996,
                                                           max = 2018,
                                                           value = c(1996, 2018)))))

                        )
                        ),
                        
                        sidebarPanel(gt_output(outputId = "drug_condition_treat")),
                        
                        
                            # Allows user to pick from a drop down with list of brand drugs
                        
                            selectInput(inputId = "selected_brand",
                                        label = "Brand:",
                                        choices = drugs_list,
                                        selected = drugs_list[5]),
                            
                            
                            fluidRow(
                                column(4,
                                       plotOutput("select_brand_plot")),
                                
                                column(4,
                                       gt_output(outputId = "Price_growth")),
                                
                                column(4,
                                       p("A look at the current 10 most prescribed drugs in the US reveals that their unit drug acquistion prices, 
                                         the prices at which pharmacies buy a unit of the drug, have been for the most part
                                         increasing at a rapid pace in recent years. Acquisition prices are a good proxy for the unit prices
                                         paid by US consumers, so this rise in acquisition prices is reflective of a similar rise in the price paid
                                         by consumers. This upward trend is not limited to just the popular drugs but also observed in many of the therapeutic
                                         classes of drugs, suggesting that it's a problem pertaining to the entire US pharmacuetical industry.")),
                            
                            fluidRow(
                                column(7,
                                       p("This concerning trend of increasing prices in the US is further compounded by the fact that US prices are consistently
                                         above that of international prices for the same drugs. A comparison of G7 countries which are countries comparable to the US additional
                                         highlights that pharmacuetical spending per capita is much higher in the US and has been explosively growing in recent years.
                                         All of these patterns together raising questions about why prices are increasing in the US and why prices in the 
                                         US are inflated relative to elsewhere to begin with. Luckily, a look at R&D cost trends (covered in the second tab) reveals some answers.")),
                                
                                column(8,
                                       plotOutput("Price_comparisons")),
                                
                                column(4,
                                       imageOutput("spending_cap")))
                            
                       ),
                        
                        
               ),
               
               # Sets up R&D costs tab and prints associated objects
               
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
                         revenue earned by the US premium be roughly equal to the R&D cost needs. The data, however, shows that the average revenue earned 
                         from the US premium by the top pharmacuetical firms is 63% higher than the costs associated with R&D."),
                        
                        p("The results suggest that pricing in the US is not fair and primarily driven by profit increasing motives. Firms are taking advantage of the US premium
                         to upmark their products well beyond R&D needs. Unfortunately, this grab for money translates to higher out-of-pocket costs for
                         patients who generally have inelastic demand for these drugs."),
                        
               ),
               
               # Sets up model tab and associated objects
               
               tabPanel("Model",
                        
                            fluidRow(
                                column(4,
                                       plotOutput("generic_brand")),
                                column(4,
                                       plotOutput("usa_int")),
                                column(4,
                                       plotOutput("alternatives"))),
                            fluidRow(
                                column(4,
                                   tableOutput("g_b_table")),
                                column(4,
                                   tableOutput("u_i_table")),
                                column(4,
                                   tableOutput("alt_table"))),
                            
                           mainPanel(
                         
                               tableOutput("regression")),
                             
                          
                        
                             p("This is a predictive model that estimates the average price per unit of a drug given the number of substitutes,
                              whether the drug is bought in or outside of the US, and whether it's a generic or brand drug.
                              The estimate of the intercept, $9.7, represents the predicted average cost per unit of a brand drug with no
                              other substitutes, bought outside of the US. The other estimates correspond to the associated increase or decrease in average
                              cost per unit of the given predictor. Interestingly, every additional substitute for a drug is associated with a decrease of $1.8
                              in average price. Additionally, generic drugs are estimated to be cheaper on average than brand names. The model also captures the higher
                              drug prices observed in the US relative to international, as reflected by the significant positive estimate associated with buying in the US."),
                            
                                
                             p("Given that 0 does not fall in the 95% confidence interval for the estimate of the generic and usa_price_per_unit
                               variables, we can conclude that they are significant predictors. 0 does fall in the confidence interval, so we can not 
                               conclude with this data that more alternatives is correlated with cheaper average cost. It was difficult to get data on prices,
                               so the dataset for this model is fairly small, hence the large uncertainty. All in all, we conclude from this model that the generic type is significantly cheaper than brand and that international bought drugs
                              are significantly cheaper to US bought ones")
                    
                       
               ),
               
               # Creates the about tab and the text in it
               
               tabPanel("About",
                        
                        h2("Data"),
                        
                        p("I used multiple sources to collect the data for this project. For the drug acqusition prices, I used the First Data Bank Health's dataset
                          to get prices for select drugs over some period. The comparision of US to international prices as well as US premium revenue to R&D costs
                          came from data provided by Health Affairs. The data for the model was sourced from PharmacyChecker and GoodRx which are sites that find the lowest
                          costs for a given drug. Lastly, data released by the OECD and Deloitte was utilized to get info on spending per capita and R&D costs, respectively"),
                        
                        h2("Motivation"),
                        
                        p("Health care and its associated costs have been hot topics in recent years. There have been multiple complains that necessary, life
                        saving drugs, like insulin, have had their prices explode to absurd levels, placing an increasing burden on patients. I wanted to use this project to explore these complaints and see if
                        firms have been charging fair prices from their drugs."),
                        
                        h2("Contact"),
                        
                        p("Hello! My name is Yishak Ali, and I'm a junior in Leverett House studying Neuroscience and Economics. This was my final project
                          for Gov 50: Data. If you're interested in contacting me, my email is ysali@college.harvard.edu. The source code for this app can be found here: https://github.com/YishakAli/Phamaceutical-Drug-Trends"))
               
    )
)

server <- function(input, output, session) {
    
    # Renders the first plot with all the brand drugs and their acquisition prices over the years
    
    output$all_brand_plot <- renderPlot({
        updated_data %>%
            filter(!ndc_description %in% c("VICODIN HP 10-300 MG TABLET", "FORTAMET ER 1,000 MG TABLET")) %>% 
            ggplot(aes(x = effective_date, y = mean_price, 
                       color = ndc_description)) +
            geom_line(size = .8) +
            geom_point() +
            stat_summary(fun.y=mean, aes(group = 1), geom="line", colour="darkblue", size = 1.2, lty = 2) +
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
    
    # Renders the table that gives descriptions of the conditions the listed drugs treat.
    
    output$drug_condition_treat <- render_gt({
        tibble(Brand = c("Amoxil", "Cozaar", "Glucophage",
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
    
    # Renders the table with the percent change and average rate of change for each brand drug
    
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
            
            # Changes cell fill color to reflect the size and direction of percent and average rate of change
            
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
    
    # Renders the barplot that compares the US and international prices of drugs by plotting
    # international prices as a percentage of US prices.
    
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
    
    # Renders the individual brand plot based on user input
    
    output$select_brand_plot <- renderPlot({
        updated_data %>% 
            
            # Allows the user to see only the brand/plot they select from the drop down
            # by filtering to just the desired brand.
            
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
    
    # Renders the barplot that displays R&D costs over the years
    
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
    
    # Renders the barplot that shows the average revenue the 15 top firms earn from the US premium
    
    output$Excess_profits <- renderPlot({
          us_premium %>%
                ggplot(aes(x =fct_reorder(company, revenues_from_us_prmium_as_percent_of_global_research_and_development),
                           y = revenues_from_us_prmium_as_percent_of_global_research_and_development,
                           fill = Company)) +
                    geom_col() +
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
        
    })
    
    # Renders therapeutic classes plot under the by therapeutic class sub tab
    
    output$t_classes <- renderPlot({
        therapeutic_classes_final %>%
            drop_na() %>% 
            rename(`Therapeutic Class` = therapeutic_class) %>% 
            filter(`Therapeutic Class` %in% input$select_class,
                    year %in% input$select_year[1]:input$select_year[2]) %>%
            group_by(`Therapeutic Class`, year) %>% 
            summarize(expend_per_purchase = log((expenditure*1000000)/(1000*purchases))) %>% 
            ggplot(aes(x = year, y = expend_per_purchase, color = `Therapeutic Class`)) +
            geom_line() +
            stat_summary(fun.y=mean, aes(group = 1), geom="line", colour="darkblue", size = 1.2, lty = 3) +
            labs(title = "Log Expenditure per Purchase by Therapeutic Class",
                 x = "Year",
                 y = "Log Expenditure per Purchase",
                 subtitle = "Dashed line: Selected Classes Average Trend") +
            theme_clean() +
            theme(legend.title = element_text(size = 6), 
                  legend.text = element_text(size = 6),
                  legend.position = "right") 
        
    })
    
    # Renders gif for pharma spending per capita of G7 countries over the years
    
    output$spending_cap <- renderImage({
        outfile <- tempfile(fileext='.gif')
        list(src = "outfile.gif",
             contentType = 'image/gif',
              width = 390,
              height = 400,
              deleteFile = T)
        
    
    })
    
    # Renders regression table seen under the model tab
    
    output$regression <- render_gt({
        tbl_regression(fit_model, intercept = T) %>% 
            as_gt() %>% 
            tab_header(title = "Regression of Drug Prices" ,
                       subtitle = "The Effect of Various Predictors on Prices") %>% 
            tab_source_note(md("Source: PharmacyChecker & Good Rx"))
        
        
    })
    
    # Renders posterior distributions for US and international bought drugs
    
    output$generic_brand <- renderPlot({
        obj <- tibble(type = c("brand", "generic"),
                      num_subs = 1, location = "usa_price_per_unit")
       
        
        predict %>% 
            pivot_longer(cols = brand:generic, names_to = "type",
                         values_to = "cost") %>% 
            ggplot(aes(cost, fill = type)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),bins = 100) +
            geom_vline(xintercept = brand_mean, lty = 2) +
            geom_vline(xintercept = generic_mean, lty = 2,
                       color = "purple") +
            labs(title = "Predicted Average Cost for Generic vs Brand \n",
                 x = "Average Cost",
                 y = "Probability") +
            theme_minimal() +
            scale_fill_manual(values = c("coral", "green"), 
                              labels = c("Generic", "Brand"),
                              name = "Type")
        
    })
    
    # Renders posterior distributions for brand and generic drugs
    
    output$usa_int <- renderPlot({
        obj_2 <- tibble(type = "brand",
                        num_subs = 1, location = c("usa_price_per_unit", "int_price_per_unit"))
        
        predict_2 %>% 
            pivot_longer(cols = usa:international, names_to = "location",
                         values_to = "cost") %>% 
            ggplot(aes(cost, fill = location)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),bins = 100) +
            geom_vline(xintercept = usa_mean, lty = 2,
                       color = "black") + 
            geom_vline(xintercept = int_mean, lty = 2,
                       color = "red") + 
            labs(title = "Predicted Average Cost for International vs US Bought \n",
                 x = "Average Cost", y = "Probability") +
            theme_minimal() +
            scale_fill_manual(values = c("pink", "dodgerblue"), 
                              labels = c("International", "US"),
                              name = "Location")
        
    })
    
    # Renders posterior distribution for varying number of alternatives
    
    output$alternatives <- renderPlot({
        obj_3 <- tibble(type = "brand",
                        num_subs = c(1,2,3), location = "usa_price_per_unit")
        
        predict_3 %>% 
            pivot_longer(cols = c(one, two, three), names_to = "Subs",
                         values_to = "cost") %>% 
            ggplot(aes(cost, fill = Subs)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),bins = 100) +
            geom_vline(xintercept = one_mean, lty = 1,
                       color = "green") + 
            geom_vline(xintercept = two_mean, lty = 2,
                       color = "purple") +
            geom_vline(xintercept = three_mean, lty = 3,
                       color = "brown") +
            labs(title = "Predicted Average Costs for Varying Number of Drug Alternatives \n" ,
                 x = "Average Cost", y = "Probability") +
            theme_minimal() +
            scale_fill_manual(values = c("coral", "green", "blue"), 
                              labels = c("One", "Two", "Three"),
                              name = "Alternative")
    })
    
    # Following 3 segments render the 3 tables seen on the model tab
    
    output$g_b_table <- renderTable({
        
        tibble(Type = c("Generic", "Brand"), `Average Cost ($)` = c(generic_mean, brand_mean))
     
    })

    output$u_i_table <- renderTable({
    
        tibble(Location = c("International", "US"), `Average Cost ($)` = c(int_mean, usa_mean))


    })

    output$alt_table <- renderTable({
    
        tibble(Alternatives = 1:3, `Average Cost ($)` = c(one_mean, two_mean, three_mean))


    })
    
    # Render the image on the home tab
    
    output$graphic <- renderImage({
        
        list(src = "drugcosts-edt-superJumbo.jpg",
             contentType = 'image/jpeg',
             width = 1350,
             height = 400,
             deleteFile = T)
        
        
    })

}


shinyApp(ui, server)
