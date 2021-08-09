##### BRAND LINEUP R SHINY APPLICATION #####
##### CODE WRITTEN AND MAINTAINED BY JOSEPH MENA #####

library(shiny)
library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(shinythemes)
library(shinycssloaders)
library(ggplot2)
library(tools)
library(scales)
library(lubridate)
library(DT)



ui <- navbarPage(theme=shinytheme("sandstone"), strong("Brand Lineup"),
                 
                 
                 tabPanel("Search 1 Brand",
                          
                          # Application title
                          titlePanel(strong("Exploring a Brand's Online Audience")),
                          
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              helpText("Discover the reach, popularity, and demographics of topics online, such as brands, to know which audiences to target")
                              ,
                              
                              helpText("If an entry has more than one meaning, like Dawn or Tide, then try making your search a little more specific, like 'Tide Detergent'")
                              ,
                              helpText("Simply enter a brand, company, or product category and click search!")
                              ,

                              
                              textInput("brand1", "Enter Brand 1:", ""),

                              selectInput("dateselect", "Select time window:", choices=c("Past 30 days" = "1%20month%20ago", "Past 3 months" = "3%20months%20ago", "Past 6 months" = "6%20months%20ago", "Past Year" = "1%20year%20ago", "Past 2 years" = "2%20years%20ago", "Past 3 years" = "3%20years%20ago"), selected = ("Past Year" = "1%20year%20ago")),
                              
                              actionButton("button", "Search")
 
                              
                            ),
                            
                            mainPanel(
                              
                              conditionalPanel( 
                                condition = "input.button > 0", radioButtons(inputId = "radiobuttons", "What kind of data would you like?:", c("Size & Sentiment", "Demographics", "Combination"), selected = "Size & Sentiment", inline = TRUE), br(), strong(helpText("- click column title to sort by that field -"))
                              ),

                              conditionalPanel(
                                condition = "input.radiobuttons == 'Size & Sentiment'", dataTableOutput("table1_1")), #%>% withSpinner(color="#37384b")
                              conditionalPanel(
                                condition = "input.radiobuttons == 'Demographics'", dataTableOutput("table2_1")), #%>% withSpinner(color="#37384b")
                              conditionalPanel(
                                condition = "input.radiobuttons == 'Combination'", dataTableOutput("table3_1")), #%>% withSpinner(color="#37384b")
                              
                              
                              conditionalPanel(
                                condition = "input.button > 0 && input.radiobuttons == 'Size & Sentiment'", br(), strong(helpText("Download CSV file of your selected data below and open with Excel:")), downloadButton("downloadData1_1", "Download CSV")), #%>% withSpinner(color="#37384b")
                              conditionalPanel(
                                condition = "input.button > 0 && input.radiobuttons == 'Demographics'", br(), strong(helpText("Download CSV file of your selected data below and open with Excel:")), downloadButton("downloadData2_1", "Download CSV")), #%>% withSpinner(color="#37384b")
                              conditionalPanel(
                                condition = "input.button > 0 && input.radiobuttons == 'Combination'", br(), strong(helpText("Download CSV file of your selected data below and open with Excel:")), downloadButton("downloadData3_1", "Download CSV"))
                              
     
                            )
                            
                          )  
                          
                               
                 ),
                 
                 
                 tabPanel("Search 2 Brands",
                          
                          # Application title
                          titlePanel(strong("Comparing the Online Audiences of 2 Brands")),
                          
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              helpText("Discover the reach, popularity, and demographics of topics online, such as brands, to know which audiences to target")
                              ,
                              
                              helpText("If an entry has more than one meaning, like Dawn or Tide, then try making your search a little more specific, like 'Tide Detergent'")
                              ,
                              helpText("Simply enter 2 brands, companies, or product categories and click search!")
                              ,

                              
                              textInput("brand1_2", "Enter Brand 1:", ""),
                              textInput("brand2_2", "Enter Brand 2:", ""),
                            
                              selectInput("dateselect2", "Select time window:", choices=c("Past 30 days" = "1%20month%20ago", "Past 3 months" = "3%20months%20ago", "Past 6 months" = "6%20months%20ago", "Past Year" = "1%20year%20ago", "Past 2 years" = "2%20years%20ago", "Past 3 years" = "3%20years%20ago"), selected = ("Past Year" = "1%20year%20ago")),
                              
                              actionButton("button2", "Search")
                             
                
                            ),
                            
                            mainPanel(
                              
                              conditionalPanel( 
                                condition = "input.button2 > 0", radioButtons(inputId = "radiobuttons2", "What kind of data would you like?:", c("Size & Sentiment", "Demographics", "Combination"), selected = "Size & Sentiment", inline = TRUE), br(), strong(helpText("- click column title to sort by that field -"))
                              ),
                              
                              conditionalPanel(
                                condition = "input.radiobuttons2 == 'Size & Sentiment'", dataTableOutput("table1_2")), #%>% withSpinner(color="#37384b")
                              conditionalPanel(
                                condition = "input.radiobuttons2 == 'Demographics'", dataTableOutput("table2_2")), #%>% withSpinner(color="#37384b")
                              conditionalPanel(
                                condition = "input.radiobuttons2 == 'Combination'", dataTableOutput("table3_2")),
                              
                              br(),
                              
                              conditionalPanel(
                                condition = "input.button2 > 0 && input.radiobuttons2 == 'Size & Sentiment'", strong(helpText("Download CSV file of your selected data below and open with Excel:")), downloadButton("downloadData1_2", "Download CSV")), #%>% withSpinner(color="#37384b")
                              conditionalPanel(
                                condition = "input.button2 > 0 && input.radiobuttons2 == 'Demographics'", strong(helpText("Download CSV file of your selected data below and open with Excel:")), downloadButton("downloadData2_2", "Download CSV")), #%>% withSpinner(color="#37384b")
                              conditionalPanel(
                                condition = "input.button2 > 0 && input.radiobuttons2 == 'Combination'", strong(helpText("Download CSV file of your selected data below and open with Excel:")), downloadButton("downloadData3_2", "Download CSV"))
                              
                              
                            )
                            
                          )
                          
                                  
                 ),
                 
                 
                 tabPanel("Search 5 Brands",
                
                
                
                # Application title
                titlePanel(strong("Comparing the Online Audiences of 5 Brands")),
                
                
                sidebarLayout(
                  sidebarPanel(
                    
                    helpText("Discover the reach, popularity, and demographics of topics online, such as brands, to know which audiences to target")
                    ,
                    
                    helpText("If an entry has more than one meaning, like Dawn or Tide, then try making your search a little more specific, like 'Tide Detergent'")
                    ,
                    helpText("Simply enter 5 brands, companies, or product categories and click search!")
                    ,

                    textInput("brand1_3", "Enter Brand 1:", ""),
                    textInput("brand2_3", "Enter Brand 2:", ""),
                    textInput("brand3_3", "Enter Brand 3:", ""),
                    textInput("brand4_3", "Enter Brand 4:", ""),
                    textInput("brand5_3", "Enter Brand 5:", ""),
                    
                    selectInput("dateselect3", "Select time window:", choices=c("Past 30 days" = "1%20month%20ago", "Past 3 months" = "3%20months%20ago", "Past 6 months" = "6%20months%20ago", "Past Year" = "1%20year%20ago", "Past 2 years" = "2%20years%20ago", "Past 3 years" = "3%20years%20ago"), selected = ("Past Year" = "1%20year%20ago")),
                    
                    actionButton("button3", "Search")
                   
  
                  ),
                  
                  mainPanel(
                    
                    conditionalPanel( 
                      condition = "input.button3 > 0", radioButtons(inputId = "radiobuttons3", "What kind of data would you like?:", c("Size & Sentiment", "Demographics", "Combination"), selected = "Size & Sentiment", inline = TRUE), br(), strong(helpText("- click column title to sort by that field -"))
                    ),
                    
                    conditionalPanel(
                      condition = "input.radiobuttons3 == 'Size & Sentiment'", dataTableOutput("table1_3")), #%>% withSpinner(color="#37384b")
                    conditionalPanel(
                      condition = "input.radiobuttons3 == 'Demographics'", dataTableOutput("table2_3")), #%>% withSpinner(color="#37384b")
                    conditionalPanel(
                      condition = "input.radiobuttons3 == 'Combination'", dataTableOutput("table3_3")),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "input.button3 > 0 && input.radiobuttons3 == 'Size & Sentiment'", strong(helpText("Download CSV file of your selected data below and open with Excel:")), downloadButton("downloadData1_3", "Download CSV")), #%>% withSpinner(color="#37384b")
                    conditionalPanel(
                      condition = "input.button3 > 0 && input.radiobuttons3 == 'Demographics'", strong(helpText("Download CSV file of your selected data below and open with Excel:")), downloadButton("downloadData2_3", "Download CSV")), #%>% withSpinner(color="#37384b")
                    conditionalPanel(
                      condition = "input.button3 > 0 && input.radiobuttons3 == 'Combination'", strong(helpText("Download CSV file of your selected data below and open with Excel:")), downloadButton("downloadData3_3", "Download CSV"))
                    
  
                    
                  )
                  
                )
                
)  

)  



server <- function(input,output,session)  {
  
  observeEvent(input$button,{
    
    showModal(modalDialog("Loading data...This should take less than 1 minute...", footer=NULL))
    
    
    #data derived from API call
    
    #components of API request to concatenate, including authentication and parameters
    base <- "https://atlas.infegy.com/api/v2/"
    endpoint1 <- "volume"
    endpoint2 <- "sentiment"
    endpoint3 <- "gender"
    endpoint4 <- "demographics"
    api_key <- ### THIRD-PARTY PRIVATE KEY ###
    
    
    query1 <- reactive(gsub(" ","%20", input$brand1))

    start_date <- reactive(input$dateselect)
    end_date <- "end_date=now"
    countries <- "countries=US"
    limit <- "limit=1000"
    
    #full request
    call1_vol <- paste0(base,endpoint1,"?",api_key,"&","query=",query1(),"&","start_date=",start_date(),"&",end_date,"&",countries,"&",limit)
   
    call1_sent <- paste0(base,endpoint2,"?",api_key,"&","query=",query1(),"&","start_date=",start_date(),"&",end_date,"&",countries,"&",limit)
   
    call1_gend <- paste0(base,endpoint3,"?",api_key,"&","query=",query1(),"&","start_date=",start_date(),"&",end_date,"&",countries,"&",limit)
   
    call1_demogr <- paste0(base,endpoint4,"?",api_key,"&","query=",query1(),"&","start_date=",start_date(),"&",end_date,"&",countries,"&",limit)
  

    
    #volume calls
    
    
    #volume call 1
    get_query_volume_1 <- GET(call1_vol)
    get_query_volume_text_1 <- httr::content(get_query_volume_1, "text")
    get_query_volume_json_1 <- fromJSON(get_query_volume_text_1, flatten = TRUE)
    get_query_volume_df_1 <- as.data.frame(get_query_volume_json_1$output)
    #get_query_volume_df_1 <- get_query_volume_df_1 %>% mutate(Brand=paste(tools::toTitleCase(query1)))
    
   
    #binding volume calls
    all_queries_volume_df <- get_query_volume_df_1
    
    
    
    #sentiment calls#
    
    #sentiment call 1
    get_query_sentiment_1 <- GET(call1_sent)
    get_query_sentiment_text_1 <- httr::content(get_query_sentiment_1, "text")
    get_query_sentiment_json_1 <- fromJSON(get_query_sentiment_text_1, flatten = TRUE)
    get_query_sentiment_df_1 <- as.data.frame(get_query_sentiment_json_1$output)

   
    #binding sentiment calls
    all_queries_sentiment_df <- get_query_sentiment_df_1
    
    
    #gender calls#
    
    #gender call 1
    get_query_gender_1 <- GET(call1_gend)
    get_query_gender_text_1 <- httr::content(get_query_gender_1, "text")
    get_query_gender_json_1 <- fromJSON(get_query_gender_text_1, flatten = TRUE)
    get_query_gender_df_1 <- as.data.frame(get_query_gender_json_1$output)

    
    #binding gender calls
    all_queries_gender_df <- get_query_gender_df_1
    
    
    #demographics calls#
    
    #demographics call 1
    get_query_demographics_1 <- GET(call1_demogr)
    get_query_demographics_text_1 <- httr::content(get_query_demographics_1, "text")
    get_query_demographics_json_1 <- fromJSON(get_query_demographics_text_1, flatten = TRUE)
    get_query_demographics_df_1 <- as.data.frame(get_query_demographics_json_1$output)
  
    
    #binding demographics calls
    all_queries_demographics_df <- get_query_demographics_df_1
    
    
    library(tidyverse)
    library(magrittr)
    
    
    all_queries_volume_cleaned_df <- all_queries_volume_df %>% 
      summarise(Total_Posts = sum(posts_universe), Average_Daily_Posts = mean(posts_universe)) %>%
      mutate(Brand=paste(tools::toTitleCase(input$brand1))) %>%
      select(Brand, Total_Posts, Average_Daily_Posts)

    
    #sentiment cleaning
    
    all_queries_sentiment_cleaned_df <- all_queries_sentiment_df %>%
      summarise(sum_positive_documents = sum(positive_documents), sum_neutral_documents = sum(neutral_documents), sum_documents = sum(documents)) %>%
      mutate(half_sum_neutral_documents = (sum_neutral_documents/2)) %>%
      mutate(Positive_Sentiment = (sum_positive_documents + half_sum_neutral_documents)/sum_documents) %>%
      mutate(Brand=paste(tools::toTitleCase(input$brand1))) %>%
      select(Brand, Positive_Sentiment)


    all_queries_sentiment_cleaned_df$Positive_Sentiment <- percent(all_queries_sentiment_cleaned_df$Positive_Sentiment)                          
    
    #gender cleaning
    all_queries_gender_df <- all_queries_gender_df %>% dplyr::rename(male_count = male.count, female_count = female.count) 
    
    all_queries_gender_cleaned_df <- all_queries_gender_df %>%
      summarise(sum_male_count = sum(male_count), sum_female_count = sum(female_count)) %>%
      mutate(Brand=paste(tools::toTitleCase(input$brand1))) %>%
      mutate(Male_Voice = sum_male_count/(sum_male_count + sum_female_count)) %>%
      mutate(Female_Voice = sum_female_count/(sum_male_count + sum_female_count)) %>%
      select(Brand, Male_Voice, Female_Voice)

 
    all_queries_gender_cleaned_df$Male_Voice <- percent(all_queries_gender_cleaned_df$Male_Voice)                          
    all_queries_gender_cleaned_df$Female_Voice <- percent(all_queries_gender_cleaned_df$Female_Voice)                          
    
    
    #demographics cleaning
    
    all_queries_demographics_df <- all_queries_demographics_df %>% dplyr::rename(query_median_household_income = query.median_household_income, query_income_0k_25k = query.income_0k_25k, query_education_highschool = query.education_highschool, query_education_college = query.education_college) 
    
    
    all_queries_demographics_cleaned_df <- all_queries_demographics_df %>%
      mutate(Avg_Household_Income = query_median_household_income, Lower_Income_Bracket = (query_income_0k_25k/100), High_School_Graduate = (query_education_highschool/100), College_Graduate = (query_education_college/100)) %>%
      mutate(Brand=paste(tools::toTitleCase(input$brand1))) %>%
      select(Brand, Avg_Household_Income, Lower_Income_Bracket, High_School_Graduate, College_Graduate)
    
    
    all_queries_demographics_cleaned_df$Lower_Income_Bracket <- percent(all_queries_demographics_cleaned_df$Lower_Income_Bracket)                          
    all_queries_demographics_cleaned_df$High_School_Graduate <- percent(all_queries_demographics_cleaned_df$High_School_Graduate)                          
    all_queries_demographics_cleaned_df$College_Graduate <- percent(all_queries_demographics_cleaned_df$College_Graduate)                          
    
    
    #joining all info into one dataframe
    all_queries_all_endpoints_df <- all_queries_volume_cleaned_df %>%
      left_join(all_queries_sentiment_cleaned_df, by="Brand") %>%
      left_join(all_queries_gender_cleaned_df, by="Brand") %>%
      left_join(all_queries_demographics_cleaned_df, by="Brand")
    
    all_queries_all_endpoints_df$Total_Posts <- formatC(all_queries_all_endpoints_df$Total_Posts, format="d", big.mark=",")   
    all_queries_all_endpoints_df$Average_Daily_Posts <- formatC(all_queries_all_endpoints_df$Average_Daily_Posts, format="d", big.mark=",")   
    all_queries_all_endpoints_df$Avg_Household_Income <- formatC(all_queries_all_endpoints_df$Avg_Household_Income, format="d", big.mark=",")   
    
    all_queries_all_endpoints_df$Avg_Household_Income <- paste('$',formatC(all_queries_all_endpoints_df$Avg_Household_Income, big.mark=',', format = 'f'))
    
    all_queries_all_endpoints_df$Male_Voice <- formatC(all_queries_all_endpoints_df$Male_Voice, digits = 1, format = "f")
    all_queries_all_endpoints_df$Female_Voice <- formatC(all_queries_all_endpoints_df$Female_Voice, digits = 1, format = "f")
    all_queries_all_endpoints_df$Lower_Income_Bracket <- formatC(all_queries_all_endpoints_df$Lower_Income_Bracket, digits = 1, format = "f")
    all_queries_all_endpoints_df$High_School_Graduate <- formatC(all_queries_all_endpoints_df$High_School_Graduate, digits = 1, format = "f")
    all_queries_all_endpoints_df$College_Graduate <- formatC(all_queries_all_endpoints_df$College_Graduate, digits = 1, format = "f")
    
    
    all_queries_all_endpoints_df2 <- all_queries_all_endpoints_df
    
    all_queries_all_endpoints_df <- all_queries_all_endpoints_df %>%
                                      dplyr::rename('Total Posts' = Total_Posts, 'Average Daily Posts'= Average_Daily_Posts, 'Positive Sentiment' = Positive_Sentiment, 'Male Voice' = Male_Voice, 'Female Voice' = Female_Voice, 'Avg Household Income' = Avg_Household_Income, 'Lower Income Bracket'= Lower_Income_Bracket, 'High School Graduate' = High_School_Graduate, 'College Graduate' = College_Graduate)
    

    size_and_sentiment_table_df <- all_queries_all_endpoints_df2 %>%
                                    select(Brand, Total_Posts, Average_Daily_Posts, Positive_Sentiment)


    demographics_table_df <- all_queries_all_endpoints_df2 %>%
                              select(Brand, Male_Voice, Female_Voice, Avg_Household_Income, Lower_Income_Bracket, High_School_Graduate, College_Graduate)


    combination_table_df <- all_queries_all_endpoints_df2 %>%
                              select(Brand, Total_Posts, Average_Daily_Posts, Positive_Sentiment, Male_Voice, Female_Voice, Avg_Household_Income, Lower_Income_Bracket, High_School_Graduate, College_Graduate)

    
    output$table1_1 <- renderDataTable(
      all_queries_all_endpoints_df %>% select(Brand, 'Total Posts', 'Average Daily Posts', 'Positive Sentiment'), options = list(pageLength = 15, "bPaginate" = FALSE, "bFilter" = FALSE, "bInfo" = FALSE)
    )
    
    
    output$table2_1 <- renderDataTable(
      all_queries_all_endpoints_df %>% select(Brand, 'Male Voice', 'Female Voice', 'Avg Household Income', 'Lower Income Bracket', 'High School Graduate', 'College Graduate'), options = list(pageLength = 15, "bPaginate" = FALSE, "bFilter" = FALSE, "bInfo" = FALSE)
    )
    
    
    output$table3_1 <- renderDataTable(
      all_queries_all_endpoints_df %>% select(Brand, 'Total Posts', 'Average Daily Posts', 'Positive Sentiment', 'Male Voice', 'Female Voice', 'Avg Household Income', 'Lower Income Bracket', 'High School Graduate', 'College Graduate'), options = list(pageLength = 15, "bPaginate" = FALSE, "bFilter" = FALSE, "bInfo" = FALSE)
    )
    
    
    output$downloadData1_1 <- downloadHandler(
      filename = function() {
        paste(input$brand1, "_online_size_&_sentiment", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(size_and_sentiment_table_df, file, row.names = FALSE)
      }
    )
    
    output$downloadData2_1 <- downloadHandler(
      filename = function() {
        paste(input$brand1, "_online_demographics", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(demographics_table_df, file, row.names = FALSE)
      }
    )
    
    output$downloadData3_1 <- downloadHandler(
      filename = function() {
        paste(input$brand1, "_all_online_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(combination_table_df, file, row.names = FALSE)
      }
    )
    
    
    
    removeModal()

    
     
  })
    
  
  
  observeEvent(input$button2,{
   
    showModal(modalDialog("Loading data...This should take less than 1 minute...", footer=NULL))
    
    
    #data derived from API call
    
    #components of API request to concatenate, including authentication and parameters
    base <- "https://atlas.infegy.com/api/v2/"
    endpoint1 <- "volume"
    endpoint2 <- "sentiment"
    endpoint3 <- "gender"
    endpoint4 <- "demographics"
    api_key <- ### THIRD-PARTY PRIVATE KEY ###
    
    
    query1_2 <- reactive(gsub(" ","%20", input$brand1_2))
    query2_2 <- reactive(gsub(" ","%20", input$brand2_2))
    
    start_date_2 <- reactive(input$dateselect2)
    
    end_date <- "end_date=now"
    countries <- "countries=US"
    limit <- "limit=1000"
    
    #full request
    call1_vol <- paste0(base,endpoint1,"?",api_key,"&","query=",query1_2(),"&","start_date=",start_date_2(),"&",end_date,"&",countries,"&",limit)
    call2_vol <- paste0(base,endpoint1,"?",api_key,"&","query=",query2_2(),"&","start_date=",start_date_2(),"&",end_date,"&",countries,"&",limit)
   
    call1_sent <- paste0(base,endpoint2,"?",api_key,"&","query=",query1_2(),"&","start_date=",start_date_2(),"&",end_date,"&",countries,"&",limit)
    call2_sent <- paste0(base,endpoint2,"?",api_key,"&","query=",query2_2(),"&","start_date=",start_date_2(),"&",end_date,"&",countries,"&",limit)
   
    call1_gend <- paste0(base,endpoint3,"?",api_key,"&","query=",query1_2(),"&","start_date=",start_date_2(),"&",end_date,"&",countries,"&",limit)
    call2_gend <- paste0(base,endpoint3,"?",api_key,"&","query=",query2_2(),"&","start_date=",start_date_2(),"&",end_date,"&",countries,"&",limit)
   
    call1_demogr <- paste0(base,endpoint4,"?",api_key,"&","query=",query1_2(),"&","start_date=",start_date_2(),"&",end_date,"&",countries,"&",limit)
    call2_demogr <- paste0(base,endpoint4,"?",api_key,"&","query=",query2_2(),"&","start_date=",start_date_2(),"&",end_date,"&",countries,"&",limit)
   
    
    #volume calls
    
    #volume call 1
    get_query_volume_1 <- GET(call1_vol)
    get_query_volume_text_1 <- httr::content(get_query_volume_1, "text")
    get_query_volume_json_1 <- fromJSON(get_query_volume_text_1, flatten = TRUE)
    get_query_volume_df_1 <- as.data.frame(get_query_volume_json_1$output)
    get_query_volume_df_1 <- get_query_volume_df_1 %>% mutate(Brand=paste(tools::toTitleCase(input$brand1_2)))
    
    #volume call 2
    get_query_volume_2 <- GET(call2_vol)
    get_query_volume_text_2 <- httr::content(get_query_volume_2, "text")
    get_query_volume_json_2 <- fromJSON(get_query_volume_text_2, flatten = TRUE)
    get_query_volume_df_2 <- as.data.frame(get_query_volume_json_2$output)
    get_query_volume_df_2 <- get_query_volume_df_2 %>% mutate(Brand=paste(tools::toTitleCase(input$brand2_2)))
    
    #binding volume calls
    all_queries_volume_df <- rbind(get_query_volume_df_1, get_query_volume_df_2)
    
    
    #sentiment calls#
    
    #sentiment call 1
    get_query_sentiment_1 <- GET(call1_sent)
    get_query_sentiment_text_1 <- httr::content(get_query_sentiment_1, "text")
    get_query_sentiment_json_1 <- fromJSON(get_query_sentiment_text_1, flatten = TRUE)
    get_query_sentiment_df_1 <- as.data.frame(get_query_sentiment_json_1$output)
    get_query_sentiment_df_1 <- get_query_sentiment_df_1 %>% mutate(Brand=paste(tools::toTitleCase(input$brand1_2)))
    
    #sentiment call 2
    get_query_sentiment_2 <- GET(call2_sent)
    get_query_sentiment_text_2 <- httr::content(get_query_sentiment_2, "text")
    get_query_sentiment_json_2 <- fromJSON(get_query_sentiment_text_2, flatten = TRUE)
    get_query_sentiment_df_2 <- as.data.frame(get_query_sentiment_json_2$output)
    get_query_sentiment_df_2 <- get_query_sentiment_df_2 %>% mutate(Brand=paste(tools::toTitleCase(input$brand2_2)))
    
    #binding sentiment calls
    all_queries_sentiment_df <- rbind(get_query_sentiment_df_1, get_query_sentiment_df_2)
    
    
    #gender calls#
    
    #gender call 1
    get_query_gender_1 <- GET(call1_gend)
    get_query_gender_text_1 <- httr::content(get_query_gender_1, "text")
    get_query_gender_json_1 <- fromJSON(get_query_gender_text_1, flatten = TRUE)
    get_query_gender_df_1 <- as.data.frame(get_query_gender_json_1$output)
    get_query_gender_df_1 <- get_query_gender_df_1 %>% mutate(Brand=paste(tools::toTitleCase(input$brand1_2)))
    
    #gender call 2
    get_query_gender_2 <- GET(call2_gend)
    get_query_gender_text_2 <- httr::content(get_query_gender_2, "text")
    get_query_gender_json_2 <- fromJSON(get_query_gender_text_2, flatten = TRUE)
    get_query_gender_df_2 <- as.data.frame(get_query_gender_json_2$output)
    get_query_gender_df_2 <- get_query_gender_df_2 %>% mutate(Brand=paste(tools::toTitleCase(input$brand2_2)))
    
    #binding gender calls
    all_queries_gender_df <- rbind(get_query_gender_df_1, get_query_gender_df_2)
    
    
    #demographics calls#
    
    #demographics call 1
    get_query_demographics_1 <- GET(call1_demogr)
    get_query_demographics_text_1 <- httr::content(get_query_demographics_1, "text")
    get_query_demographics_json_1 <- fromJSON(get_query_demographics_text_1, flatten = TRUE)
    get_query_demographics_df_1 <- as.data.frame(get_query_demographics_json_1$output)
    get_query_demographics_df_1 <- get_query_demographics_df_1 %>% mutate(Brand=paste(tools::toTitleCase(input$brand1_2)))
    
    #demographics call 2
    get_query_demographics_2 <- GET(call2_demogr)
    get_query_demographics_text_2 <- httr::content(get_query_demographics_2, "text")
    get_query_demographics_json_2 <- fromJSON(get_query_demographics_text_2, flatten = TRUE)
    get_query_demographics_df_2 <- as.data.frame(get_query_demographics_json_2$output)
    get_query_demographics_df_2 <- get_query_demographics_df_2 %>% mutate(Brand=paste(tools::toTitleCase(input$brand2_2)))
    
    
    #binding demographics calls
    all_queries_demographics_df <- rbind(get_query_demographics_df_1, get_query_demographics_df_2)
    
    
    library(tidyverse)
    library(magrittr)
    
    
    #volume cleaning
    

    library(purrr)
    library(psych)
    library(tidyr)

  
      
    all_queries_volume_cleaned_df_sum_posts = aggregate(all_queries_volume_df$posts_universe, list(all_queries_volume_df$Brand), sum)
    all_queries_volume_cleaned_df_avg_posts = aggregate(all_queries_volume_df$posts_universe, list(all_queries_volume_df$Brand), mean)
   
    all_queries_volume_cleaned_df <- all_queries_volume_cleaned_df_sum_posts %>%
                                    left_join(all_queries_volume_cleaned_df_avg_posts, by="Group.1")
      
    all_queries_volume_cleaned_df <- all_queries_volume_cleaned_df %>%
                                    dplyr::rename(Brand = Group.1, Total_Posts = x.x, Average_Daily_Posts = x.y)
      
    
    
    #sentiment cleaning
    
    
    all_queries_sentiment_df_sum_pos = aggregate(all_queries_sentiment_df$positive_documents, list(all_queries_sentiment_df$Brand), sum)
    all_queries_sentiment_df_sum_neutr = aggregate(all_queries_sentiment_df$neutral_documents, list(all_queries_sentiment_df$Brand), sum)
    all_queries_sentiment_df_sum_all = aggregate(all_queries_sentiment_df$documents, list(all_queries_sentiment_df$Brand), sum)
    
    
    all_queries_sentiment_cleaned_df <- all_queries_sentiment_df_sum_pos %>%
      left_join(all_queries_sentiment_df_sum_neutr, by="Group.1") %>%
      left_join(all_queries_sentiment_df_sum_all, by="Group.1")
    
      
    all_queries_sentiment_cleaned_df <- all_queries_sentiment_cleaned_df %>%
      dplyr::rename(Brand = Group.1, sum_positive_documents = x.x, sum_neutral_documents = x.y, sum_documents = x)
    
    
    all_queries_sentiment_cleaned_df <- all_queries_sentiment_cleaned_df %>%
      mutate(half_sum_neutral_documents = (sum_neutral_documents/2)) %>%
      mutate(Positive_Sentiment = (sum_positive_documents + half_sum_neutral_documents)/sum_documents) %>%
      select(Brand, Positive_Sentiment)
    
    
    all_queries_sentiment_cleaned_df$Positive_Sentiment <- percent(all_queries_sentiment_cleaned_df$Positive_Sentiment)                          
    
    
    #gender cleaning
    
    
    all_queries_gender_df_sum_male = aggregate(all_queries_gender_df$male.count, list(all_queries_gender_df$Brand), sum)
    all_queries_gender_df_sum_female = aggregate(all_queries_gender_df$female.count, list(all_queries_gender_df$Brand), sum)

    all_queries_gender_cleaned_df <- all_queries_gender_df_sum_male %>%
      left_join(all_queries_gender_df_sum_female, by="Group.1")

      
    all_queries_gender_cleaned_df <- all_queries_gender_cleaned_df %>%
      dplyr::rename(Brand = Group.1, sum_male_count = x.x, sum_female_count = x.y)
    
    
    all_queries_gender_cleaned_df <- all_queries_gender_cleaned_df %>%
      mutate(Male_Voice = sum_male_count/(sum_male_count + sum_female_count)) %>%
      mutate(Female_Voice = sum_female_count/(sum_male_count + sum_female_count)) %>%
      select(Brand, Male_Voice, Female_Voice)
    

    all_queries_gender_cleaned_df$Male_Voice <- percent(all_queries_gender_cleaned_df$Male_Voice)                          
    all_queries_gender_cleaned_df$Female_Voice <- percent(all_queries_gender_cleaned_df$Female_Voice)                          
    
    
    #demographics cleaning
    
    all_queries_demographics_cleaned_df <- all_queries_demographics_df %>%
      #select(Brand, query.average_household_size, query.median_household_income, query.income_0k_25k, query.race_white, query.race_black, query.race_asian, query.race_mixed, query.culture_hispanic, query.education_lt_highschool, query.education_highschool, query.education_some_college, query.education_college) 
      select(Brand, query.median_household_income, query.income_0k_25k, query.education_highschool, query.education_college) %>%
      mutate(Avg_Household_Income = query.median_household_income, Lower_Income_Bracket = (query.income_0k_25k/100), High_School_Graduate = (query.education_highschool/100), College_Graduate = (query.education_college/100))
    
    
    all_queries_demographics_cleaned_df$Lower_Income_Bracket <- percent(all_queries_demographics_cleaned_df$Lower_Income_Bracket)                          
    all_queries_demographics_cleaned_df$High_School_Graduate <- percent(all_queries_demographics_cleaned_df$High_School_Graduate)                          
    all_queries_demographics_cleaned_df$College_Graduate <- percent(all_queries_demographics_cleaned_df$College_Graduate)                          
    
    
    #joining all info into one dataframe
    all_queries_all_endpoints_df <- all_queries_volume_cleaned_df %>%
      left_join(all_queries_sentiment_cleaned_df, by="Brand") %>%
      left_join(all_queries_gender_cleaned_df, by="Brand") %>%
      left_join(all_queries_demographics_cleaned_df, by="Brand")
    
    all_queries_all_endpoints_df$Total_Posts <- formatC(all_queries_all_endpoints_df$Total_Posts, format="d", big.mark=",")   
    all_queries_all_endpoints_df$Average_Daily_Posts <- formatC(all_queries_all_endpoints_df$Average_Daily_Posts, format="d", big.mark=",")   
    all_queries_all_endpoints_df$Avg_Household_Income <- formatC(all_queries_all_endpoints_df$Avg_Household_Income, format="d", big.mark=",")   
    
    all_queries_all_endpoints_df$Avg_Household_Income <- paste('$',formatC(all_queries_all_endpoints_df$Avg_Household_Income, big.mark=',', format = 'f'))
    
    all_queries_all_endpoints_df$Male_Voice <- formatC(all_queries_all_endpoints_df$Male_Voice, digits = 1, format = "f")
    all_queries_all_endpoints_df$Female_Voice <- formatC(all_queries_all_endpoints_df$Female_Voice, digits = 1, format = "f")
    all_queries_all_endpoints_df$Lower_Income_Bracket <- formatC(all_queries_all_endpoints_df$Lower_Income_Bracket, digits = 1, format = "f")
    all_queries_all_endpoints_df$High_School_Graduate <- formatC(all_queries_all_endpoints_df$High_School_Graduate, digits = 1, format = "f")
    all_queries_all_endpoints_df$College_Graduate <- formatC(all_queries_all_endpoints_df$College_Graduate, digits = 1, format = "f")
    
    all_queries_all_endpoints_df2 <- all_queries_all_endpoints_df
    
    all_queries_all_endpoints_df <- all_queries_all_endpoints_df %>%
      dplyr::rename('Total Posts' = Total_Posts, 'Average Daily Posts'= Average_Daily_Posts, 'Positive Sentiment' = Positive_Sentiment, 'Male Voice' = Male_Voice, 'Female Voice' = Female_Voice, 'Avg Household Income' = Avg_Household_Income, 'Lower Income Bracket'= Lower_Income_Bracket, 'High School Graduate' = High_School_Graduate, 'College Graduate' = College_Graduate)
    

    size_and_sentiment_table_df <- all_queries_all_endpoints_df2 %>%
      select(Brand, Total_Posts, Average_Daily_Posts, Positive_Sentiment)
    
    
    demographics_table_df <- all_queries_all_endpoints_df2 %>%
      select(Brand, Male_Voice, Female_Voice, Avg_Household_Income, Lower_Income_Bracket, High_School_Graduate, College_Graduate)
    
    
    combination_table_df <- all_queries_all_endpoints_df2 %>%
      select(Brand, Total_Posts, Average_Daily_Posts, Positive_Sentiment, Male_Voice, Female_Voice, Avg_Household_Income, Lower_Income_Bracket, High_School_Graduate, College_Graduate)
    


    output$table1_2 <- renderDataTable(
      all_queries_all_endpoints_df %>% select(Brand, 'Total Posts', 'Average Daily Posts', 'Positive Sentiment'), options = list(pageLength = 15, "bPaginate" = FALSE, "bFilter" = FALSE, "bInfo" = FALSE)
    )
    
    
    output$table2_2 <- renderDataTable(
      all_queries_all_endpoints_df %>% select(Brand, 'Male Voice', 'Female Voice', 'Avg Household Income', 'Lower Income Bracket', 'High School Graduate', 'College Graduate'), options = list(pageLength = 15, "bPaginate" = FALSE, "bFilter" = FALSE, "bInfo" = FALSE)
    )
    
    
    output$table3_2 <- renderDataTable(
      all_queries_all_endpoints_df %>% select(Brand, 'Total Posts', 'Average Daily Posts', 'Positive Sentiment', 'Male Voice', 'Female Voice', 'Avg Household Income', 'Lower Income Bracket', 'High School Graduate', 'College Graduate'), options = list(pageLength = 15, "bPaginate" = FALSE, "bFilter" = FALSE, "bInfo" = FALSE)
    )
    
    
    
    output$downloadData1_2 <- downloadHandler(
      filename = function() {
        paste(input$brand1_2, "_", input$brand2_2, "_online_size_&_sentiment", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(size_and_sentiment_table_df, file, row.names = FALSE)
      }
    )
    
    output$downloadData2_2 <- downloadHandler(
      filename = function() {
        paste(input$brand1_2, "_", input$brand2_2, "_online_demographics", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(demographics_table_df, file, row.names = FALSE)
      }
    )
    
    output$downloadData3_2 <- downloadHandler(
      filename = function() {
        paste(input$brand1_2, "_", input$brand2_2, "_all_online_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(combination_table_df, file, row.names = FALSE)
      }
    )
    
    
    removeModal()

     
  })
  
  
  observeEvent(input$button3,{
    showModal(modalDialog("Loading data...This should only take about 1 minute...", footer=NULL))
    
    
    #data derived from API call
    
    #components of API request to concatenate, including authentication and parameters
    base <- "https://atlas.infegy.com/api/v2/"
    endpoint1 <- "volume"
    endpoint2 <- "sentiment"
    endpoint3 <- "gender"
    endpoint4 <- "demographics"
    api_key <- ### THIRD-PARTY PRIVATE KEY ###
    
      
    query1_3 <- reactive(gsub(" ","%20", input$brand1_3))
    query2_3 <- reactive(gsub(" ","%20", input$brand2_3))
    query3_3 <- reactive(gsub(" ","%20", input$brand3_3))
    query4_3 <- reactive(gsub(" ","%20", input$brand4_3))
    query5_3 <- reactive(gsub(" ","%20", input$brand5_3))

    start_date_3 <- reactive(input$dateselect3)
    #start_date_3 <-"1%20year%20ago"
    
    end_date <- "end_date=now"
    countries <- "countries=US"
    limit <- "limit=1000"
    
    #full request
    call1_vol <- paste0(base,endpoint1,"?",api_key,"&","query=",query1_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call2_vol <- paste0(base,endpoint1,"?",api_key,"&","query=",query2_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call3_vol <- paste0(base,endpoint1,"?",api_key,"&","query=",query3_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call4_vol <- paste0(base,endpoint1,"?",api_key,"&","query=",query4_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call5_vol <- paste0(base,endpoint1,"?",api_key,"&","query=",query5_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    
    call1_sent <- paste0(base,endpoint2,"?",api_key,"&","query=",query1_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call2_sent <- paste0(base,endpoint2,"?",api_key,"&","query=",query2_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call3_sent <- paste0(base,endpoint2,"?",api_key,"&","query=",query3_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call4_sent <- paste0(base,endpoint2,"?",api_key,"&","query=",query4_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call5_sent <- paste0(base,endpoint2,"?",api_key,"&","query=",query5_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    
    call1_gend <- paste0(base,endpoint3,"?",api_key,"&","query=",query1_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call2_gend <- paste0(base,endpoint3,"?",api_key,"&","query=",query2_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call3_gend <- paste0(base,endpoint3,"?",api_key,"&","query=",query3_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call4_gend <- paste0(base,endpoint3,"?",api_key,"&","query=",query4_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call5_gend <- paste0(base,endpoint3,"?",api_key,"&","query=",query5_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    
    call1_demogr <- paste0(base,endpoint4,"?",api_key,"&","query=",query1_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call2_demogr <- paste0(base,endpoint4,"?",api_key,"&","query=",query2_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call3_demogr <- paste0(base,endpoint4,"?",api_key,"&","query=",query3_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call4_demogr <- paste0(base,endpoint4,"?",api_key,"&","query=",query4_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    call5_demogr <- paste0(base,endpoint4,"?",api_key,"&","query=",query5_3(),"&","start_date=",start_date_3(),"&",end_date,"&",countries,"&",limit)
    
    
    #volume calls
    
    #volume call 1
    get_query_volume_1 <- GET(call1_vol)
    get_query_volume_text_1 <- httr::content(get_query_volume_1, "text")
    get_query_volume_json_1 <- fromJSON(get_query_volume_text_1, flatten = TRUE)
    get_query_volume_df_1 <- as.data.frame(get_query_volume_json_1$output)
    get_query_volume_df_1 <- get_query_volume_df_1 %>% mutate(Brand=paste(tools::toTitleCase(input$brand1_3)))
    
    #volume call 2
    get_query_volume_2 <- GET(call2_vol)
    get_query_volume_text_2 <- httr::content(get_query_volume_2, "text")
    get_query_volume_json_2 <- fromJSON(get_query_volume_text_2, flatten = TRUE)
    get_query_volume_df_2 <- as.data.frame(get_query_volume_json_2$output)
    get_query_volume_df_2 <- get_query_volume_df_2 %>% mutate(Brand=paste(tools::toTitleCase(input$brand2_3)))
    
    #volume call 3
    get_query_volume_3 <- GET(call3_vol)
    get_query_volume_text_3 <- httr::content(get_query_volume_3, "text")
    get_query_volume_json_3 <- fromJSON(get_query_volume_text_3, flatten = TRUE)
    get_query_volume_df_3 <- as.data.frame(get_query_volume_json_3$output)
    get_query_volume_df_3 <- get_query_volume_df_3 %>% mutate(Brand=paste(tools::toTitleCase(input$brand3_3)))
    
    #volume call 4
    get_query_volume_4 <- GET(call4_vol)
    get_query_volume_text_4 <- httr::content(get_query_volume_4, "text")
    get_query_volume_json_4 <- fromJSON(get_query_volume_text_4, flatten = TRUE)
    get_query_volume_df_4 <- as.data.frame(get_query_volume_json_4$output)
    get_query_volume_df_4 <- get_query_volume_df_4 %>% mutate(Brand=paste(tools::toTitleCase(input$brand4_3)))
    
    #volume call 5
    get_query_volume_5 <- GET(call5_vol)
    get_query_volume_text_5 <- httr::content(get_query_volume_5, "text")
    get_query_volume_json_5 <- fromJSON(get_query_volume_text_5, flatten = TRUE)
    get_query_volume_df_5 <- as.data.frame(get_query_volume_json_5$output)
    get_query_volume_df_5 <- get_query_volume_df_5 %>% mutate(Brand=paste(tools::toTitleCase(input$brand5_3)))
    
    #binding volume calls
    all_queries_volume_df <- rbind(get_query_volume_df_1, get_query_volume_df_2, get_query_volume_df_3, get_query_volume_df_4, get_query_volume_df_5)
    
    
    
    #sentiment calls#
    
    #sentiment call 1
    get_query_sentiment_1 <- GET(call1_sent)
    get_query_sentiment_text_1 <- httr::content(get_query_sentiment_1, "text")
    get_query_sentiment_json_1 <- fromJSON(get_query_sentiment_text_1, flatten = TRUE)
    get_query_sentiment_df_1 <- as.data.frame(get_query_sentiment_json_1$output)
    get_query_sentiment_df_1 <- get_query_sentiment_df_1 %>% mutate(Brand=paste(tools::toTitleCase(input$brand1_3)))
    
    #sentiment call 2
    get_query_sentiment_2 <- GET(call2_sent)
    get_query_sentiment_text_2 <- httr::content(get_query_sentiment_2, "text")
    get_query_sentiment_json_2 <- fromJSON(get_query_sentiment_text_2, flatten = TRUE)
    get_query_sentiment_df_2 <- as.data.frame(get_query_sentiment_json_2$output)
    get_query_sentiment_df_2 <- get_query_sentiment_df_2 %>% mutate(Brand=paste(tools::toTitleCase(input$brand2_3)))
    
    #sentiment call 3
    get_query_sentiment_3 <- GET(call3_sent)
    get_query_sentiment_text_3 <- httr::content(get_query_sentiment_3, "text")
    get_query_sentiment_json_3 <- fromJSON(get_query_sentiment_text_3, flatten = TRUE)
    get_query_sentiment_df_3 <- as.data.frame(get_query_sentiment_json_3$output)
    get_query_sentiment_df_3 <- get_query_sentiment_df_3 %>% mutate(Brand=paste(tools::toTitleCase(input$brand3_3)))
    
    #sentiment call 4
    get_query_sentiment_4 <- GET(call4_sent)
    get_query_sentiment_text_4 <- httr::content(get_query_sentiment_4, "text")
    get_query_sentiment_json_4 <- fromJSON(get_query_sentiment_text_4, flatten = TRUE)
    get_query_sentiment_df_4 <- as.data.frame(get_query_sentiment_json_4$output)
    get_query_sentiment_df_4 <- get_query_sentiment_df_4 %>% mutate(Brand=paste(tools::toTitleCase(input$brand4_3)))
    
    #sentiment call 5
    get_query_sentiment_5 <- GET(call5_sent)
    get_query_sentiment_text_5 <- httr::content(get_query_sentiment_5, "text")
    get_query_sentiment_json_5 <- fromJSON(get_query_sentiment_text_5, flatten = TRUE)
    get_query_sentiment_df_5 <- as.data.frame(get_query_sentiment_json_5$output)
    get_query_sentiment_df_5 <- get_query_sentiment_df_5 %>% mutate(Brand=paste(tools::toTitleCase(input$brand5_3)))
    
    #binding sentiment calls
    all_queries_sentiment_df <- rbind(get_query_sentiment_df_1, get_query_sentiment_df_2, get_query_sentiment_df_3, get_query_sentiment_df_4, get_query_sentiment_df_5)
    
    
    #gender calls#
    
    #gender call 1
    get_query_gender_1 <- GET(call1_gend)
    get_query_gender_text_1 <- httr::content(get_query_gender_1, "text")
    get_query_gender_json_1 <- fromJSON(get_query_gender_text_1, flatten = TRUE)
    get_query_gender_df_1 <- as.data.frame(get_query_gender_json_1$output)
    get_query_gender_df_1 <- get_query_gender_df_1 %>% mutate(Brand=paste(tools::toTitleCase(input$brand1_3)))
    
    #gender call 2
    get_query_gender_2 <- GET(call2_gend)
    get_query_gender_text_2 <- httr::content(get_query_gender_2, "text")
    get_query_gender_json_2 <- fromJSON(get_query_gender_text_2, flatten = TRUE)
    get_query_gender_df_2 <- as.data.frame(get_query_gender_json_2$output)
    get_query_gender_df_2 <- get_query_gender_df_2 %>% mutate(Brand=paste(tools::toTitleCase(input$brand2_3)))
    
    #gender call 3
    get_query_gender_3 <- GET(call3_gend)
    get_query_gender_text_3 <- httr::content(get_query_gender_3, "text")
    get_query_gender_json_3 <- fromJSON(get_query_gender_text_3, flatten = TRUE)
    get_query_gender_df_3 <- as.data.frame(get_query_gender_json_3$output)
    get_query_gender_df_3 <- get_query_gender_df_3 %>% mutate(Brand=paste(tools::toTitleCase(input$brand3_3)))
    
    #gender call 4
    get_query_gender_4 <- GET(call4_gend)
    get_query_gender_text_4 <- httr::content(get_query_gender_4, "text")
    get_query_gender_json_4 <- fromJSON(get_query_gender_text_4, flatten = TRUE)
    get_query_gender_df_4 <- as.data.frame(get_query_gender_json_4$output)
    get_query_gender_df_4 <- get_query_gender_df_4 %>% mutate(Brand=paste(tools::toTitleCase(input$brand4_3)))
    
    #gender call 5
    get_query_gender_5 <- GET(call5_gend)
    get_query_gender_text_5 <- httr::content(get_query_gender_5, "text")
    get_query_gender_json_5 <- fromJSON(get_query_gender_text_5, flatten = TRUE)
    get_query_gender_df_5 <- as.data.frame(get_query_gender_json_5$output)
    get_query_gender_df_5 <- get_query_gender_df_5 %>% mutate(Brand=paste(tools::toTitleCase(input$brand5_3)))
    
    #binding gender calls
    all_queries_gender_df <- rbind(get_query_gender_df_1, get_query_gender_df_2, get_query_gender_df_3, get_query_gender_df_4, get_query_gender_df_5)
    
    
    #demographics calls#
    
    #demographics call 1
    get_query_demographics_1 <- GET(call1_demogr)
    get_query_demographics_text_1 <- httr::content(get_query_demographics_1, "text")
    get_query_demographics_json_1 <- fromJSON(get_query_demographics_text_1, flatten = TRUE)
    get_query_demographics_df_1 <- as.data.frame(get_query_demographics_json_1$output)
    get_query_demographics_df_1 <- get_query_demographics_df_1 %>% mutate(Brand=paste(tools::toTitleCase(input$brand1_3)))
    
    #demographics call 2
    get_query_demographics_2 <- GET(call2_demogr)
    get_query_demographics_text_2 <- httr::content(get_query_demographics_2, "text")
    get_query_demographics_json_2 <- fromJSON(get_query_demographics_text_2, flatten = TRUE)
    get_query_demographics_df_2 <- as.data.frame(get_query_demographics_json_2$output)
    get_query_demographics_df_2 <- get_query_demographics_df_2 %>% mutate(Brand=paste(tools::toTitleCase(input$brand2_3)))
    
    #demographics call 3
    get_query_demographics_3 <- GET(call3_demogr)
    get_query_demographics_text_3 <- httr::content(get_query_demographics_3, "text")
    get_query_demographics_json_3 <- fromJSON(get_query_demographics_text_3, flatten = TRUE)
    get_query_demographics_df_3 <- as.data.frame(get_query_demographics_json_3$output)
    get_query_demographics_df_3 <- get_query_demographics_df_3 %>% mutate(Brand=paste(tools::toTitleCase(input$brand3_3)))
    
    #demographics call 4
    get_query_demographics_4 <- GET(call4_demogr)
    get_query_demographics_text_4 <- httr::content(get_query_demographics_4, "text")
    get_query_demographics_json_4 <- fromJSON(get_query_demographics_text_4, flatten = TRUE)
    get_query_demographics_df_4 <- as.data.frame(get_query_demographics_json_4$output)
    get_query_demographics_df_4 <- get_query_demographics_df_4 %>% mutate(Brand=paste(tools::toTitleCase(input$brand4_3)))
    
    #demographics call 5
    get_query_demographics_5 <- GET(call5_demogr)
    get_query_demographics_text_5 <- httr::content(get_query_demographics_5, "text")
    get_query_demographics_json_5 <- fromJSON(get_query_demographics_text_5, flatten = TRUE)
    get_query_demographics_df_5 <- as.data.frame(get_query_demographics_json_5$output)
    get_query_demographics_df_5 <- get_query_demographics_df_5 %>% mutate(Brand=paste(tools::toTitleCase(input$brand5_3)))
    
    
    #binding demographics calls
    all_queries_demographics_df <- rbind(get_query_demographics_df_1, get_query_demographics_df_2, get_query_demographics_df_3, get_query_demographics_df_4, get_query_demographics_df_5)
    
    
    library(tidyverse)
    library(magrittr)
    

    all_queries_volume_cleaned_df_sum_posts = aggregate(all_queries_volume_df$posts_universe, list(all_queries_volume_df$Brand), sum)
    all_queries_volume_cleaned_df_avg_posts = aggregate(all_queries_volume_df$posts_universe, list(all_queries_volume_df$Brand), mean)
    
    all_queries_volume_cleaned_df <- all_queries_volume_cleaned_df_sum_posts %>%
      left_join(all_queries_volume_cleaned_df_avg_posts, by="Group.1")
    
    all_queries_volume_cleaned_df <- all_queries_volume_cleaned_df %>%
      dplyr::rename(Brand = Group.1, Total_Posts = x.x, Average_Daily_Posts = x.y)
    
    
    #sentiment cleaning
    
    all_queries_sentiment_df_sum_pos = aggregate(all_queries_sentiment_df$positive_documents, list(all_queries_sentiment_df$Brand), sum)
    all_queries_sentiment_df_sum_neutr = aggregate(all_queries_sentiment_df$neutral_documents, list(all_queries_sentiment_df$Brand), sum)
    all_queries_sentiment_df_sum_all = aggregate(all_queries_sentiment_df$documents, list(all_queries_sentiment_df$Brand), sum)
    
    
    all_queries_sentiment_cleaned_df <- all_queries_sentiment_df_sum_pos %>%
      left_join(all_queries_sentiment_df_sum_neutr, by="Group.1") %>%
      left_join(all_queries_sentiment_df_sum_all, by="Group.1")
    
    
    all_queries_sentiment_cleaned_df <- all_queries_sentiment_cleaned_df %>%
      dplyr::rename(Brand = Group.1, sum_positive_documents = x.x, sum_neutral_documents = x.y, sum_documents = x)
    
    
    all_queries_sentiment_cleaned_df <- all_queries_sentiment_cleaned_df %>%
      mutate(half_sum_neutral_documents = (sum_neutral_documents/2)) %>%
      mutate(Positive_Sentiment = (sum_positive_documents + half_sum_neutral_documents)/sum_documents) %>%
      select(Brand, Positive_Sentiment)
    
    
    all_queries_sentiment_cleaned_df$Positive_Sentiment <- percent(all_queries_sentiment_cleaned_df$Positive_Sentiment)                          
    
    
    #gender cleaning
    
    all_queries_gender_df_sum_male = aggregate(all_queries_gender_df$male.count, list(all_queries_gender_df$Brand), sum)
    all_queries_gender_df_sum_female = aggregate(all_queries_gender_df$female.count, list(all_queries_gender_df$Brand), sum)
    
    all_queries_gender_cleaned_df <- all_queries_gender_df_sum_male %>%
      left_join(all_queries_gender_df_sum_female, by="Group.1")
    
    
    all_queries_gender_cleaned_df <- all_queries_gender_cleaned_df %>%
      dplyr::rename(Brand = Group.1, sum_male_count = x.x, sum_female_count = x.y)
    
    
    all_queries_gender_cleaned_df <- all_queries_gender_cleaned_df %>%
      mutate(Male_Voice = sum_male_count/(sum_male_count + sum_female_count)) %>%
      mutate(Female_Voice = sum_female_count/(sum_male_count + sum_female_count)) %>%
      select(Brand, Male_Voice, Female_Voice)
    

    all_queries_gender_cleaned_df$Male_Voice <- percent(all_queries_gender_cleaned_df$Male_Voice)                          
    all_queries_gender_cleaned_df$Female_Voice <- percent(all_queries_gender_cleaned_df$Female_Voice)                          
    
    
    #demographics cleaning
    
    all_queries_demographics_cleaned_df <- all_queries_demographics_df %>%
      #select(Brand, query.average_household_size, query.median_household_income, query.income_0k_25k, query.race_white, query.race_black, query.race_asian, query.race_mixed, query.culture_hispanic, query.education_lt_highschool, query.education_highschool, query.education_some_college, query.education_college) 
      select(Brand, query.median_household_income, query.income_0k_25k, query.education_highschool, query.education_college) %>%
      mutate(Avg_Household_Income = query.median_household_income, Lower_Income_Bracket = (query.income_0k_25k/100), High_School_Graduate = (query.education_highschool/100), College_Graduate = (query.education_college/100))
    
    
    all_queries_demographics_cleaned_df$Lower_Income_Bracket <- percent(all_queries_demographics_cleaned_df$Lower_Income_Bracket)                          
    all_queries_demographics_cleaned_df$High_School_Graduate <- percent(all_queries_demographics_cleaned_df$High_School_Graduate)                          
    all_queries_demographics_cleaned_df$College_Graduate <- percent(all_queries_demographics_cleaned_df$College_Graduate)                          
    
    
    #joining all info into one dataframe
    all_queries_all_endpoints_df <- all_queries_volume_cleaned_df %>%
      left_join(all_queries_sentiment_cleaned_df, by="Brand") %>%
      left_join(all_queries_gender_cleaned_df, by="Brand") %>%
      left_join(all_queries_demographics_cleaned_df, by="Brand")
    
    
    
    
    all_queries_all_endpoints_df$Total_Posts <- formatC(all_queries_all_endpoints_df$Total_Posts, format="d", big.mark=",")   
    all_queries_all_endpoints_df$Average_Daily_Posts <- formatC(all_queries_all_endpoints_df$Average_Daily_Posts, format="d", big.mark=",")   
    all_queries_all_endpoints_df$Avg_Household_Income <- formatC(all_queries_all_endpoints_df$Avg_Household_Income, format="d", big.mark=",")   
    
    all_queries_all_endpoints_df$Avg_Household_Income <- paste('$',formatC(all_queries_all_endpoints_df$Avg_Household_Income, big.mark=',', format = 'f'))
    
    all_queries_all_endpoints_df$Male_Voice <- formatC(all_queries_all_endpoints_df$Male_Voice, digits = 1, format = "f")
    all_queries_all_endpoints_df$Female_Voice <- formatC(all_queries_all_endpoints_df$Female_Voice, digits = 1, format = "f")
    all_queries_all_endpoints_df$Lower_Income_Bracket <- formatC(all_queries_all_endpoints_df$Lower_Income_Bracket, digits = 1, format = "f")
    all_queries_all_endpoints_df$High_School_Graduate <- formatC(all_queries_all_endpoints_df$High_School_Graduate, digits = 1, format = "f")
    all_queries_all_endpoints_df$College_Graduate <- formatC(all_queries_all_endpoints_df$College_Graduate, digits = 1, format = "f")

    
    all_queries_all_endpoints_df2 <- all_queries_all_endpoints_df
    
    all_queries_all_endpoints_df <- all_queries_all_endpoints_df %>%
      dplyr::rename('Total Posts' = Total_Posts, 'Average Daily Posts'= Average_Daily_Posts, 'Positive Sentiment' = Positive_Sentiment, 'Male Voice' = Male_Voice, 'Female Voice' = Female_Voice, 'Avg Household Income' = Avg_Household_Income, 'Lower Income Bracket'= Lower_Income_Bracket, 'High School Graduate' = High_School_Graduate, 'College Graduate' = College_Graduate)
    
    
    
    size_and_sentiment_table_df <- all_queries_all_endpoints_df2 %>%
      select(Brand, Total_Posts, Average_Daily_Posts, Positive_Sentiment)
    
    
    demographics_table_df <- all_queries_all_endpoints_df2 %>%
      select(Brand, Male_Voice, Female_Voice, Avg_Household_Income, Lower_Income_Bracket, High_School_Graduate, College_Graduate)
    
    
    combination_table_df <- all_queries_all_endpoints_df2 %>%
      select(Brand, Total_Posts, Average_Daily_Posts, Positive_Sentiment, Male_Voice, Female_Voice, Avg_Household_Income, Lower_Income_Bracket, High_School_Graduate, College_Graduate)
    
    
    
    output$table1_3 <- renderDataTable(
      all_queries_all_endpoints_df %>% select(Brand, 'Total Posts', 'Average Daily Posts', 'Positive Sentiment'), options = list(pageLength = 15, "bPaginate" = FALSE, "bFilter" = FALSE, "bInfo" = FALSE)
    )
    
    
    
    output$table2_3 <- renderDataTable(
      all_queries_all_endpoints_df %>% select(Brand, 'Male Voice', 'Female Voice', 'Avg Household Income', 'Lower Income Bracket', 'High School Graduate', 'College Graduate'), options = list(pageLength = 15, "bPaginate" = FALSE, "bFilter" = FALSE, "bInfo" = FALSE)
    )
    
    
    output$table3_3 <- renderDataTable(
      all_queries_all_endpoints_df %>% select(Brand, 'Total Posts', 'Average Daily Posts', 'Positive Sentiment', 'Male Voice', 'Female Voice', 'Avg Household Income', 'Lower Income Bracket', 'High School Graduate', 'College Graduate'), options = list(pageLength = 15, "bPaginate" = FALSE, "bFilter" = FALSE, "bInfo" = FALSE)
    )
    
    
    output$downloadData1_3 <- downloadHandler(
      filename = function() {
        paste(input$brand1_3, "_", input$brand2_3, "_", input$brand3_3, "_", input$brand4_3, "_", input$brand5_3, "_online_size_&_sentiment", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(size_and_sentiment_table_df, file, row.names = FALSE)
      }
    )
    
    output$downloadData2_3 <- downloadHandler(
      filename = function() {
        paste(input$brand1_3, "_", input$brand2_3, "_", input$brand3_3, "_", input$brand4_3, "_", input$brand5_3, "_online_demographics", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(demographics_table_df, file, row.names = FALSE)
      }
    )
    
    output$downloadData3_3 <- downloadHandler(
      filename = function() {
        paste(input$brand1_3, "_", input$brand2_3, "_", input$brand3_3, "_", input$brand4_3, "_", input$brand5_3, "_all_online_data", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(combination_table_df, file, row.names = FALSE)
      }
    )
    
    
    removeModal()
    
    
  }) 
  
}   



shinyApp(ui = ui, server = server)

#remove(list = ls())

