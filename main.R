rm(list=ls())

#Import Library, Read 
library(tidyverse)
library(ggplot2)
library(readr)
library(ggthemes)
library(reshape2)

#Read and Rename columns
main_df <- read_csv("Financial Statements.csv")
colnames(main_df) = c('Year','Company', 'Category', 'Market_Cap', 'Revenue',	'Gross_Profit',	'Net_Income',	'Earning_Per_Share',	"EBITDA",	"Shareholder_Equity",	"Cash_Flow_from_Operating",	"Cash_Flow_from_Investing",	"Cash_Flow_from_Financial_Activities",	"Current_Ratio", "Debt/Equity_Ratio",	"ROE","ROA"	,"ROI"	,"Net_Profit_Margin"	,"Free_Cash_Flow_per_Share"	,"ROTE",	"No_of_Employees",	"Inflation_Rate")

#Add in COGS, Expense, Assets, Total Liabilities
main_df <- main_df |> 
  mutate(COGS = Revenue - Gross_Profit, 
         Expense = Revenue - Net_Income,
         Assets = Net_Income/(ROA*0.01),
         Liabilities = Assets - Shareholder_Equity,
         )

#Splitting to each company and store in a list
main_list = c()
i = 1
for (company in unique(main_df$Company)){
  small_df <- main_df |> filter(company == Company)
  main_list <- append(main_list, list(small_df))
  names(main_list)[i] = company
  i = i + 1
}

#Splitting to each year and store in a list
main_list_year = c()
i = 1
for (year in unique(main_df$Year)){
  small_df <- main_df |> filter(year == Year)
  main_list_year <- append(main_list_year, list(small_df))
  names(main_list_year)[i] = year
  i = i + 1
}

#Splitting to small and big company
small_company_df <- main_df |> filter(!Company %in% c('AAPL', 'AMZN', 'GOOG', 'MSFT', 'NVDA'))
big_company_df <- main_df |> filter(Company %in% c('AAPL', 'AMZN', 'GOOG', 'MSFT'))


#Graphing
#Employee Change Graph
employee <- function(company_employee){
  ggplot(main_list[[company_employee]], aes(x = Year, y = No_of_Employees)) +
    geom_point(color = "#014d64")+
    geom_smooth(se = FALSE, method = "lm", color = "#db4c39")+
    scale_x_continuous(breaks = main_list[[company_employee]][["Year"]])+
    theme_economist()+
    labs(
      y = 'Employees',
    )
}
employee('AAPL')

#Market Cap Value Change
market_cap_small <- ggplot(small_company_df, aes(x = Year, y = Market_Cap, color = Company))+
  geom_line()+
  theme_economist()+
  scale_x_continuous(breaks = small_company_df$Year)+
  scale_y_continuous(labels = scales::dollar)+
  labs(
       y = "",
       x = ""
  )
market_cap_big <- ggplot(big_company_df, aes(x = Year, y = Market_Cap, color = Company))+
  geom_line()+
  theme_economist()+
  scale_x_continuous(breaks = big_company_df$Year)+
  scale_y_continuous(labels = scales::dollar)+
  labs(
       y = "",
       x =""
       
  )
market_cap_big
                       

#Income Statement Line Graph
income_statement <- function(company_income_statement){
  ggplot(main_list[[company_income_statement]])+
    geom_smooth(aes(x = Year, y = Revenue, color = "Revenue"), se = FALSE)+
    geom_smooth(aes(x = Year, y = Gross_Profit, color = "Gross Profit"), se = FALSE)+
    geom_smooth(aes(x = Year, y = Net_Income, color = "Net_Income"), se = FALSE)+
    labs(
         y = '',
         x = '',
         color = "Line",
        )+
    scale_x_continuous(breaks = main_list[[company_income_statement]][["Year"]])+
    scale_y_continuous(labels = scales::dollar)+
    theme_economist()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.7))
}
income_statement('MSFT') 

#Cash Flow Statement Bar Chart
cash_flow <- function(company_cash_flow){
  main_list_melted <- melt(main_list[[company_cash_flow]][,c('Year','Cash_Flow_from_Financial_Activities','Cash_Flow_from_Operating', 'Cash_Flow_from_Investing')],id.vars = 1) 
  ggplot(main_list_melted, aes(x = Year, y = value))+
    geom_col(aes(fill = variable), position = "dodge", width = 0.5)+
    scale_x_continuous(breaks = main_list[[company_cash_flow]][["Year"]])+
    scale_y_continuous(labels = scales::dollar)+
    labs(
         y = "",
         x = "",
         fill = "Activities",
         )+
    scale_fill_manual(values = c('#db4c39', '#76c0c1', '#014d64'),
      labels = c("Financial Activities", "Operating", "Investing"))+
    theme_economist()
}
cash_flow('AAPL')

#Balance Sheet Bar Graph
company_balance_sheet = 'AAPL'
balance_sheet <- function(company_balance_sheet){
  main_list_melted <- melt(main_list[[company_balance_sheet]][,c('Year','Assets','Shareholder_Equity', 'Liabilities')],id.vars = 1)
  main_list_melted <- main_list_melted |> mutate(group = ifelse(variable == 'Assets','A','E+L'))
  ggplot(main_list_melted, aes(x = group, y = value))+
    geom_col(aes(fill = variable), position = 'stack', stat = 'identity')+
    facet_grid(~Year, switch = "x")+
    scale_y_continuous(labels = scales::dollar)+
    theme_economist()+
    theme(axis.text.x = element_blank())+
  labs(
    y = "",
    fill = "Category",
    x = ''
   )
    
}
balance_sheet('AAPL')

#Compare Accross Company's Revenue, Gross Profit, Net Income, Expense, COGS (Box-Whisker)


box_whisker_big <- function(type){
  ggplot(big_company_df, aes(x = Company, y = big_company_df[[type]]))+
    geom_boxplot()+
    theme_economist()+
    labs(
      y = "",
      x = "",
      title = type
    )+
    scale_y_continuous(labels = scales::dollar)
  
}
box_whisker_small<- function(type){
  ggplot(small_company_df, aes(x = Company, y = small_company_df[[type]]))+
    geom_boxplot()+
    theme_economist()+
    labs(
      y = "",
      x = "",
      title = type
    )+
    scale_y_continuous(labels = scales::dollar)
}

#Functions For Small and Large Firms
box_graph_decision <- function(size, type){
  if (size == 'Large'){
    box_whisker_big(type)
  }
  else{
    box_whisker_small(type)
  }
}

cap_graph_decision <- function(size){
  if (size == 'Large'){
    market_cap_big
  }
  else{
    market_cap_small
  }
}




