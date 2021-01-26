library(patchwork)
library(plotly)

# Project : Calculate historical VTI for any portfolio stock
# [Done] V0.1 - Calculate x1 - x6 for any stock
# [In Progress] V0.2 - Breakdown x1 by components
# [Not Started] V0.2 - Reverse engineer VTI formula to compute targ

# Objective : Calculate X1 - X3, VTI
# Add other relevant metrics to daily chart (market cap, book value, revenue, earnings, )
# Future
#   Add metric category ?(income, balance, cash flow)

data <- readRDS('data/metrics_daily.rds')

vti_chart <- function(data, symbol) {
  
  # Calculate VTI-V6
  vti_metrics <- data %>%
    filter(ticker == symbol) %>%
    mutate(x1 = ifelse(pbRatio > 0, (pbRatio/1.5)^2, 43),  # 1.5 key value
           x2 = ifelse(psRatio > 0, (psRatio/1.25)^2, 43), # 1.25 key value
           x3 = ifelse(peRatio > 0, (peRatio/15)^2, 43),    # 15 key value, companyies with negative earnings are disqualified
           x4 = pcRatio/10,
           x5 = ifelse(shareholder_equity < 0, 43, 4 * debtEquityRatio),
           x6 = ifelse(earningsGrowth > 0.01, 0.038/earningsGrowth,4),
           x7 = 0,
           vti= 14.286 * (x1 + x2 + x3 + x4 + x5 + x6 + x7))
  
  metrics_chart <- vti_metrics %>%
    select(date, x1, x2, x3, x5, x5, x6, x7) %>%
    pivot_longer(-date, names_to = 'metric') %>%
    mutate(value_adj = value * 14.286) %>%
    ggplot(aes(date, value_adj, color = metric)) + 
    geom_line() +
    ggtitle(symbol) 
  
  vti_chart <- vti_metrics %>%
    select(date, vti) %>%
    pivot_longer(-date, names_to = 'metric') %>%
    ggplot(aes(date, value, color = metric)) + 
    geom_line() +
    ggtitle(symbol)
  
  #library(patchwork)
  #vti_chart + metrics_chart
  subplot(ggplotly(vti_chart), ggplotly(metrics_chart))
}

ratios_chart <- function(data, symbol) {
  ratio_metrics <- data %>%
    filter(ticker == symbol) %>%
    select(date, ticker, peRatio, pbRatio, psRatio, pcRatio) %>%
    pivot_longer(-c(date, ticker), names_to = 'metric') 
  
  ratio_chart <- ratio_metrics %>%
    ggplot(aes(date, value, color = metric)) + 
    geom_line() +
    ggtitle(symbol)
  
  ggplotly(ratio_chart)
}

single_metric_chart <- function(data, symbol, metric) {
  
  d_ata <- data %>%
    filter(ticker == symbol) %>%
    select(date, ticker, !!metric) %>%
    pivot_longer(-c(date, ticker), names_to = 'metric')
  
  chart <- d_ata %>%
    ggplot(aes(date, value, color = metric)) +
    geom_line() +
    ggtitle(symbol)
  
  ggplotly(chart)
}

price_chart <- function(data, symbol) {
  price_data <- data %>%
    filter(ticker == symbol) %>%
    select(date, ticker, close) %>%
    pivot_longer(close, names_to = 'metric')
  
  price_chart <- price_data %>%
    ggplot(aes(date, value, color = metric)) +
    geom_line() +
    ggtitle(symbol)
  
  ggplotly(price_chart)
}

assets_liabilities_chart <- function(data, symbol) {
  l_data <- data %>%
    filter(ticker == symbol) %>%
    select(date, ticker, totalAssets, totalLiabilities) %>%
    pivot_longer(c(totalAssets, totalLiabilities), names_to = 'metric')
  
  chart <- l_data %>%
    ggplot(aes(date, value, color = metric)) +
    geom_line() +
    ggtitle(symbol)
  
  ggplotly(chart)
  
  chart 
}