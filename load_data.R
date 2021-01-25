library(riingo)
library(tidyverse)
library(zoo)

start_date <- '2018-03-30'
symbols <- c('AAPL', 'TWLO', 'HUBS', 'ZEN', 'DXCM', 'M', 'OVID', 'GRMN', 'DBX')

# Downloads from tiingo
statements <- riingo_fundamentals_statements(ticker = symbols,
                                             start_date = start_date,
                                             end_date = NULL)

metrics_raw <- riingo_fundamentals_metrics(ticker      = symbols,
                                           start_date  = start_date,
                                           end_date    = NULL) %>%
  select(ticker, date, marketCap, peRatio, pbRatio)

# 
statements %>%
  tidyr::unnest(overview) %>%
  .$dataCode %>%
  unique() %>%
  sort()

overview_metrics <- statements %>%
  unnest(overview) %>%
  filter(dataCode %in% c('rps', 'longTermDebtEquity', 'bookVal'), quarter != 0) %>%
  select(ticker, date, dataCode, value) %>%
  pivot_wider(names_from = dataCode, values_from = value) %>%
  arrange(date, ticker) %>%
  mutate(ttm_revenue_per_share = rollsumr(rps, k = 4, fill = NA))

income_statement_metrics <- statements %>%
  unnest(incomeStatement) %>%
  filter(dataCode %in% c('revenue', 'netinc'), quarter != 0) %>%
  select(ticker, date, dataCode, value) %>%
  pivot_wider(names_from = dataCode, values_from = value) %>%
  arrange(date) %>%
  mutate(netinc_1yr = lag(netinc, 4))

balance_sheet_metrics <- statements %>%
  unnest(balanceSheet) %>%
  filter(dataCode %in% c('totalAssets', 'totalLiabilities', 'cashAndEq'), quarter != 0) %>%
  select(ticker, date, dataCode, value) %>%
  pivot_wider(names_from = dataCode, values_from = value) %>%
  mutate(shareholder_equity = totalAssets - totalLiabilities) %>%
  arrange(date)

cash_flow_metrics <- statements %>%
  unnest(cashFlow) %>%
  filter(dataCode %in% c('freeCashFlow'), quarter != 0) %>%
  select(ticker, date, dataCode, value) %>%
  pivot_wider(names_from = dataCode, values_from = value) %>%
  arrange(date)

metrics_daily <- metrics_raw %>%
  complete(date = seq.POSIXt(min(metrics_raw$date), max(metrics_raw$date), by = 'day'), ticker) %>%
  group_by(ticker) %>%
  arrange(date) %>%
  fill(c(marketCap, peRatio, pbRatio))  %>%
  ungroup()

stock_prices_daily <- riingo_prices(symbols, start_date = start_date, resample_frequency = 'daily') %>%
  select(ticker, date, close)

metrics_daily_joined <- metrics_daily %>%
  left_join(stock_prices_daily) %>%
  left_join(overview_metrics) %>%
  left_join(income_statement_metrics) %>%
  left_join(balance_sheet_metrics) %>%
  left_join(cash_flow_metrics) %>%
  group_by(ticker) %>%
  arrange(date) %>%
  fill(c(close, 
         longTermDebtEquity, 
         rps, 
         bookVal,
         ttm_revenue_per_share, 
         revenue, 
         netinc, 
         netinc_1yr,
         totalAssets, 
         totalLiabilities, 
         freeCashFlow,
         shareholder_equity,
         cashAndEq)) %>%
  ungroup()

metrics_daily_joined <- metrics_daily_joined  %>%
  mutate(psRatio = close /ttm_revenue_per_share,
         pcRatio = marketCap / freeCashFlow,
         debtEquityRatio = totalLiabilities / shareholder_equity,
         earningsGrowth = ((netinc - netinc_1yr) / netinc_1yr))


saveRDS(metrics_daily_joined, 'data/metrics_daily.rds')