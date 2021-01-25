
# Essential Ratios

vti_metrics %>% 
  arrange(date) %>%
  filter(date > '2020-01-01') %>%
  select(date, pbRatio, psRatio, peRatio) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date,value, color = name )) + geom_line()


vti_metrics %>%
  arrange(date) %>%
  filter(!is.na(close) & !is.na(ttm_revenue_per_share)) %>%
  mutate(price_growth  = (close - first(close))/first(close),
         sales_growth  = (ttm_revenue_per_share - first(ttm_revenue_per_share))/first(ttm_revenue_per_share),
         book_growth  = (bookVal - first(bookVal))/first(bookVal)
  )  %>%
  select(date, price_growth, bookVal) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, color = name))  + geom_line()

# Calculate VTI-V7
metrics_daily %>%
  mutate(x1 = ifelse(peRatio < 0, 43, 0),
         x2 = ifelse(long_term_debt_equity_ratio < 0, 43, 0),
         x3 = ifelse(long_term_debt_equity_ratio > 4 & long_term_debt_equity_ratio < 10, 43,0),
         x4 = ifelse(long_term_debt_equity_ratio < 0 & long_term_debt_equity_ratio < 10, 
                     -((long_term_debt_equity_ratio_lag_365/long_term_debt_equity_ratio) -1),0),
         x5 = ifelse(x4 <= 0.07, 21.5, 43*(x4/0.15)),
         vti = 18.605*(x1+x2+x3+x5)) %>%
  ggplot(aes(date, vti)) + geom_line()

