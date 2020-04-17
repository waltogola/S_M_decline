# S_M_decline
### Finding stocks that are inversely 
### correlated with Covid-19 market downturn?

### Don't use any of this analysis for your own financial decisions.
### This is not financial advice.

library(tidyverse)
library(tidyquant)
library(scales)
library(ggrepel)

### get stocks in Sp500 index
sp500stocks <- tq_index("SP500")

### clean up symbols for downstream analysis
sp500stocks <-  sp500stocks %>% mutate(symbol = case_when(
      symbol=='BRK.B' ~ 'BRK-B',
      symbol=='BF.B' ~ 'BF-B',
      TRUE ~ symbol
   )
)

### view top 50 stocks by sp500 weight
sp500stocks %>%
      top_n(weight,n=50) %>% 
      ggplot(aes(x=reorder(symbol,-weight), y=weight)) +
      geom_col(aes(fill=weight)) +
      theme(legend.position = "None",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                         limits = c(0,0.06)) +
      labs(title="Top 50 S&P 500 Stocks by Index Weight (March 2020)",
           y="Index Weight",
           x="Company Symbol")

start_date <- c("2020-01-01")
end_date <- c("2020-03-31")

### VFINX comparision baseline 
### Vanguard fund that tracks similar to the SP500 index
VFINX <- tq_get(x='VFINX',
                get  = "stock.prices",
                from = start_date,
                to   = end_date) %>%
      dplyr::select(symbol, date, close) %>%
      mutate(label="S&P 500 Index (VFINX)")

### takes a while to run, only run if variable not already loaded
if(!exists("prices_sp500stocks")) prices_sp500stocks <- sp500stocks$symbol %>%
      ### gets stock price data from Yahoo Finance
      tq_get(get  = "stock.prices",
             from = start_date,
             to   = end_date) %>%
      dplyr::select(symbol, date, close)

### drop stocks we didn't receive full data for
prices_sp500stocks_clean <- prices_sp500stocks %>%
      group_by(symbol) %>%
      filter(n()>=60) %>%
      ungroup() %>%
      mutate(label="S&P 500 Individual Stock")

### function to normalize close price data
### This will use see price movements upward or downward following
### covid acceleration
min_max_norm <- function(x) {
      (x - min(x)) / (max(x) - min(x))
}

filter_cut_point <- c('2020-02-01')

stocks_and_index_df <- prices_sp500stocks_clean %>%
      rbind(VFINX) %>%
      filter(date>filter_cut_point) %>%
      group_by(symbol) %>%
      ### could also normalize on the log close price
      mutate(close_normalized = min_max_norm(close)) %>%
      mutate(stock_line_label= ifelse(date==max(date),symbol,NA)) %>%
      ungroup()

stocks_and_index_df %>%
      ggplot(aes(x=substr(factor(date),6,10),y=close_normalized,
                 group=symbol, color=label, alpha=label)) +
      geom_line() +
      scale_alpha_discrete(range = c(1, 0.1), guide = 'none') +
      labs(title="Feb - Mar 2020: Stock Price Trends
Stock market decline correlates with acceleration of Covid-19 pandemic.",
           subtitle = "Y-axis value of 1 = max close price during observation timeframe.
Y-axis value of 0 = minimum close price during observation timeframe.",
           x="Stock Market Trading Day",
           y="Min Max Normalized Close Price") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            legend.position='top', 
            legend.justification='left',
            legend.direction='horizontal',
            plot.title = element_text(size=18),
            plot.subtitle = element_text(size=10)) +
      scale_color_manual(values=c("dodgerblue","grey40")) +
      guides(colour = guide_legend(title=NULL,
                                   override.aes = list(alpha = 1, size=3)))

VFINX_normalized <- filter(stocks_and_index_df,symbol=="VFINX")

### get stock correlations with the sp500 proxy
### note: correlation is the same if actual close price is used or 
### of normalized close price is used
cor_df <- stocks_and_index_df %>%
      group_by(symbol) %>%
      summarise(correlation_with_SP500_proxy = 
                      cor(VFINX_normalized$close_normalized, 
                          close_normalized)) %>%
      arrange(correlation_with_SP500_proxy)

### surface 20 stocks that have been least correlated with the sp500
least_correlated_stocks <- 
      cor_df %>% top_n(correlation_with_SP500_proxy, n=-20)

stocks_and_index_df %>%
      filter(symbol %in% c("VFINX",unique(least_correlated_stocks$symbol))) %>%
      ggplot(aes(x=substr(factor(date),6,10),y=close_normalized,
                 group=symbol, color=label)) +
      geom_line(aes(alpha=label)) +
      scale_alpha_discrete(range = c(1, 0.2), guide = 'none') +
      geom_label_repel(aes(label=stock_line_label),
                       direction = "y",
                       box.padding = 0.1,
                       segment.size = 0.2,
                       segment.alpha = 0.5,
                       label.padding = 0.1,
                       nudge_x = 5,
                       na.rm=T) +
      scale_x_discrete(expand = expansion(mult=c(0.05,.15))) +
      labs(title="Feb - Mar 2020: Stock Price Trends
20 stocks least correlated with S&P 500 index decline.",
           subtitle = "Y-axis value of 1 = max close price during observation timeframe.
Y-axis value of 0 = minimum close price during observation timeframe.",
           x="Stock Market Trading Day",
           y="Min Max Normalized Close Price") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            legend.position='none',
            plot.title = element_text(size=18),
            plot.subtitle = element_text(size=10)) +
      scale_color_manual(values=c("dodgerblue","grey40")) +
      guides(colour = guide_legend(title=NULL,
                                   override.aes = list(alpha = 1, size=3)))

least_correlated_stocks %>% 
      inner_join(sp500stocks,by=c("symbol")) %>%
      dplyr::select(correlation_with_SP500_proxy, symbol, company, sector) %>%
      mutate(correlation_with_SP500_proxy = round(correlation_with_SP500_proxy,2)) %>%
      arrange(correlation_with_SP500_proxy) %>%
      mutate(row_index = row_number()) %>%
      gather(key=key, value=value, correlation_with_SP500_proxy, symbol, company, sector) %>%
      mutate(key=factor(key,levels=c('correlation_with_SP500_proxy','symbol','company','sector'))) %>%
      ggplot(aes(y=reorder(row_index,-row_index),x=key)) +
            geom_text(aes(label=value)) +
      labs(title="Stock info for least correlated stocks with S&P 500 index decline",
           x="", y="Least Correlated Rank",
           caption = "S&P 500 proxy = VFINX (Vanguard 500 Index Fund)") +
      theme(plot.title = element_text(size=20),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=10),
            axis.title = element_text(size=18))
