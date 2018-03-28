library(plotly)
library(quantmod)


getSymbols("AAPL",src='yahoo')

# basic example of ohlc charts
df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)

df
2+2
# cutom colors
i <- list(line = list(color = '#FFD700'))
d <- list(line = list(color = '#0000ff'))

p <- df %>%
  plot_ly(x = ~Date, type="candlestick",
          open = ~AAPL.Open, close = ~AAPL.Close,
          high = ~AAPL.High, low = ~AAPL.Low,
          increasing = i, decreasing = d)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="finance/candlestick-colors")
chart_link


