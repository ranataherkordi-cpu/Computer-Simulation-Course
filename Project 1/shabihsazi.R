library(tseries)
library(forecast)
library(ggplot2)
library(lubridate)

prepare_data <- function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # پاکسازی نام ستون‌ها
  colnames(df) <- gsub("^X\\.", "", colnames(df))  # حذف X. اول اسم
  colnames(df) <- gsub("\\.$", "", colnames(df))   # حذف . آخر اسم
  
  # تبدیل ستون DTYYYYMMDD به تاریخ
  df$Date <- ymd(as.character(df$DTYYYYMMDD))
  
  # مرتب‌سازی بر اساس تاریخ
  df <- df[order(df$Date), ]
  df
}

extract_numeric <- function(df) {
  cols <- setdiff(names(df), c("DTYYYYMMDD", "Date"))
  Filter(function(col) is.numeric(df[[col]]), cols)
}

plot_series <- function(df, var) {
  ggplot(df, aes(x = Date, y = .data[[var]])) +
    geom_line(color = "dodgerblue4") +
    ggtitle(paste("Series:", var)) +
    xlab(NULL) + ylab(NULL) +
    theme_bw()
}

plot_monthly_avg <- function(df, var) {
  df$Month <- month(df$Date, label = TRUE)
  ggplot(df, aes(x = Month, y = .data[[var]])) +
    stat_summary(fun = mean, geom = "bar", fill = "dodgerblue4") +
    ggtitle(paste("Monthly Average of", var)) +
    theme_linedraw()
}

model_and_forecast <- function(ts_obj, label) {
  arima_fit <- auto.arima(ts_obj)
  print(summary(arima_fit))
  future <- forecast(arima_fit, h = 31)
  autoplot(future) +
    ggtitle(paste("Forecast for", label)) +
    theme_minimal()
}

run_time_series_analysis <- function(df) {
  vars <- extract_numeric(df)
  for (v in vars) {
    print(plot_series(df, v))
    
    ts_obj <- ts(df[[v]], frequency = 365)
    
    cat("ADF test:\n")
    print(adf.test(ts_obj))
    cat("PP test:\n")
    print(pp.test(ts_obj))
    
    print(plot_monthly_avg(df, v))
    
    if (ndiffs(ts_obj) > 0) ts_obj <- diff(ts_obj)
    
    print(model_and_forecast(ts_obj, v))
  }
}

analyze_correlations <- function(df) {
  variables <- c("tm", "tmax", "tmin")
  for (v in variables) {
    corr_val <- cor(df[[v]], df$rain, use = "complete.obs")
    cat(sprintf("Correlation between %s and rain: %.2f\n", v, corr_val))
    
    formula <- as.formula(paste("rain ~", v))
    fit <- lm(formula, data = df)
    
    print(summary(fit))
  }
}


# Load ETF data
etf_df <- prepare_data("Lotus Gold Com.ETF.csv")
run_time_series_analysis(etf_df)

# Load weather data
weather_df <- read.csv("Tehran (Mehrabad Airport).csv")
analyze_correlations(weather_df)
