library('ggplot2')
library('forecast')
 library('tseries')

daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)

daily_data$Date = as.Date(daily_data$dteday)

ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month') + ylab("Daily Bike Checkouts") +
        xlab("")

count_ts = ts(daily_data[, c('cnt')])

daily_data$clean_cnt = tsclean(count_ts)

ggplot() +
        geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')

daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)

daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)


ggplot() +
        geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
        geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
        geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
        ylab('Bicycle Count')


#First, we calculate seasonal component of the data using stl(). STL is a flexible 
#function for decomposing and forecasting the series. It calculates the seasonal component of 
#the series using smoothing, and adjusts the original series by subtracting 
#seasonality in two simple lines:
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

