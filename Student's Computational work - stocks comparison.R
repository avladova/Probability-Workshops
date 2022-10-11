#Import all five tickers in R.
Tickers <-
  read.table(file = 'desktop/Tickers.txt', header = TRUE, sep = ",")

#Select the output range accrodung in the number of rows to avoid Error "[ reached 'max' / getOption("max.print") -- omitted 197 rows ]".
options(max.print = 11880)

#Input of the table.
Tickers

#Counting the log returns for close prices by adding additional column.
Tickers$LOGReturns = c(NA, diff(log(Tickers$CLOSE)))

#Rounding up the values of log returns up to 5 digits.
Tickers$RoundedLOGReturns = round(Tickers$LOGReturns, 5)

library(pastecs)
#ФосАгро-ао descriptive statisctics.
stat.desc(Tickers$LOGReturn[1:236], norm = TRUE)

#МТС-ао descriptive statisctics.
stat.desc(Tickers$LOGReturn[237:472], norm = TRUE)

#Лукойл descriptive statisctics.
stat.desc(Tickers$LOGReturn[473:708], norm = TRUE)

#Магнит-ао descriptive statisctics.
stat.desc(Tickers$LOGReturn[709:944], norm = TRUE)

#Инград-ао descriptive statisctics.
stat.desc(Tickers$LOGReturn[945:1178], norm = TRUE)

#Statistical histogram of ФосАгро-ао log returns.

#A rule of thumb (known as Sturges’ law) is that the number of bins (breaks function) should be the rounded value of the square root of the number of observations.
hist(
  Tickers$LOGReturn[1:236],
  breaks = 15,
  main = "ФосАгро-ao",
  xlab = "Interval [1:236]",
  ylab = ""
)

#Statistical histogram of МТС-ао log returns.
hist(
  Tickers$LOGReturn[237:472],
  breaks = 15,
  main = "МТС-ао",
  xlab = "Interval [237:472]",
  ylab = ""
)

#Statistical histogram of Лукойл log returns.
hist(
  Tickers$LOGReturn[473:708],
  breaks = 15,
  main = "Лукойл",
  xlab = "Interval [473:708]",
  ylab = ""
)

#Statistical histogram of Магнит-ао log returns.
hist(
  Tickers$LOGReturn[709:944],
  breaks = 15,
  main = "Магнит-ао",
  xlab = "Interval [709:944]",
  ylab = ""
)

#Statistical histogram of Инград-ао log returns.
hist(
  Tickers$LOGReturn[945:1178],
  breaks = 15,
  main = "Инград-ао",
  xlab = "Interval [945:1178]",
  ylab = ""
)

#Line plots, particularly useful in time series or finance.

#Time plot of ФосАгро-ао log returns.
plot(
  Tickers$LOGReturn[1:236],
  type = "l",
  main = "ФосАгро-ao",
  xlab = "Interval [1:236]",
  ylab = "",
  
)

#Time plot of МТС-ао log returns.
plot(
  Tickers$LOGReturn[237:472],
  type = "l",
  main = "МТС-ао",
  xlab = "Interval [237:472]",
  ylab = ""
)

#Time plot of Лукойл log returns.
plot(
  Tickers$LOGReturn[473:708],
  type = "l",
  main = "Лукойл",
  xlab = "Interval [473:708]",
  ylab = ""
)

#Time plot of Магнит-ао log returns.
plot(
  Tickers$LOGReturn[709:944],
  type = "l",
  main = "Магнит-ао",
  xlab = "Interval [709:944]",
  ylab = ""
)

#Time plot of Инград-ао log returns.
plot(
  Tickers$LOGReturn[945:1178],
  type = "l",
  main = "Инград-ао",
  xlab = "Interval [945:1178]",
  ylab = ""
)

#A boxplot (and whisker) graphically represents the distribution of a quantitative variable by visually displaying five common location summary (minimum, median, first/third quartiles and maximum) and any observation that was classified as a suspected outlier using the interquartile range (IQR) criterion.

#Box and whisker plot of ФосАгро-ао log returns.
boxplot(Tickers$LOGReturn[1:236],
        main = "ФосАгро-ao",
        xlab = "Interval [1:236]",
        ylab = "")

#Box and whisker plot of МТС-ао log returns.
boxplot(Tickers$LOGReturn[237:472],
        main = "МТС-ао",
        xlab = "Interval [237:472]",
        ylab = "")

#Box and whisker plot of Лукойл log returns.
boxplot(Tickers$LOGReturn[473:708],
        main = "Лукойл",
        xlab = "Interval [473:708]",
        ylab = "")

#Box and whisker plot of Магнит-ао log returns.
boxplot(Tickers$LOGReturn[709:944],
        main = "Магнит-ао",
        xlab = "Interval [709:944]",
        ylab = "")

#Box and whisker plot of Инград-ао log returns.
boxplot(Tickers$LOGReturn[945:1178],
        main = "Инград-ао",
        xlab = "Interval [945:1178]",
        ylab = "")

#Japanese Candlestick chart of ФосАгро-ао in one candle as an example.
library(plotly)

fig <- plot_ly(
  x = "Date",
  data = Tickers,
  type = "candlestick",
  open = ~ OPEN[212:234],
  close = ~ CLOSE[212:234],
  high = ~ HIGH[212:234],
  low = ~ LOW[212:234]
) %>% layout(title = 'ФосАгро-ао')
fig

library(quantmod)
#Japanese Candlestick chart of ФосАгро-ао using yahoo finance.
getSymbols("PHOR.ME", src = 'yahoo')
df <- data.frame(Date = index(PHOR.ME), coredata(PHOR.ME))
df <- tail(df, 365)

fig <- df %>% plot_ly(
  x = ~ Date,
  type = "candlestick",
  open = ~ PHOR.ME.Open,
  close = ~ PHOR.ME.Close,
  high = ~ PHOR.ME.High,
  low = ~ PHOR.ME.Low
)
fig <- fig %>% layout(title = "PHOR Candlestick Chart",
                      xaxis = list(rangeslider = list(visible = F)))

fig

#Or we can even upgrade our graph by using segments to understand and see more data.
phor <- getSymbols("PHOR.ME", auto.assign = F)
dat1 <- as.data.frame(phor)
dat1$date <- index(phor)
dat1 <- subset(dat1, date >= "2021-08-01")

names(dat1) <- sub("^PHOR.ME\\.", "", names(dat1))

fig <- plot_ly(
  dat1,
  x = ~ date,
  xend = ~ date,
  color = ~ Close > Open,
  colors = c("red", "forestgreen"),
  hoverinfo = "none"
)
fig <- fig %>% add_segments(y = ~ Low,
                            yend = ~ High,
                            size = I(1))
fig <- fig %>% add_segments(y = ~ Open,
                            yend = ~ Close,
                            size = I(3))
fig <-
  fig %>% layout(
    showlegend = FALSE,
    yaxis = list(title = "Price"),
    title = "PHOR.ME"
  )
fig <- fig %>% rangeslider()

fig

#Now I will provide you with other Candlestic charts using plotly and quantmod packages.

#Japanese Candlestick chart of МТС-ао using yahoo finance.
mts <- getSymbols("MTSS.ME", auto.assign = F)
dat2 <- as.data.frame(mts)
dat2$date <- index(mts)
dat2 <- subset(dat2, date >= "2021-08-01")

names(dat2) <- sub("^MTSS.ME\\.", "", names(dat2))

fig <- plot_ly(
  dat2,
  x = ~ date,
  xend = ~ date,
  color = ~ Close > Open,
  colors = c("blue", "black"),
  hoverinfo = "none"
)
fig <- fig %>% add_segments(y = ~ Low,
                            yend = ~ High,
                            size = I(1))
fig <- fig %>% add_segments(y = ~ Open,
                            yend = ~ Close,
                            size = I(3))
fig <-
  fig %>% layout(
    showlegend = FALSE,
    yaxis = list(title = "Price"),
    title = "MTSS.ME"
  )
fig <- fig %>% rangeslider()

fig

#Japanese Candlestick chart of Лукойл using yahoo finance.
lukoil <- getSymbols("LKOH.ME", auto.assign = F)
dat3 <- as.data.frame(lukoil)
dat3$date <- index(lukoil)
dat3 <- subset(dat3, date >= "2021-08-01")

names(dat3) <- sub("^LKOH.ME\\.", "", names(dat3))

fig <- plot_ly(
  dat3,
  x = ~ date,
  xend = ~ date,
  color = ~ Close > Open,
  colors = c("violet", "green"),
  hoverinfo = "none"
)
fig <- fig %>% add_segments(y = ~ Low,
                            yend = ~ High,
                            size = I(1))
fig <- fig %>% add_segments(y = ~ Open,
                            yend = ~ Close,
                            size = I(3))
fig <-
  fig %>% layout(
    showlegend = FALSE,
    yaxis = list(title = "Price"),
    title = "LKOH.ME"
  )
fig <- fig %>% rangeslider()

fig

#Japanese Candlestick chart of Магнит-ао using yahoo finance.
mgnt <- getSymbols("MGNT.ME", auto.assign = F)
dat4 <- as.data.frame(mgnt)
dat4$date <- index(mgnt)
dat4 <- subset(dat4, date >= "2021-08-01")

names(dat4) <- sub("^MGNT.ME\\.", "", names(dat4))

fig <- plot_ly(
  dat4,
  x = ~ date,
  xend = ~ date,
  color = ~ Close > Open,
  colors = c("pink", "yellow"),
  hoverinfo = "none"
)
fig <- fig %>% add_segments(y = ~ Low,
                            yend = ~ High,
                            size = I(1))
fig <- fig %>% add_segments(y = ~ Open,
                            yend = ~ Close,
                            size = I(3))
fig <-
  fig %>% layout(
    showlegend = FALSE,
    yaxis = list(title = "Price"),
    title = "MGNT.ME"
  )
fig <- fig %>% rangeslider()

fig

#Japanese Candlestick chart of Инград-ао using yahoo finance.
ingrad <- getSymbols("INGR.ME", auto.assign = F)
dat5 <- as.data.frame(ingrad)
dat5$date <- index(ingrad)
dat5 <- subset(dat5, date >= "2021-08-01")

names(dat5) <- sub("^INGR.ME\\.", "", names(dat5))

fig <- plot_ly(
  dat5,
  x = ~ date,
  xend = ~ date,
  color = ~ Close > Open,
  colors = c("brown", "red"),
  hoverinfo = "none"
)
fig <- fig %>% add_segments(y = ~ Low,
                            yend = ~ High,
                            size = I(1))
fig <- fig %>% add_segments(y = ~ Open,
                            yend = ~ Close,
                            size = I(3))
fig <-
  fig %>% layout(
    showlegend = FALSE,
    yaxis = list(title = "Price"),
    title = "INGR.ME"
  )
fig <- fig %>% rangeslider()

fig

#According to statisticals histogramms of my 5 assets I am going to describe them.
#The histogram displays a symmetrical distribution of data. A distribution is symmetrical if a vertical line can be drawn at some point in the histogram such that the shape to the left and the right of the vertical line are mirror images of each other.
#So, accrodung to my histogramms none of them are symmetric.
#The direction of skewness is “to the tail.” The larger the number, the longer the tail. If skewness is positive, the tail on the right side of the distribution will be longer. If skewness is negative, the tail on the left side will be longer.
#As for the PhosAgro it is skewed for 2.04; it is extreme skew to the right because of big positive values.
#As for the MTS it is skewed for -1.46; it is extreme skew to the left because of big negative values.
#As for the Lukoil it is skewed for 1.46; it is extreme skew to the right because of big positive values.
#As for the Magnit it is skewed for -2.68; it is extreme skew to the left because of big negative values.
#As for the Ingrad it is skewed for -1.41; it is extreme skew to the left because of big negative values.
#Now I will note whether the histogramms are bell-shaped or not.
#As for the PhosAgro the situation is rather interesting as it seems to be bell-shaped on the first view. However, because of two small bars on the right side we can understand that it has outliers, so it cannot be named as bell-shaped. It is arbitrary
#As for the MTS the situation is the same. That is why it is arbitrary.
#As for the Lukoil the situation is the same. Arbitrary.
#As for the Magnit it is arbitrary.
#As for the Ingrad it is arbitrary.

#Conditional formatting is not available for imported data, so I will do the matrix of logreturns myself to use formattable package.
library(formattable)
LOGReturnsMatrix = matrix(c(paste(Tickers$LOGReturns)),
                          nrow = 1178,
                          ncol = 1,
                          byrow = TRUE)
colnames(LOGReturnsMatrix) = c("LOGReturns1")
set.seed(123)
LOGReturnsMatrix <-
  data.frame(id = 1:1178, LOGReturns1 = rnorm(1178))
formattable(LOGReturnsMatrix, list(area(col = LOGReturns1) ~ color_tile("transparent", "red")))

#Now I will create the scatter plots for the pairs of log-returns (1,2)
dat6 <- data.frame(Tickers$LOGReturns[1:236])
dat67 <-
  data.frame(Tickers$LOGReturns[1:236], Tickers$LOGReturns[237:472])
plot(
  Tickers$LOGReturns[1:236],
  Tickers$LOGReturns[237:472],
  main = "Ticker 1 and 2",
  xlab = "PhosAgro",
  ylab = "MTS",
  pch = 19,
  frame = FALSE,
  xlim = c(-0.1, 0.1),
  ylim = c(-0.5, 0.5),
  mtext("cor=-0.01834003", side = 3)
)
lines(lowess(Tickers$LOGReturns[1:236] ~ Tickers$LOGReturns[237:472]), col = "blue")

#(2,3)
dat8 <- data.frame(Tickers$LOGReturns[237:472])
dat89 <-
  data.frame(Tickers$LOGReturns[237:472], Tickers$LOGReturns[473:708])
plot(
  Tickers$LOGReturns[237:472],
  Tickers$LOGReturns[473:708],
  main = "Ticker 2 and 3",
  xlab = "MTS",
  ylab = "Lukoil",
  pch = 19,
  frame = FALSE,
  xlim = c(-0.1, 0.1),
  ylim = c(-0.5, 0.5),
  mtext("cor=-0.9632484", side = 3)
)
lines(lowess(Tickers$LOGReturns[237:472] ~ Tickers$LOGReturns[473:708]), col = "green")

#(3,4)
dat10 <- data.frame(Tickers$LOGReturns[473:708])
dat1011 <-
  data.frame(Tickers$LOGReturns[473:708], Tickers$LOGReturns[709:944])
plot(
  Tickers$LOGReturns[473:708],
  Tickers$LOGReturns[709:944],
  main = "Ticker 3 and 4",
  xlab = "Lukoil",
  ylab = "Magnit",
  pch = 19,
  frame = FALSE,
  xlim = c(-0.1, 0.1),
  ylim = c(-0.5, 0.5),
  mtext("cor=0.4308692", side = 3)
)
lines(lowess(Tickers$LOGReturns[473:708] ~ Tickers$LOGReturns[709:944]), col = "red")

#(4,5)
#Because of the Error in data.frame(Tickers$LOGReturns[709:944], Tickers$LOGReturns[945:1178]) : arguments imply differing number of rows: 236, 234; I will just exclude 2 last values from the bigger amount
dat12 <- data.frame(Tickers$LOGReturns[945:1180])
dat1213 <-
  data.frame(Tickers$LOGReturns[709:944], Tickers$LOGReturns[945:1180])
plot(
  Tickers$LOGReturns[709:944],
  Tickers$LOGReturns[945:1180],
  main = "Ticker 4 and 5",
  xlab = "Magnit",
  ylab = "Ingrad",
  pch = 19,
  frame = FALSE,
  xlim = c(-0.1, 0.1),
  ylim = c(-0.5, 0.5),
  mtext("cor=-0.2592013", side = 3)
)
lines(lowess(Tickers$LOGReturns[709:942] ~ Tickers$LOGReturns[945:1178]), col = "orange")

#Removing the outliers. The one method that I prefer uses the boxplot() function to identify the outliers and the which() function to find and remove them from the dataset. I will fo it for Ticker 5, because by checking up my Ticker 1, I made myself sure that there are no outliers.
dat14 <- data.frame(Tickers$CLOSE[945:1180])
boxplot(dat14, plot = FALSE)$out
outliers <- boxplot(dat14, plot = FALSE)$out
dat14 <- dat14[-which(Tickers$CLOSE[945:1180] %in% outliers),]

#So now we can see that dat14 is cleared from all outliers and we can build up statistical histogramms to make a comparison.
hist(dat14, main = "Ingrad without outliers")
plot(dat14, main = "Ingrad without outliers", type = "l")

#It is obvious that by cleaning up the data, the graph's or schedule's image becomes considerably more accurate at trend prediction. The graph is less spikey as a result of the reduction of distortions.

#Now I need to count the correlation. cor() computes the correlation coefficient; cor.test() test for association/correlation between paired samples. It returns both the correlation coefficient and the significance level(or p-value) of the correlation. Consequently, I will use the second function to define the significance of correlation.
correlation12 <-
  cor.test(
    Tickers$LOGReturns[1:236],
    Tickers$LOGReturns[237:472],
    method = c("pearson", "kendall", "spearman")
  )
correlation12$p.value
#From the output, the p-value is greater than the significance level 0.05 implying that the distribution of the data is not significantly different from normal distribution. In other words, we can assume the normality.

correlation12$estimate
#Correlation coefficient is comprised between -1 and 1: -1 indicates a strong negative correlation : this means that every time x increases, y decreases; 0 means that there is no association between the two variables; 1 indicates a strong positive correlation : this means that y increases with x.

correlation23 <-
  cor.test(
    Tickers$LOGReturns[237:472],
    Tickers$LOGReturns[473:708],
    method = c("pearson", "kendall", "spearman")
  )
correlation23$estimate

correlation34 <-
  cor.test(
    Tickers$LOGReturns[473:708],
    Tickers$LOGReturns[709:944],
    method = c("pearson", "kendall", "spearman")
  )
correlation34$estimate

#Because of Error in cor.test.default(Tickers$LOGReturns[709:944], Tickers$LOGReturns[945:1178],  : 'x' and 'y' must have the same length, I will use one more function to fix it.
correlation45 <-
  cor.test(
    Tickers$LOGReturns[709:944],
    Tickers$LOGReturns[945:1180],
    method = c("pearson", "kendall", "spearman"),
    use = "complete.obs"
  )
correlation45$estimate
#In our case the number for correlation between 1,2 and 4,5 tickers are closer to 0 than to plus one or minus one therefore the indicators indicate that there is close to no-correlation. However, the numbers of 2,3 and 3,4 tickers show us strong negative correlation and a positive correlation respectively.

#Creating the correlation matrix for 5 ticker closing prices.
library(tibble)
library(dplyr)
Tickers[nrow(Tickers) + 2,] <- NA
ClosingPricesByColumns <-
  data.frame(Tickers$CLOSE[1:236],
             Tickers$CLOSE[237:472],
             Tickers$CLOSE[473:708],
             Tickers$CLOSE[709:944],
             Tickers$CLOSE[945:1180])
ClosingPricesByColumns.cor = cor(ClosingPricesByColumns)
#The strongest correlation is -0.85<...>, it is negative and it is between PhosAgro and Lukoil.
#The weakest correlation is -0.60<...>, it is negative and it is between PhosAgro and Magnit.

#Now I will construct the scatter plots for thу assets with the strongest and the weakest correlations.
#Firstly, it will be a scatter plot of PhosAgro and Magnit.
plot(
  Tickers$LOGReturns[1:236],
  Tickers$LOGReturns[709:944],
  main = "The Weakest Cor",
  xlab = "PhosAgro",
  ylab = "MAgnit",
  pch = 19,
  frame = FALSE,
  xlim = c(-0.1, 0.1),
  ylim = c(-0.5, 0.5),
  mtext("cor=-0.60", side = 3)
)
lines(lowess(Tickers$LOGReturns[1:236] ~ Tickers$LOGReturns[709:944]), col = "purple")

#Secondly, it will be a scatter plot of PhosAgro and Lukoil.
plot(
  Tickers$LOGReturns[1:236],
  Tickers$LOGReturns[473:708],
  main = "The Weakest Cor",
  xlab = "PhosAgro",
  ylab = "Lukoil",
  pch = 19,
  frame = FALSE,
  xlim = c(-0.1, 0.1),
  ylim = c(-0.5, 0.5),
  mtext("cor=-0.60 ", side = 3)
)
lines(lowess(Tickers$LOGReturns[1:236] ~ Tickers$LOGReturns[473:708]), col = "green")
#According to our plot we can easily notice the correlation significance.
