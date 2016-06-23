#install.packages("quantmod")
library("quantmod")

# We make sure to install and import library needed to 
# easily manage dates
install.packages("lubridate")
library("lubridate")


library(ggplot2)
library(xts)
#install.packages('dygraphs')
library(dygraphs)

# We set the initial date we will use for analysing 
# the data
startDate = as.Date("2009-01-01")

# We set the final date we will use for analysing 
# the data
endDate = as.Date("2015-12-31") 


# Retrieving Apple’s daily stock market prices from Yahoo Finance
getSymbols("GSPC", src = "yahoo", from = startDate, to = endDate)

# We generate a chart
chartSeries(GSPC)
# We save pic to file
dev.copy(png, file="GSPC_chart.png", height=500, width=500)
## and switch off the device
dev.off()

# We turn our data to a data frame 
df.GSPC<-data.frame(Date=index(GSPC),coredata(GSPC))

# Exploring columns of data frame
names(df.GSPC)
head(df.GSPC,5)

# We rename column for easier access
names(df.GSPC)[2]<-"Open"
names(df.GSPC)[3]<-"High"
names(df.GSPC)[4]<-"Low"
names(df.GSPC)[5]<-"Close"
names(df.GSPC)[6]<-"Volume"
names(df.GSPC)[7]<-"Adjusted"

# Checking changes
names(df.GSPC)

df.GSPC <- na.omit(df.GSPC)

plot(df.GSPC$Date,df.GSPC$Open,type="l")

df.GSPC$DailyReturn<-NULL
df.GSPC$DailyReturn <- (df.GSPC$Close) # - df.GSPC$Open) #/
   # df.GSPC$Open

#plot(df.GSPC$Date,df.GSPC$DailyReturn,type="l")
plot(df.GSPC$DailyReturn,type="l")

tail(df.GSPC$DailyReturn)
#rm(n)
n <- nrow(df.GSPC)
df.GSPC$DailyReturn[n]
lastGSPC <- df.GSPC$Close[n]


df.GSPC$FractReturn <- NULL
for (k in 2:n){
    df.GSPC$FractReturn[k] = df.GSPC$DailyReturn[k]/
        df.GSPC$DailyReturn[k-1]
}

head(df.GSPC)
df.GSPC <- na.omit(df.GSPC)
head(df.GSPC)

plot(df.GSPC$Date,df.GSPC$FractReturn,type="l")

df.GSPC$HistScenarios <- NULL
lastGSPC
df.GSPC$HistScenarios <- df.GSPC$FractReturn*lastGSPC
head(df.GSPC)

df.GSPC$EmpiricalDistr <- NULL

df.GSPC$EmpiricalDistr <- df.GSPC$HistScenarios - lastGSPC

head(df.GSPC)
tail(df.GSPC)



EmpiricalDistrOrdered <- sort(df.GSPC$EmpiricalDistr)

quantile(df.GSPC$EmpiricalDistr,c(0.05,0.95))
quantile(EmpiricalDistrOrdered,c(0.05,0.95))

hist(df.GSPC$EmpiricalDistr,breaks = 200)
# We save pic to file
dev.copy(png, file="GSPC_hist.png", height=500, width=500)
## and switch off the device
dev.off()

quantile(sorted_dailyReturn,c(0.05,0.95))
max(sorted_dailyReturn)
min(sorted_dailyReturn)

sorted_dailyReturn<-sort(df.GSPC$DailyReturn,decreasing = FALSE)
plot(sorted_dailyReturn,type="l",col='black')
abline(h=-0.349995,col='red')


plot(df.GSPC$DailyReturn,type="l",col='black')
abline(h=0.32000,col='green')
abline(h=-0.349995,col='red')
# We save pic to file
dev.copy(png, file="simple_VaR_plot.png", height=500, width=500)
## and switch off the device
dev.off()



#nrow(df.GSPC)


hist_GSPD<-hist(df.GSPC$DailyReturn,plot=TRUE)
ls(hist_GSPD)
hist_GSPD$density


# Important references
# http://www.r-bloggers.com/historical-value-at-risk-versus-historical-expected-shortfall/
# http://www.r-tutor.com/elementary-statistics/numerical-measures/percentile
# http://www.r-bloggers.com/basics-of-histograms/
