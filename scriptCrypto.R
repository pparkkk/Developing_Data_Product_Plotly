setwd("/Users/Jakraya/Desktop/R/Statistics_and_Machine_Learning/Developing_Data_Products/Developing_Data_Product_Plotly")

# load the data
if (!file.)
    data <- read.csv("crypto-markets.csv")

# subset only the top 5 currencies by price as of 10 September 2020 
# and available on this data set
topcur <- c("Bitcoin", "Maker", "Ethereum", "Bitcoin Cash", 
            "Bitcoin SV")
top5 <- data[which(data$name %in% topcur),]
top5$date <- as.Date(top5$date)
crypto <- unique(top5$name)
str(top5)

# create a list of data frames, each contains one of the top 5 
# currencies' data
df <- list()
for (i in 1:length(crypto)) {
    df[[i]] <- top5[which(top5$name == crypto[i]),]
}

# select only some columns : name = 3, date = 4, open = 6
for (i in 1:length(df)) {
    df[[i]] <- df[[i]][,c(3,4,6)]
}

# rename the columns' names
for (i in 1:length(df)) {
    name <- unique(df[[i]][,1])
    open <- paste(name, ".", "open", sep = "")
    names(df[[i]]) <- c(name, "Date", open)
}

# combine all into 1 data frame
topfive <- data.frame(Date = df[[1]]$Date,
                      Bitcoin = NA,
                      Ethereum = NA,
                      Bitcoin.Cash = NA,
                      Bitcoin.SV = NA,
                      Maker = NA)

for (i in 1:length(df)) {
    # identify which row to start/end
    tempdata <- df[[i]]
    startDate <- tempdata[1,2]
    startRow <- which(topfive$Date == startDate)
    endRow <- startRow + dim(tempdata)[1] - 1
    # replace with data
    topfive[startRow:endRow,i+1] <- tempdata[,3]
}

# plot
library(plotly)
plot_ly(topfive, x = ~Date, y = ~Bitcoin, type = "scatter", mode = 
            "lines", name = "Bitcoin") %>%
    add_trace(y = ~Ethereum, name = "Ethereum") %>%
    add_trace(y = ~Bitcoin.Cash, name = "Bitcoin Cash") %>%
    add_trace(y = ~Bitcoin.SV, name = "Bitcoin SV") %>%
    add_trace(y = ~Maker, name = "Maker") %>%
    layout(xaxis = list(title = "Date"),
           yaxis = list(title = "Daily Open Prices (USD)"),
           title = "Daily Open Prices for the 5 Most Expensive Cryptocurrencies")



