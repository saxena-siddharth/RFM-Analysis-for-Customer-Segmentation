##### Parameters
date <-  Sys.Date() -1

##### Library
library(RJDBC)
library(rfm)
library(dplyr)

##### Import dataset
data <- read.csv("../data/SampleData.csv")
head(data)

##### Converting date into yyyy-mm-dd format
data$SAORDT <- as.Date(paste(substr(data$SAORDT,1,4), substr(data$SAORDT,5,6), substr(data$SAORDT,7,8), sep="-"))

##### Descriptive Statistics
summary(data$SAITOT)
names(data)
nrow(data)
ncol(data) 
str(data)
head(data)
tail(data)
colSums(is.na(data)) #missing values

##### Creating a subset
datatest <- data[, c("SAOPT.","SAORDT","SAITOT")]
colnames(datatest) <- c("Client","Date","Amount")
min(datatest$Date)
max(datatest$Date)
head(datatest)

##### Calculating IQR range
iqr <- IQR(datatest$Amount)
iqr
Q <- quantile(datatest$Amount, probs=c(.25, .75), na.rm = FALSE)
c(Q[2], Q[1])
up <-  Q[2]+1.5*iqr
low<- Q[1]-1.5*iqr 
c(up, low)

##### Eliminating the outliers
eliminated <- subset(datatest, datatest$Amount > (Q[1] - 1.5*iqr) & datatest$Amount < (Q[2]+1.5*iqr))
eliminated <- eliminated[eliminated$Client!=0, ]
analysis_date <- Sys.Date()
asc_eliminated <-  eliminated[order(eliminated$Amount), ]
View(asc_eliminated)


##### RFM Analysis
rfm_result <- rfm_table_order(eliminated, Client, Date, Amount, analysis_date)
rfm_result
colnames(rfm_result)
ncol(rfm_result)

answer <- rfm_result$rfm
View(answer)


answer$RScore.HL[answer$recency_score > mean(answer$recency_score)] <- 'H'
answer$RScore.HL[answer$recency_score <= mean(answer$recency_score)] <- "L"
answer$FScore.HL[answer$frequency_score > mean(answer$frequency_score)] <- 'H'
answer$FScore.HL[answer$frequency_score <= mean(answer$frequency_score)] <- "L"
answer$MScore.HL[answer$monetary_score > mean(answer$monetary_score)] <- 'H'
answer$MScore.HL[answer$monetary_score <= mean(answer$monetary_score)] <- "L"

answer$RFM <- paste(answer$RScore.HL,answer$FScore.HL,answer$MScore.HL,sep="")

answer$RFM_Segment[answer$RFM%in%c("HHH")] <- "Best"
answer$RFM_Segment[answer$RFM%in%c("HHL")] <- "Upsell"
answer$RFM_Segment[answer$RFM%in%c("HLH")] <- "Recruit"
answer$RFM_Segment[answer$RFM%in%c("LHH")] <- "Winback Best"
answer$RFM_Segment[answer$RFM%in%c("HLL")] <- "Retain"
answer$RFM_Segment[answer$RFM%in%c("LHL")] <- "Winback"
answer$RFM_Segment[answer$RFM%in%c("LLH")] <- "Reactivate"
answer$RFM_Segment[answer$RFM%in%c("LLL")] <- "Uncertain"

write.csv(answer,file="../output/Customer_Segmentation.csv",row.names=F)

##### Data Exploration

rfm_heatmap(rfm_result) #to show average monetary scores for different categories of R & F scores
rfm_bar_chart(rfm_result) #to generate distribution of monetary scores for combinations of F & R scores
rfm_histograms(rfm_result) #to examine the relative distribution of R, F & M
rfm_rm_plot(rfm_result) #Recency vs Monetary plot
rfm_fm_plot(rfm_result) #Frequency vs Monetary plot
rfm_rf_plot(rfm_result) #Recency vs Frequency plot
rfm_order_dist(rfm_result) #Customers by number of orders

