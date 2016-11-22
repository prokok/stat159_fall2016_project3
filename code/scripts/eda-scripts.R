#setwd("C:/Users/vlfgn/Desktop/Clean/stat159_fall2016_project3/code/scripts")
#Loading the raw dataset
#Focus on the 4 year universities(high overall graduation rates)

dat = read.csv("../../data/subset-data.csv", stringsAsFactors = FALSE)

#Subsetting by 4-year school(Variable name: CCUGPROF > 4) stored in dat_4
dat_4=subset(dat, CCUGPROF>4)

#Using POOLED completion rate as our criteria. 
#If there are NA values in POOLED completion rate, not going to use those universities.
#is.na(dat1$C150_4_POOLED)==is.na(dat1$C150_4) used this to check
#list of universities does have C150_4_POOLED
dat_4$INSTNM[which(!is.na(dat_4$C150_4_POOLED))]
length(dat_4$INSTNM[which(!is.na(dat_4$C150_4_POOLED))])#2484
#list of universities where C150_4_POOLED ==0
dat_4$INSTNM[which(dat_4$C150_4_POOLED==0)]
length(dat_4$INSTNM[which(dat_4$C150_4_POOLED==0)])#15


#Data for our graduation rate in 4 year universities undergraduate.(excluding NAs and 0s)
completion_4 = dat_4[which(!is.na(dat_4$C150_4_POOLED)), ]
completion_4 = completion_4[which(completion_4$C150_4_POOLED!=0),]


#histogram for completion rate of 4 year universities.
x = completion_4$C150_4_POOLED * 100

png("../../images/histogram of completion rate.png", width=800, height=600)
h=hist(x, breaks = 20, col = '#5679DF'
     , xlab = "Percentage Rate(percentage)"
     , main = "Completion Rate in 4 year university(undergraduate)"
     , xlim = c(0,100), ylim = c(0,260))
xfit = seq(0,100,length=100) 
yfit1 = dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit1 = yfit1*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit1, col="red", lwd=2)
dev.off()


###Remeber, we are using dat1 = dat_4#########################################
###Let dat1=dat_4 for frequent use
dat1=dat_4

###################################################################################
############################Complemtion Rate by Demographics#######################
#C150_4_WHITE
##Completion rate for first-time, full-time students at four-year institutions
##(150% of expected time to completion/6 years) for white students
#C150_4_BLACK
##Completion rate for first-time, full-time students at four-year institutions
##(150% of expected time to completion/6 years) for black students
#C150_4_HISP
##Completion rate for first-time, full-time students at four-year institutions
##(150% of expected time to completion/6 years) for Hispanic students
#C150_4_ASIAN
##Completion rate for first-time, full-time students at four-year institutions
##(150% of expected time to completion/6 years) for Asian students

#The number of NAs in each demographic completions rate AMONG 2740 observations.
sum(as.numeric(is.na(dat1$C150_4_WHITE))) #388
sum(as.numeric(is.na(dat1$C150_4_BLACK))) #551
sum(as.numeric(is.na(dat1$C150_4_HISP))) #535
sum(as.numeric(is.na(dat1$C150_4_ASIAN))) #879


#Eexclude NA's and 0's in demographic completions(WHITE,BLACK,HISPANIC,ASIAN)
names_demo = c("C150_4_WHITE","C150_4_BLACK","C150_4_HISP","C150_4_ASIAN")
com_demo = subset(dat1, (!is.na(dat1$C150_4_WHITE)) & (!is.na(dat1$C150_4_BLACK)) &
               (!is.na(dat1$C150_4_HISP)) & (!is.na(dat1$C150_4_ASIAN)))
com_demo = com_demo[which(com_demo$C150_4_WHITE!=0),]
com_demo = com_demo[which(com_demo$C150_4_BLACK!=0),]
com_demo = com_demo[which(com_demo$C150_4_HISP!=0),]
com_demo = com_demo[which(com_demo$C150_4_ASIAN!=0),]

#Summary Statistics of completion rate by Demographics(WHITE, BLACK, HISP, ASIAN)
sink(file = "../../data/eda-output.txt")
cat("1. Explanatory Analysis of Quantative Varibles\n\n")
cat("A. Summary Statistics Of Completion Rate by demographics\n\n")
for(i in 1:length(names_demo))
{
  cat("summary statistics of", names_demo[i], "\n\n")
  cat(summary(com_demo[,names_demo[i]]), "\n")
  cat("Stadard Deviation. : ", sd(com_demo[,names_demo[i]]),"\n")
  cat("Range. : ", max(com_demo[,names_demo[i]])-min(com_demo[,names_demo[i]])," \n")
  cat("IQR. : ", IQR(com_demo[,names_demo[i]]),"\n")
  cat("\n")
}
cat("\n\n")
sink()

##Histogram of completions rate by Demographics(WHITE, BLACK, HISP, ASIAN)
for(i in 1:length(names_demo))
{
  path1 = paste("../../images/histogram-completion-rate",names_demo[i],".png")
  png(filename = path1, width=800, height=600)
  k1 = com_demo[,names_demo[i]]*100
  k2 = hist(k1,breaks= 20, main = paste("Histogram of completion rate of "
            ,names_demo[i]), col = "#5679DF", xlab = names_demo[i])
  xfit = seq(from = 0, to = 100,length=100) 
  yfit = dnorm(xfit,mean=mean(k1),sd=sd(k1)) 
  yfit  =  yfit*diff(k2$mids[1:2])*length(k1) 
  lines(xfit, yfit, col="red", lwd=2)
  dev.off()
}


###################################Demographic Percentage############################
#The number of NAs in each demographic PERCENTAGE AMONG 206 observations.
#We are going to look at first 4 variables.
#Since no data is missing, just going to use dat1.
sum(as.numeric(is.na(dat1$UGDS_WHITE))) #0
sum(as.numeric(is.na(dat1$UGDS_BLACK))) #0
sum(as.numeric(is.na(dat1$UGDS_HISP))) #0
sum(as.numeric(is.na(dat1$UGDS_ASIAN))) #0
sum(as.numeric(is.na(dat1$UGDS_AIAN))) #0
sum(as.numeric(is.na(dat1$UGDS_NHPI))) #0
sum(as.numeric(is.na(dat1$UGDS_2MOR))) #0
sum(as.numeric(is.na(dat1$UGDS_NRA))) #0
sum(as.numeric(is.na(dat1$UGDS_UNKN))) #0

names_demo2 = c("UGDS_WHITE", "UGDS_BLACK", "UGDS_HISP", "UGDS_ASIAN")

#Summary Statistics of Each Demographics(WHITE, BLACK, HISP, ASIAN)
sink(file = "../../data/eda-output.txt", append=TRUE)
cat("B. Summary Statistics Of demographic percentage\n\n")
for(i in 1:length(names_demo2))
{
  cat("summary statistics of", names_demo2[i], "\n\n")
  cat(summary(dat1[,names_demo2[i]]), "\n")
  cat("Stadard Deviation. : ", sd(dat1[,names_demo2[i]]),"\n")
  cat("Range. : ", max(dat1[,names_demo2[i]])-min(dat1[,names_demo2[i]])," \n")
  cat("IQR. : ", IQR(dat1[,names_demo2[i]]),"\n")
  cat("\n")
}
cat("\n\n")
sink()

##Histogram of demographic percentage
for(i in 1:length(names_demo2))
{
  path1 = paste("../../images/histogram-deomgraphic-percentage",names_demo2[i],".png")
  png(filename = path1, width=800, height=600)
  hist(dat1[,names_demo2[i]],breaks= 10, main = paste("Histogram of demograhic percentage of "
          ,names_demo2[i]), col = "#5679DF", xlab = names_demo2[i])
  dev.off()
}

###############################Repayment rate on FAFSA################################
#COMPL_RPY_1YR_RT
##1_yr_repayment.completers
#COMPL_RPY_3YR_RT
##3_yr_repayment.completers
#COMPL_RPY_5YR_RT
##5_yr_repayment.completers
#COMPL_RPY_7YR_RT
##7_yr_repayment.completers

#The number of NAs in Repayment rate on year 1,3,5,7
#ruling out 1_yr_repayment.completers because 
sum(as.numeric(is.na(dat1$COMPL_RPY_1YR_RT))) #206
sum(as.numeric(is.na(dat1$COMPL_RPY_3YR_RT))) #5
sum(as.numeric(is.na(dat1$COMPL_RPY_5YR_RT))) #15
sum(as.numeric(is.na(dat1$COMPL_RPY_7YR_RT))) #18

#Since the column class is character becuase "PrivacySuppressed" in the variables
#Need to convert colums into numeric data.
yr3 = na.omit(as.numeric(dat1$COMPL_RPY_3YR_RT))
yr5 = na.omit(as.numeric(dat1$COMPL_RPY_5YR_RT))
yr7 = na.omit(as.numeric(dat1$COMPL_RPY_7YR_RT))


#Summary Statistics of repayment rate#
sink(file = "../../data/eda-output.txt", append=TRUE)
  cat("C. Summary Statistics Of Repayment rate on FAFSA depending in 3,5,7 year\n\n")
  
  cat("summary statistics of Three year repayment rate for completers\n\n")
  cat(summary(yr3), "\n")
  cat("Stadard Deviation. : ", sd(yr3),"\n")
  cat("Range. : ", max(yr3)-min(yr3)," \n")
  cat("IQR. : ", IQR(yr3),"\n")
  cat("\n")
  
  cat("summary statistics of Five year repayment rate for completers\n\n")
  cat(summary(yr5), "\n")
  cat("Stadard Deviation. : ", sd(yr5),"\n")
  cat("Range. : ", max(yr5)-min(yr5)," \n")
  cat("IQR. : ", IQR(yr5),"\n")
  cat("\n")
  
  cat("summary statistics of Seven year repayment rate for completers\n\n")
  cat(summary(yr7), "\n")
  cat("Stadard Deviation. : ", sd(yr7),"\n")
  cat("Range. : ", max(yr3)-min(yr7)," \n")
  cat("IQR. : ", IQR(yr7),"\n")
  cat("\n")
  
cat("\n\n")
sink()


png(filename = "../../images/histgram of Three year repayment rate for completers.png", width=800, height=600)
hist(yr3, col = "#5679DF", breaks = 10, main = "Histogram of Three year repayment rate for completers")
dev.off()
png(filename = "../../images/histgram of Five year repayment rate for completers.png", width=800, height=600)
hist(yr5, col = "#5679DF", breaks = 10, main = "Histogram of Five year repayment rate for completers")
dev.off()
png(filename = "../../images/histgram of Seven year repayment rate for completers.png", width=800, height=600)
hist(yr7, col = "#5679DF", breaks = 10, main = "Histogram of Seven year repayment rate for completers")
dev.off()


###############################Median earning###################################
#####################Not Available#############################################
##Data exists prior to 2014_2013. Therefore using MERGED2012_13_PP.csv###

dat2 = read.csv("../../data/MERGED2012_13_PP.csv", stringsAsFactors = FALSE, na.strings = 'NULL')
colnames(dat2) = c("UNITID",names(dat2)[-1])

dat_ca_2 = subset(dat2, STABBR == 'CA')
flag=numeric(0)
for(i in 1:length(dat_ca_2$UNITID))
{
  for(j in 1:length(dat1$UNITID))
  {
    if(dat_ca_2$UNITID[i] == dat1$UNITID[j])
    {
      flag[i] = 1
      break
    }
    else{
      flag[i] = 0
    }
  }
}

dat2 = subset(dat_ca_2, flag == 1)

#MD_EARN_WNE_P10
##Median earnings of students working and not enrolled 10 years after entry
#MD_EARN_WNE_P6
##Median earnings of students working and not enrolled 6 years after entry
#MD_EARN_WNE_P8
##Median earnings of students working and not enrolled 8 years after entry


#The number of NAs in Median Earning on year after 6,8,10 years
sum(as.numeric(is.na(dat2$MD_EARN_WNE_P6))) #14
sum(as.numeric(is.na(dat2$MD_EARN_WNE_P8))) #14
sum(as.numeric(is.na(dat2$MD_EARN_WNE_P10))) #14

#Since the column class is character becuase "PrivacySuppressed" in the variables
#Need to convert colums into numeric data.
m6 = na.omit(as.numeric(dat2$MD_EARN_WNE_P6))
m8 = na.omit(as.numeric(dat2$MD_EARN_WNE_P8))
m10 = na.omit(as.numeric(dat2$MD_EARN_WNE_P10))


sink(file = "../../data/eda-output.txt", append=TRUE)
cat("D. Summary Statistics Of Median Earning after 6,8,10 year\n\n")

cat("summary statistics of Median Earning after 6 year\n\n")
cat(summary(m6), "\n")
cat("Stadard Deviation. : ", sd(m6),"\n")
cat("Range. : ", max(m6)-min(m6)," \n")
cat("IQR. : ", IQR(m6),"\n")
cat("\n")

cat("summary statistics of Median Earning after 8 year\n\n")
cat(summary(m8), "\n")
cat("Stadard Deviation. : ", sd(m8),"\n")
cat("Range. : ", max(m8)-min(m8)," \n")
cat("IQR. : ", IQR(m8),"\n")
cat("\n")

cat("summary statistics of Median Earning after 10 year\n\n")
cat(summary(m10), "\n")
cat("Stadard Deviation. : ", sd(m10),"\n")
cat("Range. : ", max(m10)-min(m10)," \n")
cat("IQR. : ", IQR(m10),"\n")
cat("\n")

cat("\n\n")
sink()

png(filename = "../../images/histgram of Median Earning after 6 year(log transfromed).png", width=800, height=600)
hist(log(m6), col = "#5679DF", breaks = 10, main = "Histogram of Median Earning after 6 year"
     , xlab = "log transformed", xlim = c(9,12))
dev.off()
png(filename = "../../images/histgram of Median Earning after 8 year(log transfromed).png", width=800, height=600)
hist(log(m8), col = "#5679DF", breaks = 10, main = "Histogram of Median Earning after 8 year"
     , xlab = "log transformed", xlim = c(9,12))
dev.off()
png(filename = "../../images/histgram of Median Earning after 10 year(log transfromed).png", width=800, height=600)
hist(log(m10), col = "#5679DF", breaks = 10, main = "Histogram of Median Earning after 10 year"
     , xlab = "log transformed", xlim = c(9,12))
dev.off()



#####################Admission rate####################################
#ADM_RATE
##Admission rate

#The number of NAs in Median earning in 6,8,10 years after entry
sum(as.numeric(is.na(dat1$ADM_RATE))) #81
ad = na.omit(dat1$ADM_RATE)

#Summary Statistics of Admission#
sink(file = "../../data/eda-output.txt", append=TRUE)
cat("C. Summary Statistics Of Admission rate\n\n")
cat(summary(ad), "\n")
cat("Stadard Deviation. : ", sd(ad),"\n")
cat("Range. : ", max(ad)-min(ad)," \n")
cat("IQR. : ", IQR(ad),"\n")
cat("\n")
cat("\n\n")
sink()

png(filename = "../../images/histgram of admission rate.png", width=800, height=600)
hist(ad, col = "#5679DF", breaks = 10, main = "Histogram of Admission rate")
dev.off()

###############Compare complete rates to demographic#################################
#(Demographic graduation rate/Whole Graduation rate (make sure these are over the same time frame))

com_demo_w = subset(dat1, (!is.na(dat1$C150_4_WHITE)) & (!is.na(dat1$C150_4_BLACK)) &
                    (!is.na(dat1$C150_4_HISP)) & (!is.na(dat1$C150_4_ASIAN)) & (!is.na(dat1$C150_4)))

png(filename = "../../images/scatterplot of completion rate of white.png", width=800, height=600)
plot(com_demo_w$C150_4,com_demo_w$C150_4_WHITE
     , main = "scatter plot of overall completion rate and completion rate of white"
     , xlab = "Completion rate", ylab = "Completion rate of white ")
dev.off()

png(filename = "../../images/scatterplot of completion rate of black.png", width=800, height=600)
plot(com_demo_w$C150_4,com_demo_w$C150_4_BLACK
     , main = "scatter plot of overall completion rate and completion rate of black"
     , xlab = "Completion rate", ylab = "Completion rate of black ")
dev.off()

png(filename = "../../images/scatterplot of completion rate of hispanic.png", width=800, height=600)
plot(com_demo_w$C150_4,com_demo_w$C150_4_HISP
     , main = "scatter plot of overall completion rate and completion rate of hispanic"
     , xlab = "Completion rate", ylab = "Completion rate of hispanic ")
dev.off()

png(filename = "../../images/scatterplot of completion rate of asian.png", width=800, height=600)
plot(com_demo_w$C150_4,com_demo_w$C150_4_ASIAN
     , main = "scatter plot of overall completion rate and completion rate of asian"
     , xlab = "Completion rate", ylab = "Completion rate of asian")
dev.off()

png(filename = "../../images/boxplot of completion rate by demographic.png", width=800, height=600)
boxplot(com_demo_w$C150_4_WHITE, com_demo_w$C150_4_BLACK
        , com_demo_w$C150_4_HISP, com_demo_w$C150_4_ASIAN, com_demo_w$C150_4
        , las=2, col = c("red","sienna","palevioletred1","royalblue2", "royalblue2")
        , names = c("white", "black", "hispanic", "asian", "overall"))
dev.off()

##############################################################################################
#######################Correlation for completion to other indicators#########################
#overall Completion rate: C150_4
#Cost of attendance: NPT4_PUB/NPT4_PRIV
#Size of school: CCSIZSET
#public/ private(non-profit)/ private(profit) : CONTROL
#Sat scores : SAT_AVG

#The number of NAs in in each indicators mentioned above.
sum(as.numeric(is.na(dat1$C150_4))) #36
sum(as.numeric(is.na(dat1$NPT4_PUB))) #174
sum(as.numeric(is.na(dat1$NPT4_PRIV))) #51
sum(as.numeric(is.na(dat1$CCSIZSET))) #0
sum(as.numeric(is.na(dat1$SAT_AVG))) #134
#Subsetting by SAT_AVG

cor = subset(dat1,(!is.na(dat1$SAT_AVG)))
length(cor$C150_4)
length(cor$NPT4_PUB)
length(cor$NPT4_PRIV)
length(cor$CCSIZSET)
length(cor$SAT_AVG)
length(na.omit(cor$NPT4_PUB))

na.omit(as.numeric(cor$NPT4_PUB)) + na.omit(as.numeric(cor$NPT4_PRIV))


cor$NPT4_PUB
cor$NPT4_PRIV
