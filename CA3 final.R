

setwd("C:/Users/JAYACHANDRAN/CA2/")

crimedata <- read.csv("C:/Users/JAYACHANDRAN/CA2/crime_new.csv",stringsAsFactors = FALSE)


summary(crimedata)
View(crimedata)

#Subset for northern Ireland data
df1 <- subset(crimedata, select =c(Policing_District,Count))
View(df1)

#Subset for Belfast data
df2 <- subset(crimedata, select =c(Policing_District,Count))
View(df2)

#install.packages("dplyr")
#df <- filter(crimedata,crimedata$Policing_District ==NorthernIreland,c('Policing_District','Count'))
#df <- filter(crimedata,crimedata$Policing_District ==NorthernIreland,c('Policing_District','Count'))

# Filtering Northern Ireland District in Crime data
library(dplyr)
NrthIre <- df1[which(crimedata$Policing_District == "Northern Ireland"),]

# Filtering Belfast District in Crime data
library(dplyr)
Belfast <- df2[which(crimedata$Policing_District == "Belfast City"),]


# To combine two subsets into dataframe for substituting in SD
dfnew <- rbind(NrthIre,Belfast)
dfnew

View(dfnew)

# histogram graph to compare northern ireland and belfast crime rates
library("lattice")
histogram(~Count |  Policing_District, data = dfnew)


# to find mean value
mean(NrthIre$Count)  #613.9918
mean(Belfast$Count)  #195.1477


# Shapiro test for dataset 'Belfast' and column Count
normality_test_B <- shapiro.test(Belfast$Count[0:5000])
normality_test_B$p.value
normality_test_B

# Shapiro test for dataset 'NrthIre' and column Count
normality_test_N <- shapiro.test(NrthIre$Count[0:5000])
normality_test_N$p.value
normality_test_N

# checking normality value for NrthIre
install.packages("ggpubr")
library(ggpubr)
ggqqplot(NrthIre$Count, ylab = "Crime rates - Northern Ireland", ggtheme = theme_minimal())

# checking normality value for Belfast
library(ggpubr)
ggqqplot(Belfast$Count, ylab = "Crime rates - Belfast City", ggtheme = theme_minimal())



# Quantile-quantile plot allows us to 
# data is distributed normally or not
# Compare the quantiles of both samples 
# We use square brackets to select the cases we want

with(crimedata,
     qqplot(Count[Policing_District == "Northern Ireland"],
            Count[Policing_District == "Belfast City"], 
            main = "Comparing 2 samples", 
            xlab = "Northern Ireland",
            ylab =  "Belfast"))

# Using a QQ plot to check for normality
# qqnorm function plots your sample 
# against a normal distribution
with(crimedata, {
  qqnorm(Count[Policing_District == "Northern Ireland"], 
         main = "Northern Ireland Samples")
})

with(crimedata, {
  qqnorm(Count[Policing_District == "Belfast City"], 
         main = "Belfast Samples")
})

# Using a QQ plot to check for normality
# We can add normality line to the plot to evaluate normality
# for Policing_District = "Northern ireland"
with(crimedata, {
  qqnorm(Count[Policing_District == "Northern Ireland"], 
         main = "Northen Ireland crime rate ")
  qqline(Count[Policing_District == "Northern Ireland"])
})

# We can add normailty line 
# to the plot to evaluate normality
# for Policing_District = 
with(crimedata, {
  qqnorm(Count[Policing_District == "Belfast City"], 
         main = "Belfast city crime rates ")
  qqline(Count[Policing_District == "Belfast City"])
})


# Taking only "Count" variable from Nrth ad Belfast
df3 <- subset(crimedata, select =c(Count))
df4 <- subset(crimedata, select =c(Count))

library(dplyr)
#df <- filter(crimedata,crimedata$Policing_District ==NorthernIreland,
                                     #c('Policing_District','Count'))
NIld <- df3[which(crimedata$Policing_District == "Northern Ireland"),]

library(dplyr)
#df <- filter(crimedata,crimedata$Policing_District ==NorthernIreland,
                                        #c('Policing_District','Count'))
Blft <- df4[which(crimedata$Policing_District == "Belfast City"),]


#H0 : There is no significant  difference between  crime rate in Belfast  and Northen Ireland.
#Ha: There is a significant relationship crime rate in Belfast  and Northen Ireland

# Wilcox test 
wilcox_test <- wilcox.test(NIld,Blft)
wilcox_test

# Finding Standard Deviation for Delta value
sd(dfnew$Count)

# sd = 1167.415
# d value = 0.35   
# we got d value by substracting the mean value of
# both datasets and divide by SD.

# Finding nrows for Northern Ireland and Belfast
nrow(NrthIre)
nrow(Belfast)
# nrow value for NRTH IRE 8100
# nrow value for belfast IRE 8043mpl


#Power test

install.packages("pwr")
library(pwr)
# finding the sample size
power_info <- power.t.test(delta = 0.35,n=NULL, sig.level = .05, power =.90,
                        type="two.sample" , alternative = "two.sided")

power_info

plot(power_info)



#power_in

# To find the power value
Power_in=power.t.test(n=173,delta = 0.35, sd=1, type = c("two.sample"))
Power_in

plot(Power_in)


#calculating  conventional effect size using Cohen method
cohen.ES(test = c("t"), size = c("small"))
#df_cor <- rbind(b1,b2)
#View(dftest)

# Spearman's rank correlation test
# obtaining samples from both datasets

co_NI <- NrthIre[1:87,]

co_BT <- Belfast[1:87,]

# substituting two datasets samples in Correlation test code 
corr_test <- cor.test(co_NI$Count,co_BT$Count,method ="spearman")
corr_test












