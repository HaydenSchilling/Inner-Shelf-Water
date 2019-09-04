mydata <- read.csv("Data/CleanedData_220519.csv", header = T)

table(is.na(mydata))

str(mydata)
table(mydata$site, mydata$Vbin)
