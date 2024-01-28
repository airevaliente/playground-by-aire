# Q3.2.1 -----------------------------------------------------------------------
#Answer
remind_me <- function(){
  print("Weekly Appointments")
  weekly_appointments <- c("Monday = Tango at 7","Wednesday = Theater at 6.40",
                           "Thursday = Dutch at 5.30", "Friday = Work at 9")
  return(weekly_appointments)
}
remind_me()

cheat<- function(x) {
  if(x == "Q3.1.3") {
  return(
  
  'install.packages("titanic")

library(titanic)
library(ggplot2)

data("titanic_train")

ggplot(
  data = titanic_train,
  aes(x = factor(Sex),
      fill = factor(Survived, labels = c("dead", "alive")))) +
  geom_bar(position = "stack") +
  labs(x = "Sex",
       y = "Count",
       fill = "How did it go?"))' )} else if (x == "Q3.1.12") {
  return('install.packages("quantmod")
library(quantmod)
getSymbols("ENI.MI", src = "yahoo", from = "2023-01-01", to = "2023-12-31")
chart_Series(ENI.MI$ENI.MI.Close, name = "Eni (ENI.MI) Prices in 2023")
title("Eni Prices in 2023")

# Eni is an Italian company that: looks for, develops, and sells 
# hydrocarbons; supplies and markets gas and power; refines and markets petroleum
# products; produces and markets petrochemicals and plastics. ')
  }else if (x == "Q3.1.16") {
   return('matrix(1:9, nrow = 3, byrow = TRUE)*3')
  }        
}  

cheat("Q3.1.3")

# Q3.2.1 -----------------------------------------------------------------------
#Answer
library(ggplot2)
library(RColorBrewer)
set.seed(345)

ngroup <- 30
names <- paste("G_", seq(1, ngroup), sep = "") # group names
DAT <- data.frame()

for (i in 1:30) {
  data <- data.frame(matrix(0, ngroup, 3))
  data[, 1] <- i
  data[, 2] <- sample(names, nrow(data))
# I assign a random group name to each row
  data[, 3] <- prop.table(sample(c(rep(0, 100), c(1:ngroup)), nrow(data)))
  
  DAT <- rbind(DAT, data)
}

colnames(DAT) <- c("Year", "Group", "Value")

DAT <- DAT[order(DAT$Year, DAT$Group), ]
coul <- colorRampPalette(brewer.pal(12, "Paired"))(ngroup)
# I order indices randomly and assign colors in random order
coul <- coul[sample(seq_along(coul), length(coul))]

ggplot(DAT, aes(x = Year, y = Value, fill = Group)) +
  geom_area(alpha = 1) +
  theme_bw() +
  scale_fill_manual(values = coul) +
  theme(
    text = element_blank(), # I hide text
    line = element_blank(), # I hide lines
    title = element_blank(), # I hide the title
    legend.position = "none", # I remove the legend 
    panel.border = element_blank(), # I hide the rectangle around the graph
    panel.background = element_blank() # I hide the background
  )

