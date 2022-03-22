library(ggplot2)

#Question 3.1 part a
Appliance <- read.csv("/Users/Cassie Gill/OneDrive/SCMA 854/CGill HW3/SCMA 854 HW3/ApplianceShipments.csv")
appliance.ts <- ts(Appliance$Shipments, start = c(1985,1), end=c(1989,4), frequency = 4)
plot(appliance.ts, col="blue", lty=1, lwd=2, xlab = "Year", ylab = "Appliance Shipments in millions of $",
     ylim = c(3900, 5000), main = "Appliance Shipments")


#Question 3.1 part b
plot(appliance.ts, col="blue", lty=1, lwd=2, xlab = "Year", ylab = "Appliance Shipments in millions of $", 
     ylim = c(3500, 5000), main = "Appliance Shipments")

#The plot shows that shipments increase during the first and fourth quarters but fall during the second and 
#third quarters. The increases in the first and fourth could be budgetary due to fiscal year closing and opening
#as well as a sales push to meet sales targets. The decrease in quarters two and three could be due to 
#decreased spending to ensure staying within the budget for the year. 


#Question 3.1 part c
par(oma = c(0,0,0,2))
xrange <- c(1,5)
yrange <- range(appliance.ts)
plot(xrange, yrange, main = "Appliance Shipments by Quarter", type = "n", xlab = "Year", ylab =
           "Appliance Shipments in Millions of $", bty = "l")
colors <- rainbow(4)
linetype <- c(1:4)
plotchar <- c(1:4)
for (i in 1:4) {
  current_quarter <- subset(appliance.ts, cycle(appliance.ts)==i)
  lines(current_quarter, type="b", lwd=1.5, lty=linetype[i], col=colors[i], pch=plotchar[i])
}
legend(5.25, 4800, 1:4, cex=0.8, col=colors, pch=plotchar, lty=linetype, title="Quarter",
       xpd=NA)

#Quarters two and three seems to have the highest shipments possibly due to the increase in temperature 
#during the months of April-September. The majority of HVAC repairs take place during this time and failures
#could cause the need for replacement units which would explain the increase in shipments.


#Question 3.1 part d
yearly <- aggregate(appliance.ts, nfrequency=1, FUN=sum)
plot(yearly, bty="l")



#Question 3.2 part a
RidingMowers <- read.csv("/Users/cjgil/OneDrive/SCMA 854/RidingMowers.csv")
par(xpd=TRUE)
xl <- expression(Income ~ ("in" ~ "1000s"))
yl <- expression(Lot ~ Size ~ ("in" ~ "1000ft"^2))
plot(RidingMowers$Income,RidingMowers$Lot_Size, xlab = xl, ylab = yl,
     col = ifelse(RidingMowers$Ownership=="Owner","black","red"), pch=19, bty="l")
legend ("topleft", inset = c(0,-0.15), legend = c("Ownership = Owner", "Ownership = Nonowner"),
        col=c("black","red"), pch=19)

