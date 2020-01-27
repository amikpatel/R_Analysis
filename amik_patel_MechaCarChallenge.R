# Import MechaCar.csv
MechaCar <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)
library(tidyverse)
# Use Multiple Linear Regression in R
model1 <- lm(mpg ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance` +
               `AWD`, data=MechaCar)
# Generate Summary Table
summary(model1)
#import dataset into ggplot2
#plt <- ggplot(MechaCar,aes(x=vehicle.length,y=mpg)) 
#add scatter plot with labels
#plt + geom_point() + xlab("Vehicle Length (L)") + ylab("City Fuel-Efficiency (MPG)") 
#plot Car Length vs MPG

plot(MechaCar$`vehicle length`, MechaCar$mpg,
     +      main = 'Regression Car Length vs MPG',
     xlab = 'Vehicle Length', ylab = 'City Fuel-Efficiency (MPG)')
abline(lm(mpg ~ `vehicle length`,data = MechaCar), col = 'red')

#plot Car Clearance vs MPG
plot(MechaCar$`ground clearance`, MechaCar$mpg,
     +      main = 'Regression Car Clearance vs MPG',
     xlab = 'Vehicle Clearance', ylab = 'City Fuel-Efficiency (MPG)')
abline(lm(mpg ~ `ground clearance`,data = MechaCar), col = 'red')

#plot AWD vs MPG
plot(MechaCar$AWD, MechaCar$mpg,
     main = 'AWD vs MPG',xlab = 'AWD', ylab = 'City Fuel-Efficiency (MPG)')
abline(lm(mpg ~ AWD,data = MechaCar), col = 'red')

#plot car weight vs MPG
plot(MechaCar$`vehicle weight`, MechaCar$mpg,
     main = 'Vehicle Weight vs MPG', xlab = 'Vehicle Weight', ylab = 'City Fuel-Efficiency (MPG)')
abline(lm(mpg ~ `vehicle weight`,data = MechaCar), col = 'red')

#plot spoiler angle vs MPG
plot(MechaCar$`spoiler angle`, MechaCar$mpg,
     main = 'Spoiler Angle vs MPG', xlab = 'Spoiler Angle', ylab = 'City Fuel-Efficiency (MPG)')
abline(lm(mpg ~ `spoiler angle`, data = MechaCar), col = 'red')

#Suspension Coil Summary
Suspension_Coil <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F)

#Coil_Summary <- data %>%
 # summarize(
  #  Mean= mean(Suspension_Coil$PSI), 
   # Median= median(Suspension_Coil$PSI), 
    #Variance= var(Suspension_Coil$PSI), 
    #Standard_Deviation= sd(Suspension_Coil$PSI))
mean(Suspension_Coil$PSI)
median(Suspension_Coil$PSI)
var(Suspension_Coil$PSI)
sd(Suspension_Coil$PSI)

plt <- ggplot(Suspension_Coil,aes(x=PSI))
plt + geom_density()

#Suspension Coil One Sample T-Test
coil_sample_table <- Suspension_Coil %>% sample_n(50)
t.test((coil_sample_table$PSI), mu=mean(Suspension_Coil$PSI))

#Suspension Coil Two Sample T-Test
coil_sample_table2 <- Suspension_Coil %>% sample_n(50)
t.test((coil_sample_table$PSI), (coil_sample_table2$PSI))