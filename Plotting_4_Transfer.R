#Importing the Raw Dataset
library(readr)
df1 <- read_csv("W850_T_10-3.csv")
df2 <- read_csv("W852_T_10-3.csv")
df3 <- read_csv("W857_T_10-4.csv")
df4 <- read_csv("W859_T_10-2.csv")

#Plot first curve
plot(-df1$V1[1020:1110], -df1$I2[1020:1110], log = "y", type = "o", lwd = "0.6", lty = 1, 
     pch = 20, cex = 0.6, col ="blue", 
     xlab = "Gate Voltage / -V", 
     ylab = "Drain Current / -A", yaxt= "n",
     mar=c(5.1,4.1,4.1,4.1), ylim = c(10^-9, 3.5*10^-4))

axis(2, at = c(0, 1E-9, 1E-8, 1E-7,1E-6,1E-5,1E-4), 
     labels = c(0,expression(10^-9),
                expression(10^-8), expression(10^-7), expression(10^-6),
                expression(10^-5), expression(10^-4)))

par(new = TRUE, mar=c(5.1,4.1,4.1,4.1))

plot(-df1$V1[889:979], sqrt(-df1$I2[889:979]), type = "o", lwd= "0.4", lty = 1, 
     pch = 20, cex = 0.6, col = "blue", 
     xaxt = "n", yaxt = "n", xlab = "", 
     ylab = "", ylim = c(0, 0.02))

axis(side = 4)

mtext(expression("Drain Current"^{1/2}*" / -A"^{1/2}), side = 4, line = 2.8)


#Plot second curve
par(new = TRUE, mar=c(5.1,4.1,4.1,4.1))

plot(-df2$V1[1020:1110], -df2$I2[1020:1110], log = "y", type = "o", lwd = "0.6", lty = 1, 
     pch = 20, cex = 0.6, col ="red", 
     xlab = "Gate Voltage / -V", 
     ylab = "Drain Current / -A", yaxt= "n",
     mar=c(5.1,4.1,4.1,4.1), ylim = c(10^-9, 3.5*10^-4))

par(new = TRUE, mar=c(5.1,4.1,4.1,4.1))

plot(-df2$V1[889:979], sqrt(-df2$I2[889:979]), type = "o", lwd= "0.4", lty = 1, 
     pch = 20, cex = 0.6, col = "red", 
     xaxt = "n", yaxt = "n", xlab = "", 
     ylab = "", ylim = c(0, 0.02))



