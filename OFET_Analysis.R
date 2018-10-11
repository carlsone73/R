#Importing the Raw Dataset
library(readr)
df<- read_csv("W850 20180314_Transfer_20-4.csv")
plot(-df$V1,-df$I2)
#Assigning FET constants
C <- 1.5E-8
W <- 2000
L <- 20

#Creating Separate Variable Groupings for Linear vs. Saturated Regimes

VL <- df$V1[223:444]
IL <- df$I2[223:444]
VS <- df$V1[889:1110]
IS <- df$I2[889:1110]

plot(-VL,-IL)
plot(-VS,-IS)

#Calculating the Threshold Voltage
VL_T <- -VL[30:70]
IL_T <- -IL[30:70]

Thresh_l <- lm(IL_T ~ VL_T)
plot(-VL[1:111], -IL[1:111])
abline(Thresh_l)
summary(Thresh_l)
Vth_l <- summary(Thresh_l)$coefficients[1]/-summary(Thresh_l)$coefficients[2]

VS_T <- -VS[30:70]
IS_T <- sqrt(-IS[30:70])

plot(VS_T, IS_T)
Thresh_s <-lm(IS_T ~ VS_T)
plot(-VS[1:111], sqrt(-IS[1:111]))
abline(Thresh_s)
summary(Thresh_s)
Vth_s <- summary(Thresh_s)$coefficients[1]/-summary(Thresh_s)$coefficients[2]

#Calculating the linear mobility
VL_L_F <- -VL[30:70] + Vth_l
IL_L_F <- -IL[30:70]

plot(-VL[1:111] + Vth_l, -IL[1:111])

lin_F <- lm(IL_L_F ~ VL_L_F)
summary(lin_F)
abline(lin_F)

mu_lin_F <- summary(lin_F)$coefficients[2]*L/C/W/25
mu_lin_F

VL_L_R <- -VL[112:132] + Vth_l
IL_L_R <- -IL[112:132]

plot(-VL[112:222] + Vth_l, -IL[112:222])

lin_R <- lm(IL_L_R ~VL_L_R)
summary(lin_R)
abline(lin_R)

mu_lin_R <- summary(lin_R)$coefficients[2]*L/C/W/25
mu_lin_R

mu_lin <- (mu_lin_F + mu_lin_R)/2

#Calculating the saturated mobility
VS_SR_F <- -VS[50:80] + Vth_s
IS_SR_F <- sqrt(-IS[50:80])

plot(-VS[1:111] + Vth_s, sqrt(-IS[1:111]))
sat_F <- lm(IS_SR_F ~ VS_SR_F)
summary(sat_F)
abline(sat_F)

mu_sat_F <- summary(sat_F)$coefficients[2]^2*2*L/C/W
mu_sat_F

VS_SR_R <- -VS[112:132] + Vth_s
IS_SR_R <- sqrt(-IS[112:132])

plot(-VS[112:222] + Vth_s, sqrt(-IS[112:222]))
sat_R <- lm(IS_SR_R ~ VS_SR_R)
summary(sat_R)
abline(sat_R)

mu_sat_R <- summary(sat_R)$coefficients[2]^2*2*L/C/W
mu_sat_R

mu_sat <- (mu_sat_F + mu_sat_R)/2

#On/Off ratio

OO <- min(IS)/max(0.0001*IS)

  
#Checking out the plots to make sure they look good
reg <- lm(IS_SR_F ~ VS_SR_F)

plot(-VS[1:111], -IS[1:111], log = "y", type = "o", lwd = "0.6", lty = 1, 
     pch = 20, cex = 0.6, col ="red", 
     xlab = "Gate Voltage / -V", 
     ylab = "Drain Current / -A", yaxt= "n",
     mar=c(5.1,4.1,4.1,4.1), ylim = c(10^-9, 3.5*10^-4))

#legend("topleft", legend = c(expression("W852")), inset = c(0.05,0.05))

axis(2, at = c(0, 1E-9, 1E-8, 1E-7,1E-6,1E-5,1E-4), 
        labels = c(0,expression(10^-9),
              expression(10^-8), expression(10^-7), expression(10^-6),
              expression(10^-5), expression(10^-4)))
      

#axis(2, at = c(1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1e-05, 1e-04, 1e-03), 
     #labels = c(expression(10^-9),expression(10^-9),expression(10^-8), 
                #expression(10^-7), expression(10^-6), 
                #expression(10^-5),expression(10^-4),expression(10^-3)))

par(new = TRUE, mar=c(5.1,4.1,4.1,4.1))

plot(-VS[1:111], sqrt(-IS[1:111]), type = "o", lwd= "0.4", lty = 1, 
     pch = 20, cex = 0.6, col = "blue", 
     xaxt = "n", yaxt = "n", xlab = "", 
     ylab = "", ylim = c(0, 0.02))
axis(side = 4)
mtext(expression("Drain Current"^{1/2}*" / -A"^{1/2}), side = 4, line = 2.8)

#Output curve plotting
V_0 <- df$V1[1:121]
I_0 <- df$I1[1:121]
V_10 <- df$V1[122:242]
I_10 <- df$I1[122:242]
V_20 <- df$V1[243:363]
I_20 <- df$I1[243:363]
V_30 <- df$V1[364:484]
I_30 <- df$I1[364:484]
V_40 <- df$V1[485:605]
I_40 <- df$I1[485:605]
V_50 <- df$V1[606:726]
I_50 <- df$I1[606:726]
V_60 <- df$V1[727:847]
I_60 <- df$I1[727:847]


plot(-V_0, -I_0, axes = FALSE, xlab = "", ylab = "", type = "o", ylim = c(0, 0.00003),
     col = "orange", cex = 0.6)
par(new = TRUE)
plot(-V_10, -I_10, axes = FALSE, xlab = "", ylab = "", type = "o", ylim = c(0, 0.00003),
     col = 304, cex = 0.6)
par(new = TRUE)
plot(-V_20, -I_20, axes = FALSE, xlab = "", ylab = "", type = "o", ylim = c(0, 0.00003),
     col = "#FF7F00", cex = 0.6)
par(new = TRUE)
plot(-V_30, -I_30, axes = FALSE, xlab = "", ylab = "", type = "o", ylim = c(0, 0.00003),
     col = "#984EA3", cex = 0.6)
par(new = TRUE)
plot(-V_40, -I_40, axes = FALSE, xlab = "", ylab = "", type = "o", ylim = c(0, 0.00003),
     col = "#4DAF4A", cex = 0.6)
par(new = TRUE)
plot(-V_50, -I_50, axes = FALSE, xlab = "", ylab = "", type = "o", ylim = c(0, 0.00003),
     col = "#377EB8", cex = 0.6)
par(new = TRUE)
plot(-V_60, -I_60, type = "o", ylim = c(0, 0.00003), xlab = "Drain Voltage / -V",
     ylab = "Drain Current / -A", yaxt = "n", col = "#E41A1C", cex = 0.6)


axis(2, at = c(0, 0.00001, 0.00002, 0.00003), labels = c(0,expression(1%*%10^-4),
     expression(2%*%10^-4),expression(3%*%10^-4)))

legend("topleft", legend = c(expression("V"[G]*"= 0V"),expression("V"[G]*"= -10V"),
                             expression("V"[G]*"= -20V"),expression("V"[G]*"= -30V"),
                             expression("V"[G]*"= -40V"),expression("V"[G]*"= -50V"),
                             expression("V"[G]*"= -60V")), 
       col = c("orange", 304, "#FF7F00", "#984EA3", "#4DAF4A", "#377EB8", "#E41A1C"), 
       inset = c(0.0,0.0), lty = 1)

