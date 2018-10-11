#Importing the Raw Dataset
library(readr)
df<- read_csv("100_0_20-4_L-A.csv")
plot(-df$VGS,-df$ID)
#Assigning FET constants
C <- 1.5E-8
W <- 2000
L <- 20

#Creating Separate Variable Groupings for Linear Regime
VL <- df$VGS[1:202]
IL <- df$ID[1:202]

plot(-VL,-IL)

#Calculating the Threshold Voltage
VL_T <- -VL[93:101]
IL_T <- -IL[93:101]

Thresh <- lm(IL_T ~ VL_T)
plot(-VL, -IL)
abline(Thresh)
summary(Thresh)
Vth <- summary(Thresh)$coefficients[1]/-summary(Thresh)$coefficients[2]

#Calculating the linear mobility
VL_L_F <- VL[96:101] - Vth
IL_L_F <- IL[96:101]

plot(VL_L_F + Vth, IL_L_F)

VL_L_R <- VL[102:108] - Vth
IL_L_R <- IL[102:108]

plot(VL_L_R + Vth, IL_L_R)

lin_F <- lm(IL_L_F ~ VL_L_F)
summary(lin_F)

mu_lin_F <- summary(lin_F)$coefficients[2]*L/C/W/20
mu_lin_F

lin_R <- lm(IL_L_R ~VL_L_R)
summary(lin_R)

mu_lin_R <- summary(lin_R)$coefficients[2]*L/C/W/20
mu_lin_R

mu_lin <- (mu_lin_F + mu_lin_R)/2

