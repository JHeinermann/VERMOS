# Script to export Data to NetLogo

#############################################################################################################################
#### Used Packages ####
#############################################################################################################################
library(readxl)
library(ggplot2)
library(EnvStats)

#############################################################################################################################
#### Selfmade Functions ####
#############################################################################################################################
GetFile <- function(FileName){
  paste0(sub(sub(".*\\/", "", rstudioapi::getSourceEditorContext()$path), "", rstudioapi::getSourceEditorContext()$path), FileName)
}

#############################################################################################################################
#### Import Overstory Data ####
#############################################################################################################################
overstory_data <- read_excel(GetFile("5138_5144.xlsx"), 
                             sheet = "5138_Oberstand", col_types = c("text", 
                                                                     "text", "numeric", "numeric", "text", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "numeric", "numeric", 
                                                                     "numeric", "text"))

colnames(overstory_data) <- c("District", "Stand_ID", "Plot_ID", "Tree_ID", "Species", "Species_ID",
                              "X", "Y", "DBH", "Height", "Crown_Start", "r_crown_N", "r_crown_E",
                              "r_crown_S", "r_crown_W", "Comment")

#############################################################################################################################
#### Calculate Crown Radius from  4 Crown Radii ####
#############################################################################################################################
overstory_data$r_crown <- sqrt((pi * overstory_data$r_crown_N ^ 2 + pi * overstory_data$r_crown_E ^ 2 + 
                                  pi * overstory_data$r_crown_S ^ 2 + pi * overstory_data$r_crown_W ^ 2) / pi / 4)
overstory_data$DBH <- overstory_data$DBH / 100
overstory_data <- overstory_data[!is.na(overstory_data$r_crown), ]

#############################################################################################################################
#### Calculate Root Radius Based on Crown Radius ####
#############################################################################################################################
overstory_data$r_root <- sqrt((pi * overstory_data$r_crown ^ 2) / (sum(pi * overstory_data$r_crown ^ 2) / (50 * 50)) / pi)
CrownRoot <- sqrt((50 * 50 ) / sum(pi * overstory_data$r_crown ^ 2))
ggplot(overstory_data)+
  geom_function(fun = function(x){x * overstory_data$r_root[1] / overstory_data$r_crown[1]}, color = "blue", size = 1)+
  geom_point(aes(x = r_crown, y = r_root))+
  annotate("text", x = 0, y = 5, hjust = 0, vjust = 1, label = paste0("y = ", round(CrownRoot, digits = 2), "x"))+
  scale_x_continuous(name = "Crown Radius [m]", limits = c(0, 4))+
  scale_y_continuous(name = "Root Radius [m]")

#############################################################################################################################
#### Plot Data ####
#############################################################################################################################
hist(overstory_data$DBH)
hist(overstory_data$Height)
hist(overstory_data$r_crown)

plot(overstory_data$DBH, overstory_data$Height)
plot(overstory_data$DBH, overstory_data$r_crown)

#############################################################################################################################
#### Export Overstory Data to Github ####
#############################################################################################################################
write.table(overstory_data[, c("Stand_ID", "Plot_ID", "Tree_ID", "Species", "Species_ID",
                               "X", "Y", "DBH", "Height", "r_crown", "r_root")], 
            file = GetFile("5138_Overstory.txt"), 
            sep = " ", 
            col.names = FALSE,
            row.names = FALSE)

#############################################################################################################################
#### Import Regeneration Data ####
#############################################################################################################################
Regen <- read_excel("E:/Dokumente/VERMOS/Flaechenaufnahmen/Versuch.xlsx", 
                    sheet = "5138_Verjungung", col_types = c("text", 
                                                             "text", "numeric", "numeric", "text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "text"))
colnames(Regen) <- c("District", "Stand_ID", "Plot_ID", "Tree_ID", "Species", "Species_ID",
                     "X", "Y", "RCD", "DBH", "Height", "Comment")

#############################################################################################################################
#### Calculate DBH from RCD and r_crown from DBH ####
#############################################################################################################################
DBH_Model <- nls(DBH ~ b * RCD + c * (RCD ^ 2), start = list(b = 0.3, c = 2), data = Regen)
modfun <- function(RCD){
  coef(DBH_Model)[1] * RCD + coef(DBH_Model)[2] * (RCD ^ 2)
}
Regen$isDBH <- ifelse(is.na(Regen$DBH), FALSE, TRUE)
Regen$DBH <- ifelse(is.na(Regen$DBH), modfun(Regen$RCD), Regen$DBH)

ggplot(Regen)+
  geom_function(fun = modfun, color = "blue", size = 1)+
  geom_point(aes(x = RCD, y = DBH, color = isDBH))+
  scale_color_manual(name = "DBH available?", labels = c("TRUE" = "Yes", "FALSE" = "No"), values = c("TRUE" = 1, "FALSE" = 2))+
  scale_x_continuous(name = "Root Collar Diameter [mm]", limits = c(0, 80))+
  scale_y_continuous(name = "Diameter at Breast Height [mm]")


rCrownModel <- nls(r_crown ~ b * DBH + c * (DBH ^ 2), start = list(b = 0.3, c = 2), data = overstory_data)
modfun2 <- function(DBH){
  # DBH2 <- DBH / 100
  coef(rCrownModel)[1] * DBH + coef(rCrownModel)[2] * (DBH ^ 2)
}

Regen$r_crown <- modfun2(Regen$DBH / 1000)

ggplot()+
  geom_function(fun = modfun2, color = "blue", size = 1)+
  geom_point(data = Regen, aes(x = DBH / 1000, y = r_crown), color = 2)+
  geom_point(data = overstory_data, aes(x = DBH, y = r_crown))+
  scale_x_continuous(name = "Diameter at Breast Height [m]", limits = c(0, 0.6))+
  scale_y_continuous(name = "Crown Radius [m]")


#############################################################################################################################
#### Give outer Layer Attributes ####
#############################################################################################################################
ggplot(Regen)+
  annotate("rect", xmin = -10, xmax = 10, ymin = -10, ymax = 10, fill = "grey90")+
  annotate("rect", xmin = -5, xmax = 5, ymin = -5, ymax = 5, fill = "grey70")+
  geom_point(aes(x = X, y = Y, color = !is.na(DBH)))+
  scale_color_manual(name = "Zone", values = c("TRUE" = 1, "FALSE" = 2), labels = c("TRUE" = "Inner", "FALSE" = "Outer"))+
  coord_fixed()+
  theme_bw()

# Calculate DBH for Outer Layer with gamma distribution (will be replaced with Chris' Function)
dfun <- function(x, multi = 0.2){
  dgamma(x, shape = egamma(Regen$DBH / 1000)$parameters[1], scale = egamma(Regen$DBH / 1000)$parameters[2]) * multi
}

ggplot(Regen)+
  geom_histogram(aes(x = DBH / 1000), color = "black", bins = 40)+
  geom_function(fun = dfun, args = list(multi = 0.11))+
  scale_x_continuous(limits = c(0, 0.03))

Regen$isDBH <- ifelse(is.na(Regen$DBH), 
                    FALSE,
                    TRUE)
g_shape <- egamma(Regen$DBH)$parameters[1]
g_scale <- egamma(Regen$DBH)$parameters[2]
for(i in 1:length(Regen$DBH)){
  if(is.na(Regen$DBH[i])){
    Regen$DBH[i] <- rgamma(length(Regen$DBH), shape = g_shape, scale = g_scale)[i]
  }
}

ggplot(Regen)+
  geom_histogram(aes(x = DBH / 1000, fill = isDBH), color = "black", bins = 100)

Regen$r_crown <- modfun2(Regen$DBH / 1000)


A1 <- coef(nls(Height / 100 ~ A * (DBH / 1000)^B, start = list(A = 0.3, B = 0.5), data = Regen))[1]
B1 <- coef(nls(Height / 100 ~ A * (DBH / 1000)^B, start = list(A = 0.3, B = 0.5), data = Regen))[2]
modfun3 <- function(x){
  A1 * x ^ B1
}

Regen$isHeight <- ifelse(is.na(Regen$Height), 
                         FALSE,
                         TRUE)
Regen$Height <- ifelse(is.na(Regen$Height), 
                       modfun3(Regen$DBH / 1000) * 100,
                       Regen$Height)

ggplot(Regen)+
  geom_function(fun = modfun3, n = 1000, color = "blue", size = 1)+
  geom_point(aes(x = DBH / 1000, y = Height / 100, color = isHeight))+
  scale_color_manual(name = "Height available?", values = c("TRUE" = 1, "FALSE" = 2), labels = c("TRUE" = "Yes", "FALSE" = "No"))+
  scale_y_continuous(name = "Height [m]", limits = c(0, 4))+
  scale_x_continuous(name = "Diameter at Breast Height [m]")



Regen$r_root <- Regen$r_crown * CrownRoot

#############################################################################################################################
#### Export Regeneration Data to Github ####
#############################################################################################################################
write.table(Regen[, c("Stand_ID", "Plot_ID", "Tree_ID", "Species", "Species_ID",
                               "X", "Y", "DBH", "Height", "r_crown", "r_root")], 
            file = GetFile("5138_Regeneration.txt"), 
            sep = " ", 
            col.names = FALSE,
            row.names = FALSE)




#############################################################################################################################
#### Create random Data just for trying the Model ####
#############################################################################################################################
# Overstory:
shape_O <- egamma(overstory_data$DBH)$parameters[1]
scale_O <- egamma(overstory_data$DBH)$parameters[2]

ggplot()+
  geom_histogram(data = overstory_data, aes(x = DBH), bins = 30, color = "black")+
  geom_function(fun = dgamma, args = list(shape = shape_O, scale = scale_O))

A2 <- coef(nls(Height ~ A * (DBH)^B, start = list(A = 0.3, B = 0.5), data = overstory_data))[1]
B2 <- coef(nls(Height ~ A * (DBH)^B, start = list(A = 0.3, B = 0.5), data = overstory_data))[2]
modfun4 <- function(x, adding = 0){
  A2 * x ^ B2 + adding
}

ggplot(overstory_data)+
  geom_point(aes(x = DBH, y = Height))+
  geom_function(fun = modfun4)


Over_Random <- data.frame(Stand_ID = "R",
                          Plot_ID = "R",
                          Tree_ID = 1:nrow(overstory_data),
                          Species = "Kiefer",
                          Species_ID = 1,
                          X = runif(nrow(overstory_data), min = -25, max = 25),
                          Y = runif(nrow(overstory_data), min = -25, max = 25),
                          DBH = rgamma(nrow(overstory_data), shape = shape_O, scale = scale_O))
Over_Random$Height <- modfun4(Over_Random$DBH) + rnorm(nrow(Over_Random), sd = sd(overstory_data$Height - modfun4(overstory_data$DBH)))
Over_Random$r_crown <- modfun2(Over_Random$DBH) + rnorm(nrow(Over_Random), sd = sd(overstory_data$r_crown - modfun2(overstory_data$DBH)))
Over_Random$r_root <- Over_Random$r_crown * CrownRoot


write.table(Over_Random, 
            file = "~/GitHub/VERMOS_open/Overstory_Random.txt", 
            sep = " ", 
            col.names = FALSE,
            row.names = FALSE)






# Regeneration
R_Species <- sample(Regen$Species, size = nrow(Regen), replace = TRUE)
Regen_Random <- data.frame(Stand_ID = "R",
                           Plot_ID = "R",
                           Tree_ID = 1:nrow(Regen),
                           Species = R_Species,
                           Species_ID = ifelse(R_Species == "Kiefer", 1, ifelse(R_Species == "Trauben-Eiche", 2, ifelse(R_Species == "Eberesche", 5, 7))),
                           X = runif(nrow(Regen), min = -10, max = 10),
                           Y = runif(nrow(Regen), min = -10, max = 10),
                           DBH = rgamma(nrow(Regen), shape = g_shape, scale = g_scale))
Regen_Random$Height <- modfun3(Regen_Random$DBH / 1000) * 100
Regen_Random$r_crown <- modfun2(Regen_Random$DBH / 1000)
Regen_Random$r_root <- Regen_Random$r_crown * CrownRoot

write.table(Regen_Random, 
            file = "~/GitHub/VERMOS_open/Regeneration_Random.txt", 
            sep = " ", 
            col.names = FALSE,
            row.names = FALSE)




#############################################################################################################################
#### Convert Climate Data ####
#############################################################################################################################
ClimateData <- read.csv(file = "~/GitHub/VERMOS_open/Climate_Data.csv")

write.table(ClimateData, 
            file = "~/GitHub/VERMOS_open/Climate_Data.txt", 
            sep = " ", 
            col.names = TRUE,
            row.names = FALSE)











