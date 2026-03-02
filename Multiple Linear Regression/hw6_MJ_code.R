# Manuela Jimenez
# HW 6

# 6.1
# 1. 
library(readxl)
NFL <- read_excel("HW6/Gridiron.xlsx", sheet = "NFL")

# 2. 
cor(NFL$PassYPA, NFL$PF)
cor(NFL$TotPassYds, NFL$PF)

# 3. 
cor(NFL$OPassYPA, NFL$PA)
cor(NFL$OTotPassYds, NFL$PA)

# 4. 
NFL = NFL %>% mutate(Margin = PF - PA)

# 5. 
mult_ml = lm(Margin~PassYPA+ OPassYPA+ RushYPA+
               ORushYPA+ TOC+ TOF + OffPenYds + DefPenYds, data= NFL)
summary(mult_ml)

# 9.
cor(NFL$RushYPA, NFL$PassYPA)

library(ggplot2)
ggplot(NFL, aes(x=RushYPA, y = PassYPA)) + geom_point()

# 10
library(dplyr)
NFL2018 <- read_excel("HW6/Gridiron.xlsx", 
                      sheet = "NFL2018")

NFL2018 = NFL2018 %>% mutate(Margin = PF - PA)
NFL2018= NFL2018 %>% mutate(Est_Margin = predict(mult_ml, newdata = NFL2018, type= "response"))
NFL2018= NFL2018 %>% mutate(Margin_Error = Est_Margin - Margin)
mae = mean(abs(NFL2018$Margin_Error))
mae

# 11
NCAAF <- read_excel("HW6/Gridiron.xlsx", 
                      sheet = "NCAAF")

# 12
mult_ml_NCAA = lm(Margin~PassYPA+ OPassYPA+ RushYPA+
               ORushYPA+ TOC+ TOF + OffPenYds + DefPenYds, data= NCAAF)
summary(mult_ml_NCAA)


# 6.2
# 1. 
MLB_Attendance <- read_excel("HW6/MLB_Attendance.xlsx", 
                             sheet = "MLB_Attendance")

# 3. 
model_MLB = lm(PctCap~ GB_Pre + RS_PG + RA_PG + WPct_Pre + Pre_Streak + Temp + Wind + Start_Time + Opp_RS_PG + Opp_RA_PG + OppWPct_Pre + Opp_Pre_Streak, data= MLB_Attendance)
summary(model_MLB)

# 5. 
model_MLB = lm(PctCap~ GB_Pre + RS_PG + RA_PG + WPct_Pre + Pre_Streak + Temp + Wind + Start_Time + Opp_RS_PG + Opp_RA_PG + OppWPct_Pre, data= MLB_Attendance)
summary(model_MLB)

model_MLB = lm(PctCap~ GB_Pre + RS_PG + RA_PG + Pre_Streak + Temp + Wind + Start_Time + Opp_RS_PG + Opp_RA_PG + OppWPct_Pre, data= MLB_Attendance)
summary(model_MLB)

model_MLB = lm(PctCap~ GB_Pre + RS_PG + RA_PG + Temp + Wind + Start_Time + Opp_RS_PG + Opp_RA_PG + OppWPct_Pre, data= MLB_Attendance)
summary(model_MLB)

# final model:
model_MLB = lm(PctCap~ GB_Pre + RS_PG + RA_PG + Temp + Wind + Start_Time + Opp_RS_PG + Opp_RA_PG, data= MLB_Attendance)
summary(model_MLB)

# 8. 
Att_2021 <- read_excel("HW6/MLB_Attendance.xlsx", 
                       sheet = "Att_2021")

Att_2021= Att_2021 %>% mutate(Est_PctCap = predict(model_MLB, newdata = Att_2021, type= "response"))
Att_2021= Att_2021 %>% mutate(PctCap_Error = Est_PctCap - PctCap)
mae = mean(abs(Att_2021$PctCap_Error))
mae
