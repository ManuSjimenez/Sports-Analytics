# Manuela Jimenez HW3
# 3.1
library(dplyr)
library(readxl)

# 1.
FBS_2025 = read_excel("FBS_2025.xlsx")
View(FBS_2025)

# 2.
WPct_Only=FBS_2025%>%select(School,WPct,ConfWPct)
WPct_Only

ACC_2025=FBS_2025%>%filter(Conf=="ACC")
ACC_2025

Winners_Only=FBS_2025%>%select(School,WPct,ConfWPct)%>%filter(WPct>=.5)
Winners_Only


# 3.2
# 1.
ACC_SEC=FBS_2025%>%filter(Conf=="ACC" | Conf=="SEC")%>%arrange(Conf, ConfWPct)
ACC_SEC

ACC_SEC=FBS_2025%>%filter(Conf=="ACC" | Conf=="SEC")%>%arrange(Conf, desc(ConfWPct))
ACC_SEC

# 2. 
ACC_SEC_NEW=ACC_SEC%>%mutate(MOV=PPG-OPPG)
ACC_SEC_NEW

# to arrange them from lowest to highest
ACC_SEC_NEW = ACC_SEC_NEW %>% arrange(Conf, MOV)
ACC_SEC_NEW

# to arrange them from highest to lowest
ACC_SEC_NEW = ACC_SEC_NEW %>% arrange(Conf, desc(MOV))
ACC_SEC_NEW

# 3. 
Conf_SOS=FBS_2025%>%group_by(Conf)%>%summarize(ASOS=mean(SOS, na.rm=TRUE))
Conf_SOS

# 4.
Conf_Stats=FBS_2025%>%group_by(Conf)%>%summarize(ASOS=mean(SOS, na.rm=TRUE), BestRk=min(AP_High, na.rm=TRUE))
Conf_Stats


# 3.3
# 1.
FBS_2024 = read_excel("FBS_2024.xlsx")
FBS_Full=bind_rows(FBS_2025,FBS_2024)

# 2. 
FBS_2024=FBS_2024%>%select(School, WPct)
FBS_Multi=left_join(FBS_2025,FBS_2024,by="School")

# 3. 
FBS_Multi=FBS_Multi%>%rename("WPct_25"="WPct.x", "WPct_24"="WPct.y")
FBS_Multi=FBS_Multi%>%select(1:4,16,5:15)

#removing the missouri and delaware rows:
FBS_Multi=FBS_Multi%>%slice(-c(71:72))

FBS_Multi = FBS_Multi %>% mutate(diff_WPct = WPct_25 - WPct_24)
FBS_Multi

# what i could have done
FBS_Multi %>% filter(School=="Florida State") %>% select(diff_WPct)


# 3.4
library(ggplot2)

# 1.
ggplot(Conf_Stats,aes(x=Conf,y=ASOS))+geom_col()

# 2.
# Using the subset ACC_2025
ggplot(ACC_2025,aes(x=School,y=PPG))+geom_col()
