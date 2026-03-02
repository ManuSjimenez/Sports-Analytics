# Homework 4
# Manuela Jimenez

# 4.1
# 1.
library(baseballr)

# 2.
bat_data=fg_batter_leaders(startseason=2023,endseason=2023)

# 3.
library(dplyr)
bat_use = bat_data %>% filter(PA>=50) # reduced data set to 542 obs

# 4.
cor(bat_use$EV,bat_use$IFH_pct)
cor(bat_use$Spd,bat_use$IFH_pct) # strongest relationship
cor(bat_use$Contact_pct, bat_use$IFH_pct)
cor(bat_use$GB_pct, bat_use$IFH_pct)

# 5. 
Ohtani_2025 = read.csv("Ohtani_2025.csv")

cor(Ohtani_2025$launch_speed,Ohtani_2025$launch_angle,use="complete.obs")

library(ggplot2)
ggplot(Ohtani_2025, aes(x = launch_speed, y = launch_angle)) + geom_point()

# 6.
Ohtani_2025=Ohtani_2025%>%mutate(is_hr=ifelse(events=="home_run","HR","NO"))

# 7.
ggplot(Ohtani_2025, aes(x = launch_speed, y = launch_angle)) + geom_point(aes(color=is_hr))

# 8.
Ohtani_HRs = Ohtani_2025 %>% filter(is_hr == "HR") %>% summarize(avg_launch_speed = mean(launch_speed),avg_launch_angle = mean(launch_angle))
Ohtani_HRs


# 4.2
library(readxl)
NCAAB <- read_excel("NCAAB_Efficiency.xlsx")

# 1.
cor(NCAAB$PPG, NCAAB$WinPct)
cor(NCAAB$OPPG, NCAAB$WinPct)

# 2.
cor(NCAAB$ORtg, NCAAB$WinPct)
cor(NCAAB$DRtg, NCAAB$WinPct)

# 3.
cor(NCAAB$ORtg, NCAAB$DRtg)

# 4.
ggplot(NCAAB, aes(x = ORtg, y = DRtg)) + geom_point()

# 5. 
cor(NCAAB$Pace, NCAAB$WinPct)

# 6.
NCAAB_tourney = NCAAB %>% filter(Tourney == 1)
cor(NCAAB_tourney$Pace, NCAAB_tourney$WinPct)
ggplot(NCAAB_tourney, aes(x = ORtg, y = DRtg)) + geom_point()


# 4.3
# 1.
PGCareers = read.csv("PGCareers.csv")

PGCareers = PGCareers %>% mutate(MPG = MP/G)

# 2.
cor(PGCareers$Age, PGCareers$MPG)

# 3. 
ggplot(PGCareers,aes(x=Age,y=MPG))+geom_point()

# 4. 
ggplot(PGCareers,aes(x=Age,y=MPG))+geom_point()+geom_smooth(method="loess")
