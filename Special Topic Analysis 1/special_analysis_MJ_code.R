# Manuela Jimenez
# Special Topic Analyses

# 1.1 Rookie Improvement
# 1. 
library(dplyr)
library(readxl)
Rookie <- read_excel("SecondYear.xlsx", sheet = "Rookies")
Second <- read_excel("SecondYear.xlsx", sheet = "SecondSeason")

# we perform the independent t-test for the two variables
t.test(Rookie$AdjNYPA, Second$AdjNYPA, alternative = "less")
t.test(Rookie$Rate, Second$Rate, alternative = "less")

# we then to be able to compare correlation against team performance we will add winning percentage to the data.
Rookie = Rookie %>% mutate(WPct = W/(W+L))
Second = Second %>% mutate(WPct = W/(W+L))

cor(Rookie$WPct, Rookie$AdjNYPA)
cor(Rookie$WPct, Rookie$Rate)
cor(Rookie$WPct, Rookie$GS)
cor(Rookie$WPct, Rookie$CmpPct)
cor(Rookie$WPct, Rookie$TDPct)
cor(Rookie$WPct, Rookie$YPG)
cor(Rookie$WPct, Rookie$AdjYPA) # it would be the same as ADJNYPA but that one is net

# now what about the correlation in the second_year data set
cor(Second$WPct, Second$AdjNYPA)
cor(Second$WPct, Second$Rate)
cor(Second$WPct, Second$GS)
cor(Second$WPct, Second$CmpPct)
cor(Second$WPct, Second$TDPct)
cor(Second$WPct, Second$YPG)


# 1.2 Manning vs. Brady
BradyvsManning <- read_excel("BradyvsManning.xlsx", 
                             sheet = "BradyvsManning")

# to compare the MVP seasons to the other players, we will standardize and normalize and the filter by name and season.
# first, we will standardize and normalize the variables in our original data set
# standardizing
BvsM_z = BradyvsManning %>% mutate(CmpPct_z = as.numeric(scale(CmpPct, center = TRUE, scale = TRUE)),
                               ANYPA_z = as.numeric(scale(ANYPA, center = TRUE, scale = TRUE)),
                               AV_z = as.numeric(scale(AV, center = TRUE, scale = TRUE)))

# normalizing
BvsM_norm = BvsM_z %>% mutate(CmpPct_norm = pnorm(CmpPct_z),
                                    ANYPA_norm = pnorm(ANYPA_z),
                                    AV_norm = pnorm(AV_z))

# we will then filter the 5 seasons where they were MVP's
brady_2007 = BvsM_norm %>% filter(Player== "Tom Brady" & Year == 2007) %>% select(Player, Year,  ANYPA_norm, CmpPct_norm, AV_norm)
brady_2010 = BvsM_norm %>% filter(Player== "Tom Brady" & Year == 2010) %>% select(Player, Year,  ANYPA_norm, CmpPct_norm, AV_norm)
manning_2008 = BvsM_norm %>% filter(Player== "Peyton Manning" & Year == 2008) %>% select(Player,Year, ANYPA_norm, CmpPct_norm, AV_norm)
manning_2009 = BvsM_norm %>% filter(Player== "Peyton Manning" & Year == 2009) %>% select(Player,Year, ANYPA_norm, CmpPct_norm, AV_norm)
manning_2013 = BvsM_norm %>% filter(Player== "Peyton Manning" & Year == 2013) %>% select(Player,Year, ANYPA_norm, CmpPct_norm, AV_norm)
comparison=rbind(brady_2007, brady_2010, manning_2008, manning_2009, manning_2013) # create data set that contains 3 players
comparison


# 1.3 LATE ROUNDERS VS EARLY ROUNDERS
RunningBacks <- read_excel("RunningBacks.xlsx")

# we want to get rid of the players who do not have Rushes
RunningBackss = RunningBacks %>% filter(!is.na(Rush))
# we also get rid of the players with no Receiving information
RunningBackss = RunningBackss %>% filter(!is.na(Rec))
# we get rid of the players who had less than 15 games
RunningBackss = RunningBackss %>% filter(G >= 17)

# we will compare them using these stats
# Yards per Rush Attempt (YPrA), Receiving Yards Per Attempt (RYPrA), Touchdown per Rush Attempt
RunningBackss = RunningBackss %>% mutate(YPrA = RshYds/Rush,
                                         RYPrA = RecYds/Rec,
                                         TDR = RshTD/Rush)
# we will separate the data into first rounders and then players from rounds 6 and 7
# and then do a independent sample t-test\
round1 = RunningBackss %>% filter(Rnd == 1)
round7 = RunningBackss %>% filter(Rnd == 7 | Rnd == 6)

# independent sample t-test
t.test(round1$YPrA, round7$YPrA)
t.test(round1$RYPrA, round7$RYPrA)
t.test(round1$TDR, round7$TDR)


# 1.4 TICKETHOLDER STATUS AND RECALL ABILITY
SteakShake <- read_excel("FSUPromotions.xlsx", 
                         sheet = "SteakShake")
FarmBureau <- read_excel("FSUPromotions.xlsx", 
                         sheet = "FarmBureau")

# chi-square test for independence 
# creating a contingency table for SteakShake
PrefTable_steak = table(SteakShake$Status, SteakShake$Recall)
PrefTable_steak
# we will now conduct the chisquare test
chisq.test(PrefTable_steak) 

# creating a contingency table for Farm Bureau
PrefTable_farm = table(FarmBureau$Status, FarmBureau$Recall)
PrefTable_farm
# we will now conduct the chisquare test
chisq.test(PrefTable_farm)

# what if we just put the two sheets together:
# chi-square test for independence 
# creating a contingency table
promotion = rbind(SteakShake,FarmBureau)
PrefTable_prom = table(promotion$Status, promotion$Recall)
PrefTable_prom

# we will now conduct the chisquare test
chisq.test(PrefTable_prom) # p-value less than alpha so we reject the null hyp, and conclude that there seems to be an association between sport event and genre.


# 1.5 MONEY AND SUCCESS IN BOTH TYPES OF FOOTBALL
# 1.
EPL <- read_excel("Payrolls.xlsx", sheet = "EPL")

cor(EPL$RELXI, EPL$PTPER)
library(ggplot2)
ggplot(EPL, aes(x = RELXI, y = PTPER)) + geom_point() +
  geom_smooth(method = "lm")

# 2. 
NFL <- read_excel("Payrolls.xlsx", sheet = "NFL")
cor(NFL$RelSalary, NFL$WinPer)
ggplot(NFL, aes(x = RelSalary, y = WinPer)) + geom_point() +
  geom_smooth(method = "lm")


# 1.6 USING SCATTERPLOTS TO EXAMINE WIDE RECEIVER PRODUCTION
WR2019 <- read_excel("WR2019.xlsx")
# 1. 
cor(WR2019$ADOT, WR2019$AYAC)

# 2.
ggplot(WR2019, aes(x = ADOT, y = AYAC)) + geom_point() + 
  geom_text(label=WR2019$Player) +
  geom_hline(yintercept = mean(WR2019$AYAC), color = "purple") +
  geom_vline(xintercept = mean(WR2019$ADOT), color = "purple")

# 2.
WR2019 = WR2019 %>% filter(Games >= 5)
ggplot(WR2019, aes(x = ADOT, y = AYAC)) + geom_point() + 
  geom_text(label=WR2019$Player) +
  geom_hline(yintercept = mean(WR2019$AYAC), color = "purple") +
  geom_vline(xintercept = mean(WR2019$ADOT), color = "purple")

# with color:
ggplot(WR2019, aes(x = ADOT, y = AYAC, color = Team)) + geom_point() + 
  geom_text(label=WR2019$Player) +
  geom_hline(yintercept = mean(WR2019$AYAC), color = "black") +
  geom_vline(xintercept = mean(WR2019$ADOT), color = "black")

# 1.7 ESPORTS AFFECTING ACTUAL PERFORMANCE
KylerCOD <- read_excel("KylerCOD.xlsx")

KylerCOD = KylerCOD %>% mutate(CmpPct = Cmp/Att)

# doing the t-test to the necessary variables
t.test(KylerCOD$CAYPA, mu=3.8) # CAYPA
t.test(KylerCOD$BadPct, mu=.161) # BadPct
t.test(KylerCOD$YPScram, mu=7.7)# YPScram
t.test(KylerCOD$CmpPct, mu=.667)#CmpPct: standard completion percentage
