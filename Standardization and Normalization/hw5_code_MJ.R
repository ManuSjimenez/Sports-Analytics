# Homework 5
# Manuela Jimenez

# 5.1
# 1. 
library(readxl)
Mahomes <- read_excel("Mahomes.xlsx", sheet = "Mahomes")
Warner <- read_excel("Mahomes.xlsx", sheet = "Warner")
Marino <- read_excel("Mahomes.xlsx", sheet = "Marino")

library(dplyr)
Mahomes = Mahomes %>% filter(Pos=="QB" & Att>=100)
Warner = Warner %>% filter(Pos=="QB" & Att>=100)
Marino = Marino %>% filter(Pos=="QB" & Att>=100)

# 2.
# standardizing
Mahomes_z = Mahomes %>% mutate(CmpPct_z = as.numeric(scale(CmpPct, center = TRUE, scale = TRUE)),
                               AdjNYPA_z = as.numeric(scale(AdjNYPA, center = TRUE, scale = TRUE)),
                               TD_z = as.numeric(scale(TD, center = TRUE, scale = TRUE)))
Warner_z = Warner %>% mutate(CmpPct_z = as.numeric(scale(CmpPct, center = TRUE, scale = TRUE)),
                             AdjNYPA_z = as.numeric(scale(AdjNYPA, center = TRUE, scale = TRUE)),
                             TD_z = as.numeric(scale(TD, center = TRUE, scale = TRUE)))
Marino_z = Marino %>% mutate(CmpPct_z = as.numeric(scale(CmpPct, center = TRUE, scale = TRUE)),
                             AdjNYPA_z = as.numeric(scale(AdjNYPA, center = TRUE, scale = TRUE)),
                             TD_z = as.numeric(scale(TD, center = TRUE, scale = TRUE)))

# normalizing
Mahomes_norm = Mahomes_z %>% mutate(CmpPct_norm = pnorm(CmpPct_z),
                                    AdjNYPA_norm = pnorm(AdjNYPA_z),
                                    TD_norm = pnorm(TD_z))
Warner_norm = Warner_z %>% mutate(CmpPct_norm = pnorm(CmpPct_z),
                                  AdjNYPA_norm = pnorm(AdjNYPA_z),
                                  TD_norm = pnorm(TD_z))
Marino_norm = Marino_z %>% mutate(CmpPct_norm = pnorm(CmpPct_z),
                                  AdjNYPA_norm = pnorm(AdjNYPA_z),
                                  TD_norm = pnorm(TD_z))

# ADDITIONALLY WE COULD
mahomes_final = Mahomes_norm %>% filter(Player== "Patrick Mahomes") %>% select(Player, CmpPct_norm, AdjNYPA_norm, TD_norm)
marino_final = Marino_norm %>% filter(Player == "Dan Marino") %>% select(Player, CmpPct_norm, AdjNYPA_norm, TD_norm)
warner_final = Warner_norm %>% filter(Player == "Kurt Warner") %>% select(Player, CmpPct_norm, AdjNYPA_norm, TD_norm)
comparison=rbind(mahomes_final, marino_final, warner_final) # create data set that contains 3 players
comparison

# 5.2
# 1. 
fbs_portal = read.csv("fbs_portal.csv")
WRs = fbs_portal %>% filter(Position == "WR")
t.test(WRs$PFF_Grade_Pre_WT,WRs$PFF_Grade_Post_WT, paired = TRUE)

# 2. 
# now for tight end:
TEs = fbs_portal %>% filter(Position == "TE")
t.test(TEs$PFF_Grade_Pre_WT,TEs$PFF_Grade_Post_WT, paired = TRUE)

# 3. 
# One-tailed paired sample t-tests
# wide receiver
t.test(WRs$PFF_Grade_Pre_WT,WRs$PFF_Grade_Post_WT, paired = TRUE, alternative = "less")
# tight end
t.test(TEs$PFF_Grade_Pre_WT,TEs$PFF_Grade_Post_WT, paired = TRUE, alternative = "less")

# 4. 
star_3 = fbs_portal %>% filter(Star_Rating == "3")
t.test(star_3$Snap_Count_Pre_per_Season,star_3$Snap_Count_Post_per_Season, paired = TRUE, alternative = "less")

star_5 = fbs_portal %>% filter(Star_Rating == "5")
t.test(star_5$Snap_Count_Pre_per_Season,star_5$Snap_Count_Post_per_Season, paired = TRUE, alternative = "less")

