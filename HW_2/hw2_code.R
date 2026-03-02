# 2.1
pbp_2025=read.csv("play_by_play_2025.csv")

#4.
nrow(pbp_2025)
ncol(pbp_2025)

View(pbp_2025)

colnames(pbp_2025)


# 2.2
unique(pbp_2025$play_type)

#2.
table(pbp_2025$play_type)

class(pbp_2025$play_type)

Down_3=subset(pbp_2025,down==3)

mean(Down_3$yards_gained)
#6.
mean(Down_3$yards_gained, na.rm=TRUE)

#7.
sd(Down_3$yards_gained, na.rm=TRUE)
median(Down_3$yards_gained, na.rm=TRUE)
min(Down_3$yards_gained, na.rm=TRUE)
max(Down_3$yards_gain, na.rm=TRUE)
range(Down_3$yards_gained, na.rm=TRUE)
sum(Down_3$yards_gained, na.rm=TRUE)

#8.
Down_1=subset(pbp_2025, down==1)
mean(Down_1$yards_gained, na.rm=TRUE)

#9.
Short_3 = subset(pbp_2025, play_type=="run" & down==3 & ydstogo<=3)
mean(Short_3$yards_gained)


# 2.3
Short_New=Short_3[1:10,1:30]

Short_New=Short_New[,-28]

#3.
Short_New$new_distance=Short_New$yardline_100-Short_New$yards_gained
mean(Short_New$new_distance, na.rm=TRUE)

#4.
Short_New$chunk_gain=ifelse(Short_New$yards_gained>=5,1,0)
table(Short_New$chunk_gain)


# 2.4
install.packages("nflreadr")
library(nflreadr)
pbp_2024=load_pbp(seasons=2024)

write.csv(pbp_2024,"pbp_2024.csv", row.names=FALSE)
