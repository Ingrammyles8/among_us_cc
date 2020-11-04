library(ggplot2)
library(dplyr)
library(tidyverse)
library(plyr)
require(scales)

plyr_stats <- read_csv("../data/among_us_player_stats.csv")
Username = "Big Cheese"
UserID = "1"

# Adding in percentages and rates to the dataframe
plyr_stats$`Imposter Percentage` = plyr_stats$`Times Imposter`/plyr_stats$`Games Started`
plyr_stats$`Crewmate Percentage` = plyr_stats$`Times Crewmate`/plyr_stats$`Games Started`
plyr_stats$`Tasks Done Per Game` = plyr_stats$`Tasks Completed`/plyr_stats$`Times Crewmate`
plyr_stats$`Completed Tasks Percentage` = plyr_stats$`All Tasks Completed`/plyr_stats$`Times Crewmate`
plyr_stats$`Ejection Percentage` = plyr_stats$`Times Ejected`/plyr_stats$`Games Finished`
plyr_stats$`Death Percentage` = plyr_stats$`Times Murdered`/plyr_stats$`Times Crewmate`
plyr_stats$`Kills Per Round` = plyr_stats$`Imposter Kills`/plyr_stats$`Times Imposter`
plyr_stats$`Imposter Win Percentage` = (plyr_stats$`Imposter Kill Wins` + 
                                        plyr_stats$`Imposter Sabotage Wins` + 
                                        plyr_stats$`Imposter Vote Wins`)/plyr_stats$`Times Imposter`
plyr_stats$`Crewmate Win Percentage` = (plyr_stats$`Crewmate Task Wins` + 
                                        plyr_stats$`Crewmate Vote Wins`)/plyr_stats$`Times Crewmate`
plyr_stats$`Win Percentage` = (plyr_stats$`Imposter Win Percentage` * plyr_stats$`Times Imposter` + 
                               plyr_stats$`Crewmate Win Percentage` * plyr_stats$`Times Crewmate`)/plyr_stats$`Games Finished`

# enter Username and UserID here for user level graphs
user_df = plyr_stats[which(plyr_stats$Username == Username & plyr_stats$UserID == UserID),]

tidy_df <- gather(user_df, variable, value, colnames(user_df))
# add column describing whether variable is associated with crewmate, imposter, neither
tidy_df$role <- ifelse(grepl(paste(c("Imposter", "Kill"), collapse = "|"), tidy_df$variable), "imp",
                       ifelse(grepl(paste(c("Crewmate", "Task", "Death", "Murdered"), collapse = "|"), tidy_df$variable), "crw", 
                              NA))


# add column describing whetehr variable is associated with crewmate win, imposter win, or neither
tidy_df$role_win <- ifelse(grepl(paste(c("Imposter", "Win"), collapse = ".*"), tidy_df$variable), "imp",
                           ifelse(grepl(paste(c("Crewmate", "Win"), collapse = ".*"), tidy_df$variable), "crw", 
                                  NA)) 

#For Counts Graph
tidy_df_num <- tidy_df[5:22,]
tidy_df_num <- tidy_df_num %>% arrange(role)
tidy_df_num$value <- as.numeric(tidy_df_num$value)
tidy_df_num$variable <- factor(tidy_df_num$variable, levels = tidy_df_num$variable)

#For Rates Graph
tidy_df_rate <- tidy_df[c(25, 29),]
tidy_df_rate <- tidy_df_rate %>% arrange(role)
tidy_df_rate$value <- as.numeric(tidy_df_rate$value)
tidy_df_rate$variable <- factor(tidy_df_rate$variable, levels = tidy_df_rate$variable)

#For Percentage Graphs
tidy_df_perc <- tidy_df[c(23:24, 26:28, 30:32),]
tidy_df_perc <- tidy_df_perc %>% arrange(role)
tidy_df_perc$value <- as.numeric(tidy_df_perc$value)
tidy_df_perc$variable <- factor(tidy_df_perc$variable, levels = tidy_df_perc$variable)



ggplot(tidy_df_num) +
  geom_bar(aes(x = variable, y=value, fill=role), stat="identity")+
  geom_text(aes(variable, value, label=round(value, 3)), position=position_dodge(width=0.9), 
            vjust=-.25, size=3) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  scale_fill_manual(values=c("blue", "red"), na.value = "purple",
                  name="Role",
                  breaks=c("crw", "imp", NA),
                  labels=c("Crewmate", "Imposter", "NA"))

ggplot(tidy_df_rate) +
  geom_bar(aes(x = variable, y=value, fill=role), stat="identity")+
  geom_text(aes(variable, value, label=round(value, 3)), position=position_dodge(width=0.9), 
            vjust=-.25, size=3) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  scale_fill_manual(values=c("blue", "red"), na.value = "purple",
                    name="Role",
                    breaks=c("crw", "imp", NA),
                    labels=c("Crewmate", "Imposter", "NA"))


ggplot(tidy_df_perc) +
  geom_bar(aes(x = variable, y=value, fill=role), stat="identity")+
  geom_text(aes(variable, value, label=round(value, 3)), position=position_dodge(width=0.9), 
            vjust=-.25, size=3) +
  theme(axis.text.x = element_text(angle=60, hjust=1)) +
  scale_fill_manual(values=c("blue", "red"), na.value = "purple",
                    name="Role",
                    breaks=c("crw", "imp", NA),
                    labels=c("Crewmate", "Imposter", "NA"))

# for population level statistics
plyr_stats_2 <- plyr_stats %>% mutate(`Mobile or CPU?`=recode(`Mobile or CPU?`,
                                              "Mobile"=1,
                                              "CPU" = 2))
plyr_stats_2 <- plyr_stats_2 %>% dplyr::rename(
                                        TDRG = `Tasks Done Per Game`,
                                        CTP = `Completed Tasks Percentage`,
                                        IWP = `Imposter Win Percentage`,
                                        CWP = `Crewmate Win Percentage`,
                                        CP = `Crewmate Percentage`,
                                        IP = `Imposter Percentage`,
                                        KPR = `Kills Per Round`,
                                        WP = `Win Percentage`
                                        )
crw_scatmat <- plyr_stats_2 %>% dplyr::select(`Mobile or CPU?`, `TDRG`, CTP, 
                                        `Death Percentage`, `CP`, `CWP`, `Bodies Reported`, `WP`, 
                                        `Games Finished`)
pairs(crw_scatmat[, 2:8], col=ifelse(crw_scatmat$`Mobile or CPU?`== 1, "black", "red"))
#plot(crw_scatmat)

imp_scatmat <- plyr_stats_2 %>% dplyr::select(`Mobile or CPU?`, `IWP`, `IP`, 
                                              `WP`, `Emergencies Called`, `CWP`, `Bodies Reported`, 
                                              `Games Finished`)
pairs(imp_scatmat[, 2:8], col=ifelse(imp_scatmat$`Mobile or CPU?`== 1, "black", "red"))
#plot(imp_scatmat)


ggplot(data=plyr_stats, aes(`Mobile or CPU?`, fill=`Mobile or CPU?`))+
  geom_bar() +
  ggtitle("Mobile vs CPU Players")


ggplot(data=plyr_stats, aes(Color))+
  geom_bar() +
  ggtitle("Player Colors")







