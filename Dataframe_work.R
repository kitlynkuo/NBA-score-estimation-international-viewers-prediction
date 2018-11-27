# CHANGELOG
# V3: Add player variables from other sources
# V2: Added player variables


rm(list = ls())
setwd("C:\\Users\\himan\\Desktop\\Business Analytics-20180704T204656Z-001\\Business Analytics")
library(data.table)
library(openxlsx)


# Magic values
FTD <- 100
STD <- 20
TEAMS <- 2
WEEKEND <- c("Fri", "Sat", "Sun")
Fest1 <- 15
Fest2 <- 365 - 15


# Get the game variables at game ID level
# Difference in scores
# Total timeout duration
# Number of ties
# Average ties
# Average Wins
# Average Losses
# DIfference in Q4 scores
# Difference in L2M scores

# Dataset preparation
game <- read.csv("game_data.csv", stringsAsFactors = FALSE)
game <- na.omit(game)
game <- data.table(game)
game_home <- game[which(game$Location == 'H'),]
game_away <- game[which(game$Location == 'A'),]
colnames(game_home) <- paste(colnames(game_home), '_H', sep = "")
colnames(game_away) <- paste(colnames(game_away), '_A', sep = "")
game <- merge(game_home, game_away, by.x = 'Game_ID_H', by.y = 'Game_ID_A', type = 'left')

# Getting team and season level average wins, losses and ties
game_home <- game_home[,j = list(mean(Wins_Entering_Gm_H), mean(Losses_Entering_Gm_H), mean(Ties_H)), by = list(Season_H, Team_H)]
colnames(game_home) <- c("Season_H", "Team_H", "Wins_Avg_H", "Losses_Avg_H", "Ties_Avg_H")

game_away <- game_away[,j = list(mean(Wins_Entering_Gm_A), mean(Losses_Entering_Gm_A), mean(Ties_A)), by = list(Season_A, Team_A)]
colnames(game_away) <- c("Season_A", "Team_A", "Wins_Avg_A", "Losses_Avg_A", "Ties_Avg_A")

game$Score_Diff <- abs(game$Final_Score_H - game$Final_Score_A)
game$Timeout_Duration <- (game$Full_Timeouts_H + game$Full_Timeouts_A) * FTD + (game$Short_Timeouts_A + game$Short_Timeouts_H) * STD
game$Q4_Score_Diff <- abs(game$Qtr_4_Score_A - game$Qtr_4_Score_H)
game$L2M_Score_Diff <- abs(game$L2M_Score_A - game$L2M_Score_H)

game <- game[,c("Game_ID_H", "Season_H", "Game_Date_H", "Team_H", "Team_A", "Score_Diff", "Timeout_Duration", "Q4_Score_Diff", "L2M_Score_Diff")]

game <- merge(game, game_home, by = c("Season_H", "Team_H"))
game <- merge(game, game_away, by.x = c("Season_H", "Team_A"), by.y = c("Season_A", "Team_A"))

# External Variables
# Is holiday
# Is festival
game$Game_Date_H <- as.Date(game$Game_Date_H, "%m/%d/%Y")
game$Day <- format(game$Game_Date_H, "%a")
game$Is_Weekend <- ifelse(game$Day %in% WEEKEND, TRUE, FALSE)
game$Is_Festival <- ifelse(as.numeric(format(game$Game_Date_H, "%j")) <= Fest1 | format(game$Game_Date_H, "%j") >= Fest2, TRUE, FALSE)

write.csv(game, "ADS_Game&Ext.csv", row.names = FALSE)
rm(game_away, game_home)

# Player vriables
# Weighted Average player points
# Minutes of top 3 players (based on points)
# Field goals
# Field goals attempted
# Success rate of 3 points (Scores / Attempted)
# ASG_Team

player <- readWorkbook("player_data_more_details.xlsx")
# player <- na.omit(player)
player <- data.table(player)

temp_player <- data.frame("Game_ID" = numeric(), "ASG_Players" = numeric(), "ASG_Points" = numeric(), "ASG_Mins" = numeric(), "Field_Goals" = numeric(), "Field_Goals_Attempted" = numeric(), "Attempted_3pts" = numeric(), "Suucess_3pts" = numeric(), "ASG_Fan_Votes" = numeric(), "ASG_Player_Votes" = numeric(), "ASG_Media_Votes" = numeric(), "Foreigners" = numeric())

games <- unique(player$Game_ID)
for(i in games){
  temp <- player[which(player$Game_ID == i),]
  foreign <- nrow(temp[which(temp$IS_foreigner == TRUE),])
  if(nrow(temp[which(temp$ASG_Team != 'None'),]) > 0){
    temp <- temp[which(temp$ASG_Team != 'None'),]
    ASG_count <- nrow(temp)
  }else{
    temp <- temp[order(-Points), ]
    temp <- temp[c(1:3), ]
    ASG_count <- 0
  }
  this_game <- c(i, ASG_count, sum(temp$Points, na.rm = TRUE), sum(temp$Minutes, na.rm = TRUE), sum(temp$Field_Goals, na.rm = TRUE), sum(temp$Field_Goals_Attempted, na.rm = TRUE), sum(temp$Three_Pointers, na.rm = TRUE), sum(temp$Three_Pointers_Attempted, na.rm = TRUE), sum(temp$Fans_votes, na.rm = TRUE), sum(temp$players_votes, na.rm = TRUE), sum(temp$Media_votes, na.rm = TRUE), foreign)
  temp_player <- rbind(temp_player, this_game)
}
colnames(temp_player) <- c("Game_ID", "ASG_Players", "ASG_Points", "ASG_Mins", "Field_Goals", "Field_Goals_Attempted", "Attempted_3pts", "Success_3pts", "ASG_Fan_Votes", "ASG_Player_Votes", "ASG_Media_Votes", "Foreigners")
rm(temp, this_game, player, ASG_count)

Final_ADS <- merge(game, temp_player, by.x = "Game_ID_H", by.y = "Game_ID", all.x = TRUE)
write.csv(Final_ADS, "ADS_NBA_BA.csv", row.names = FALSE)


# Create test dataset

test_data <- read.csv('test_set.csv', stringsAsFactors = FALSE)
test_data$Total_Viewers <- NULL
test_data <- merge(test_data, Final_ADS, by.x = c('Home_Team', 'Away_Team'), by.y = c('Team_H', 'Team_A'), all.x = TRUE)
test_data <- data.table(test_data)

test_data_aggregated <- test_data[, j = list(mean(Score_Diff, na.rm = TRUE),
                                  mean(Timeout_Duration, na.rm = TRUE),
                                  mean(Q4_Score_Diff, na.rm = TRUE),
                                  mean(L2M_Score_Diff, na.rm = TRUE),
                                  max(Wins_Avg_H, na.rm = TRUE),
                                  max(Wins_Avg_A, na.rm = TRUE),
                                  min(Losses_Avg_H, na.rm = TRUE),
                                  min(Losses_Avg_A, na.rm = TRUE),
                                  mean(ASG_Players, na.rm = TRUE),
                                  mean(ASG_Points, na.rm = TRUE),
                                  mean(ASG_Mins, na.rm = TRUE),
                                  mean(Field_Goals, na.rm = TRUE),
                                  mean(Field_Goals_Attempted, na.rm = TRUE),
                                  mean(Attempted_3pts, na.rm = TRUE),
                                  mean(Success_3pts, na.rm = TRUE),
                                  max(ASG_Fan_Votes, na.rm = TRUE),
                                  max(ASG_Media_Votes, na.rm = TRUE),
                                  max(ASG_Player_Votes, na.rm = TRUE),
                                  mean(Foreigners, na.rm = TRUE)),
                       by = list(Home_Team, Away_Team, Game_Date, Game_ID, Season)]
colnames(test_data_aggregated) <- c("Home_Team", "Away_Team", "Game_Date", "Game_ID", "Season", "Score_Diff", "Timeout_Duration",
                                    "Q4_Score_Diff", "L2M_Score_Diff", "Wins_Avg_H", "Wins_Avg_A", "Losses_Avg_H",
                                    "Losses_Avg_A", "ASG_Players", "ASG_Points", "ASG_Mins", "Field_Goals",
                                    "Field_Goals_Attempted", "Attempted_3pts", "Success_3pts", "ASG_Fan_Votes",
                                    "ASG_Media_Votes", "ASG_Player_Votes", "Foreigners")

write.csv(test_data_aggregated, "Test_Data_V1.csv", row.names = FALSE)

