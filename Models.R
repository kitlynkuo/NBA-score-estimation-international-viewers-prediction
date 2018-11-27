install.packages("corrplot")
install.packages("leaps")
library(ggplot2)
library(GGally)
library(corrplot)
library(plm)
library(MASS)

cor(temp, method = "spearman")
temp <- as.matrix(temp)
corrplot(cor(temp))
check <- cor(temp)
temp <- data[, c(6:15)]

# full model
reg1_full <- lm(total_viewers~ Score_Diff + Timeout_Duration + Q4_Score_Diff + L2M_Score_Diff + Wins_Avg_H + Losses_Avg_H + Ties_Avg_H + Wins_Avg_A + Losses_Avg_A + Ties_Avg_A + Day + Is_Weekend + Is_Festival
                 + market_share_A + market_share_H + ASG_Players + ASG_Points + ASG_Mins + Field_Goals + Field_Goals_Attempted + Attempted_3pts + Success_3pts + ASG_Fan_Votes + ASG_Player_Votes +ASG_Media_Votes + Foreigners, data = data)
reg1_players <- lm(total_viewers~ ASG_Players, data = data)

summary(reg1_full)

# consider correlation (wins & loss)
reg2 <- lm(total_viewers~ Score_Diff + Timeout_Duration + Q4_Score_Diff + L2M_Score_Diff + Wins_Avg_H*Losses_Avg_H + Ties_Avg_H + Wins_Avg_A*Losses_Avg_A + Ties_Avg_A + Day + Is_Weekend + Is_Festival
                + market_share_A + market_share_H + ASG_Players + ASG_Points + ASG_Mins + Field_Goals + Field_Goals_Attempted + Attempted_3pts + Success_3pts, data = data)
summary(reg2)

step.reg2 <- stepAIC(reg2, direction = "both", 
                      trace = FALSE)
summary(step.reg2)
# remove weekends/ Q4_Score_Diff /Score_Diff / asg related

# consider correlation (score & time duration)
reg2 <- lm(total_viewers~ Score_Diff*Q4_Score_Diff + Timeout_Duration*L2M_Score_Diff + Wins_Avg_H*Losses_Avg_H + Ties_Avg_H + Wins_Avg_A*Losses_Avg_A + Ties_Avg_A + Day + Is_Weekend + Is_Festival
                    + market_share_A + market_share_H + ASG_Players + ASG_Points + ASG_Mins + Field_Goals + Field_Goals_Attempted + Attempted_3pts + Success_3pts + ASG_Fan_Votes + ASG_Player_Votes +ASG_Media_Votes + Foreigners , data = data)
summary(reg2_training)
step.reg2_training <- stepAIC(reg2_training, direction = "both", 
                              trace = FALSE)

### checking if the 2nd model is better
anova(reg1_training , reg2_training)


cor_asg <- lm(training_n_1400$ASG_Players ~ training_n_1400$ASG_Fan_Votes)
summary(cor_asg)

# consider correlation (ASG player & ASG fan votes)
reg3 <- lm(total_viewers~ Score_Diff*Q4_Score_Diff + Timeout_Duration*L2M_Score_Diff + Wins_Avg_H*Losses_Avg_H + Ties_Avg_H + Wins_Avg_A*Losses_Avg_A + Ties_Avg_A + Day + Is_Weekend + Is_Festival
                    + market_share_A + market_share_H + ASG_Players*ASG_Fan_Votes + ASG_Points + ASG_Mins + Field_Goals + Field_Goals_Attempted + Attempted_3pts + Success_3pts  + Foreigners , data = training_n_1400)
summary(reg3)
step.reg3 <- stepAIC(reg3, direction = "both", 
                              trace = FALSE)
summary(step.reg3)

# eliminate poor indicators (attempt/ASG mins)
reg4 <- lm(total_viewers~ Score_Diff*Q4_Score_Diff + Timeout_Duration*L2M_Score_Diff + Wins_Avg_H*Losses_Avg_H + Ties_Avg_H + Wins_Avg_A*Losses_Avg_A + Ties_Avg_A + Day + Is_Weekend + Is_Festival
                    + market_share_A + market_share_H + ASG_Players*ASG_Fan_Votes + ASG_Points  + Field_Goals +  Success_3pts  + Foreigners , data = data)
summary(reg4)
step.reg4 <- stepAIC(reg4, direction = "both", 
                              trace = FALSE)
summary(step.reg4)
