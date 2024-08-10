# Name: Sarvesh Gopalakrishnan 
# Main Goal: NBA Player Performance Prediction 
# Machine Learning Models: Lasso, Ridge, Elastic Net
# Report: Milwaukee Bucks' Roster Analysis
# Dataset: https://www.kaggle.com/datasets/sumitrodatta/nba-aba-baa-stats/code
# Date: 06/13/2024

library(tidyverse)
library(dplyr)
library(glmnet)
library(ggplot2)

# Set working directory of file
setwd("C:/Users/Sarvesh/Desktop/R Programs & Notes/NBA Data")

# Read in per game stats as a data frame
NBAPerGameOff <- as.data.frame(read.csv("PlayerPerGame.csv"))

# Read in shooting stats as a data frame
NBAShotStat <- as.data.frame(read.csv("PlayerShooting.csv"))

# Read in play by play stats as a data frame
NBAPBPStat <- as.data.frame(read.csv("PlayerPBP.csv"))

# Read in award shares as a data frame
NBAAwdShare <- as.data.frame(read.csv("PlayerAwdShare.csv"))

# Read in player defensive stats as a data frame 
NBAPerGameDef <- as.data.frame(read.csv("PlayerDefPerGame.csv"))

# Read in player advanced stats as a data frame 
NBAAdvancedStat <- as.data.frame(read.csv("Advanced.csv"))

# Selecting and renaming variables for  the NBAPerGameOff Data Frame 
NBAPerGameOff <- NBAPerGameOff %>% dplyr::select(-birth_year, -lg, -gs)

names(NBAPerGameOff) <- c("seas_id", "season", "player_id", "player" , "pos", 
                          "age", "exp", "team", "gp", "MPG", "FGMPG", "FGAPG", "FGPER", 
                          "x3PMPG", "x3PAPG", "x3PPER", "x2PMPG", "x2PAPG", "x2PPER", 
                          "EFGPER", "FTMPG", "FTAPG","FTPER", "ORBPG", "DRBPG", "TRBPG", 
                          "APG", "SPG", "BPG", "TOVPG", "PFPG", "PPG")

# Selecting and renaming variables for the NBAShotStat Data Frame
NBAShotStat <- NBAShotStat %>% dplyr::select(-lg, -mp, -birth_year)
names(NBAShotStat) <- c("seas_id", "season", "player_id", "player", "pos", "age", 
                        "exp", "team", "gp", "FGPER", "AVGDISTFGA", "PERFGA2PT",
                        "PERFGA0x3", "PERFGA3x10", "PERFGA10x16", 
                        "PERFGA16x3PT", "PERFGA3PT", "FGPER2PT",
                        "FGPER0x3", "FGPER3x10", "FGPER10x16", "FGPER16x3PT",
                         "FGPER3PT", "PERASST2PT", 
                        "PERASST3PT", "PERDUNKFGA", "NUMOFDUNKS", "PER3PTCOR",
                        "FG3PTCOR", "HEAVEATT", "HEAVEMADE")

# Selecting and renaming variables for the NBAPBPStat Data Frame
NBAPBPStat <- NBAPBPStat %>% dplyr::select(-birth_year, -mp, -lg, -bad_pass_turnover, 
                                           -lost_ball_turnover)

names(NBAPBPStat) <- c("seas_id", "season", "player_id", "player", "pos",
                       "age", "exp", "team", "gp", "PERPOSSPG", 
                       "PERPOSSSG", "PERPOSSSF", "PERPOSSPF", "PERPOSSC",
                       "ONPLMINPER100", "NETPLMINPER100", "SHOTFOULSCOMM", 
                       "OFFFOULCOMM", "SHOTFOULDRAWN", "OFFFOULDRAWN", "PTSGENASST", 
                       "AND1", "FGABLKS")

# Selecting and renaming variables for the NBAAwdShare Data Frame
names(NBAAwdShare) <- c("season", "award", "player", "age", "team", "x1stVotes", 
                        "PTSWON", "PTSMAX", "AWDSHARE", "WINNER", "seas_id", "player_id")

# Selecting and renaming variables for the NBAPerGameDef Data Frame
NBAPerGameDef <- NBAPerGameDef %>% dplyr::select(-lg, -abbreviation, -mp_per_game, -g)
# Opponent/Defense Stats
names(NBAPerGameDef) <- c("season", "team", "playofs", "DFGM", "DFGA", "DFG", 
                          "D3PM", "D3PA", "D3PER", "D2PM", "D2PA", "D2PER", "DFTM", 
                          "DFTA", "DFTPER", "DORB", "DDRB", "DTRB", "DAPG", "DSPG", 
                          "DBPG", "DTOV", "DPFG", "DPPG")

# Selecting and renaming variables for the NBAAdvancedStat Data Frame
NBAAdvancedStat <- NBAAdvancedStat %>% dplyr::select(-birth_year, -lg, -mp)
names(NBAAdvancedStat) <- c("seas_id", "season", "player_id", "player", "pos", 
                            "age", "exp", "team", "gp", "PER", "TS", "x3P_AR", "FTR",
                            "ORBPER", "DRBPER", "TRBPER", "ASTPER", "STLPER", 
                            "BLKPER", "TOVPER", "USGPER", "OWS", "DWS", "WS", 
                            "WS_48", "OBPM", "DBPM", "BPM", "VORP")

#-------------------------------------------------------------------------------

# Combining the data frames above into a master data frame with all player data 
# Excludes NBAPerGameDef since this data frame includes team defensive stats 

NBA_MasterDF <- NBAPerGameOff %>% left_join(NBAShotStat, by = c("seas_id", "season", "player_id", 
                                "player", "pos", "age", "exp", "team", "gp", "FGPER"))

NBA_MasterDF <- NBA_MasterDF %>% left_join(NBAPBPStat, by = c("seas_id", "season", "player_id", "player", "pos", 
                                                              "age", "exp", "team", "gp"))

NBA_MasterDF <- NBA_MasterDF %>% left_join(NBAAdvancedStat, by = c("seas_id", "season", "player_id", "player", 
                                                                   "pos", "age", "exp", "gp", "team"))

#-------------------------------------------------------------------------------

# Cleaning the NBA_MasterDF data frame so that it is usable for data manipulation and analysis

NBA_MasterDF <- NBA_MasterDF %>% dplyr::mutate(across(all_of(names(NBA_MasterDF)), ~ ifelse(is.na(.)| . == "NA" |
                                                                                              . == "N/A", 0, .)))

year1997Index <- which(NBA_MasterDF$season < 1997)

NBA_MasterDF <- NBA_MasterDF[-year1997Index, ]
NBA_MasterDF <- NBA_MasterDF %>% dplyr::select(-OFFFOULDRAWN)

# Standardizes predictor data
standardize_pred <- function(data)
{
  numericData <- data[ , 10:ncol(data)]
  
  means <- colMeans(numericData, na.rm = TRUE)
  sds <- apply(numericData, 2, sd, na.rm = TRUE)
  
  standardized_data <- scale(numericData, center = means, scale = sds)
  
  combined_data <- cbind(data[ , 1:9], standardized_data)
  
  return(combined_data)
}

target_vars <- c("PPG", "TRBPG", "APG", "SPG", "BPG", "FGPER", "TS")

predictor_vars <- setdiff(names(NBA_MasterDF), target_vars)
predictor_vars <- predictor_vars[10:length(predictor_vars)]

targets <- NBA_MasterDF %>% dplyr::select(all_of(target_vars))

standardized_DF <- as.data.frame(standardize_pred(NBA_MasterDF %>% dplyr::select(-all_of(target_vars))))
NBA_MasterDF_std <- cbind(standardized_DF, targets)

#-------------------------------------------------------------------------------

# PREDICTING PPG/RPG/APG/SPG/BPG/FG% FOR NBA PLAYERS

#-------------------------------------------------------------------------------

# BUILDING INITIAL MODEL

trainData <- function(targetVar, k, predSet)
{
  # Splits data into predictors and targets
  pred_data <- as.matrix(NBA_MasterDF_std %>% dplyr::select(all_of(predSet)))
  target_data <- as.matrix(NBA_MasterDF_std[ , targetVar])
  
  alphaNames <- c((0:10) / 10)
  
  storeModels <- vector("list", 11)
  mseVec <- rep(0, 11)
  
  storeIndMSE <- matrix(nrow = 11, ncol = 10)
  
  for (i in 1:11)
  {
    storeModels[[i]] <- vector("list", 10)
  }
  
  # 10 fold cross validation
  folds <- sample(1:k, nrow(NBA_MasterDF_std), replace = TRUE)
  
  for (fold in 1:k)
  {
    train_set <- which(folds != fold)
    test_set <- which(folds == fold)
    
    pred_train <- pred_data[train_set, ]
    pred_test <- pred_data[test_set, ]
    
    target_train <- target_data[train_set, ]
    target_test <- target_data[test_set, ]
    
    for (i in 0:10)
    {
      alpha.fit <- cv.glmnet(pred_train, target_train, type.measure = "mse", 
                             alpha = i/10, family = "gaussian")
      
      storeModels[[i + 1]][[fold]] <- alpha.fit
      
      alpha.predicted <- predict(alpha.fit, s = alpha.fit$lambda.1se, 
                                 newx = pred_test)
      
      alpha.predicted[alpha.predicted < 0] <- 0
      
      indMSE <- mean((target_test - alpha.predicted)^2)
      
      storeIndMSE[i + 1, fold] <- indMSE
      
      mseVec[i + 1] <- mseVec[i + 1] + indMSE
    }
  }
  
  mseVec <- mseVec / k
  modelDF <- data.frame(alphaNames, mseVec)
  print(modelDF)
  
  bestAlphaIndex <- which.min(mseVec)
  pullBestMSE <- storeIndMSE[bestAlphaIndex, ]
  bestFoldIndex <- which.min(pullBestMSE)
  
  bestModel <- storeModels[[bestAlphaIndex]][[bestFoldIndex]]
  
  # returns the model with the lowest MSE value
  return(bestModel)
}

# TRAINED MODELS FOR EACH TARGET VARIABLE

initialPred <- c(colnames(NBA_MasterDF_std)[10:ncol(NBA_MasterDF_std)])

removePPG <- which(initialPred == "PPG")
removeTRBPG <- which(initialPred == "TRBPG")
removeAPG <- which(initialPred == "APG")
removeSPG <- which(initialPred == "SPG")
removeBPG <- which(initialPred == "BPG")
removeFG <- which(initialPred == "FGPER")
removeTS <- which(initialPred == "TS")

initialPredPPG <- initialPred[-removePPG]
initialPredTRBPG <- initialPred[-removeTRBPG]
initialPredAPG <- initialPred[-removeAPG]
initialPredSPG <- initialPred[-removeSPG]
initialPredBPG <- initialPred[-removeBPG]
initialPredFG <- initialPred[-removeFG]
initialPredTS <- initialPred[-removeTS]

modelForPPG <- trainData("PPG", 10, initialPredPPG)
modelForRPG <- trainData("TRBPG", 10, initialPredTRBPG)
modelForAPG <- trainData("APG", 10, initialPredAPG)
modelForSPG <- trainData("SPG", 10, initialPredSPG)
modelForBPG <- trainData("BPG", 10, initialPredBPG)
modelForFG <- trainData("FGPER", 10, initialPredFG)
modelForTS <- trainData("TS", 10, initialPredTS)
#-------------------------------------------------------------------------------

# PREDICT TARGET VARIABLES (PPG, RPG, APG, SPG, BPG, FG, TS)

pred_season <- function(year)
{
  seasonIndex <- which(NBA_MasterDF_std$season == year)
  NBA_YR_DATA <- NBA_MasterDF_std[seasonIndex, ]
  NBA_YR_PRED <- NBA_YR_DATA
  NBA_YR_COPY <- NBA_YR_DATA
  
  targetVec <- c("PPG", "TRBPG", "APG", "SPG", "BPG", "FGPER", "TS")
  statVec <- c("PRED_PPG", "PRED_RPG", "PRED_APG", "PRED_SPG", 
  "PRED_BPG", "PRED_FGPER", "PRED_TS")
  
  statModel <- list(modelForPPG, modelForRPG, modelForAPG, modelForSPG, 
                 modelForBPG, modelForFG, modelForTS)
  
  for (i in 1:length(targetVec))
  {
    NBA_YR_PRED <- as.matrix(NBA_YR_COPY %>% dplyr::select(-all_of(targetVec[i]), -seas_id, -season, 
                                                                         -player_id, -player, -pos, -age, -exp, -team, -gp))
    indModel <- statModel[[i]]
    alpha1.predicted_season <- predict(indModel, s = indModel$lambda.1se, 
                                   newx = NBA_YR_PRED)
    
    alpha1.predicted_season[alpha1.predicted_season < 0] <- 0
    
    indStat <- statVec[i]
    
    for (j in 1:nrow(NBA_YR_DATA))
    {
      NBA_YR_DATA[j, indStat] <- alpha1.predicted_season[j]
    }
  }
  
  return(NBA_YR_DATA)
}

# Predicted data for 2023, 2024, and 2025
NBA_2022_PRED <- pred_season(2022)
NBA_2023_PRED <- pred_season(2023)
NBA_2024_PRED <- pred_season(2024)

#-------------------------------------------------------------------------------

# AGGREGATE 2023, 2024, and 2025 PREDICTIONS IF NEEDED
#   For now, only 2024 data is used to predict 2025

NBA_PRED_COMB_DATA <- rbind(NBA_2024_PRED)
NBA_MASTER_PRED <- data.frame()

player <- unique(NBA_PRED_COMB_DATA$player)

NBA_MASTER_PRED <- cbind(player)

team <- c()

# Team vector created for every player's most recent team
for (i in 1:nrow(NBA_MASTER_PRED))
{
  indPlayerIndex <- which(NBA_2024_PRED$player == NBA_MASTER_PRED[i, c("player")])
  indTeam <- NBA_2024_PRED$team[indPlayerIndex]
  
  if (length(indTeam) > 1)
  {
    team <- c(team, indTeam[length(indTeam)])
  }
  else if(length(indTeam) == 0)
  {
    team <- c(team, NA)
  }
  else
  {
    team <- c(team, indTeam)
  }
}

NBA_MASTER_PRED <- cbind(NBA_MASTER_PRED, team)

pullStatVec <- c("PRED_PPG", "PRED_RPG", "PRED_APG", "PRED_SPG", 
                 "PRED_BPG", "PRED_FGPER", "PRED_TS")

NBA_MASTER_PRED <- as.data.frame(NBA_MASTER_PRED)

# Predicted target statistics aggregated across years
for (j in 1:length(player))
{
  indPlayerIndex <- which(NBA_PRED_COMB_DATA$player == player[j])
  for (k in 1:length(pullStatVec))
  {
    indStatName <- pullStatVec[k]
    pullPREDSTAT <- NBA_PRED_COMB_DATA[indPlayerIndex, indStatName]
    avgPREDSTAT <- mean(pullPREDSTAT)
    NBA_MASTER_PRED[j, indStatName] <- avgPREDSTAT
  }
}

# naTeam <- which(is.na(NBA_MASTER_PRED$team)) --> check for NA if aggregating

# NBA_MASTER_PRED <- NBA_MASTER_PRED[-naTeam, ]

print(NBA_MASTER_PRED)