
library(data.table)
library(dplyr)
library(reshape)

TourneySeeds <- fread("TourneySeeds.csv")
SampleSubmission <- fread("SampleSubmission.csv")
Seasons <- fread("Seasons.csv")
Teams <- fread("Teams.csv")
TourneySlots <- fread("TourneySlots.csv")
TourneyDetailedResults <- fread("TourneyDetailedResults.csv")
TourneyCompactResults <- fread("TourneyCompactResults.csv")
RegularSeasonCompactResults <- read.csv("RegularSeasonCompactResults.csv")
RanksKenPom <- fread("RanksKenPom.csv")

TourneySeeds <- TourneySeeds %>% 
  mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed)) %>% select(Season, Team, SeedNum)

RegularSeasonCompactResults$home <- RegularSeasonCompactResults$Wloc
levels(RegularSeasonCompactResults$home) <- list("-1"="A", "1"="H", "0"="N")
RegularSeasonCompactResults$home <- 
  as.numeric(as.character(RegularSeasonCompactResults$home))

library(lme4)

games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, split = "_", names = c('season', 'team1', 'team2')))   
colnames(games.to.predict)[1] <- "Id"
games.to.predict$home <- 0


for (season in 2012:2016){
  sub1 <- RegularSeasonCompactResults %>% 
    filter(Season==season) %>% 
    mutate(team1=as.factor(Wteam), team2=as.factor(Lteam), outcome=1, ptdiff=Wscore/Lscore, dw=.99^(132-Daynum)) %>% 
    select(team1, team2, home, outcome, ptdiff, dw)
  sub2 <- RegularSeasonCompactResults %>% 
    filter(Season==season) %>% 
    mutate(team1=as.factor(Lteam), team2=as.factor(Wteam), home=-1*home, outcome=0, ptdiff=Lscore/Wscore, dw=.99^(132-Daynum)) %>% 
    select(team1, team2, home, outcome, ptdiff, dw)
  reg.results <- rbind(sub1, sub2)

    m.ptdiff <- lmer(ptdiff ~ home +  (1 | team1) + (1 | team2), data = reg.results, weight=dw) 
  pred.pt.diffs <- predict(m.ptdiff, games.to.predict[games.to.predict$season==season,], 
                           type="response")
  games.to.predict[games.to.predict$season==season,"point_diff_pred"]<- (pred.pt.diffs - 0.5)
}

for (season in 2012:2016){
  sub1 <- RegularSeasonCompactResults %>% 
    filter(Season==season) %>% 
    mutate(team1=as.factor(Wteam), team2=as.factor(Lteam), outcome=1, ptdiff=Wscore/Lscore) %>% 
    select(team1, team2, home, outcome, ptdiff)
  sub2 <- RegularSeasonCompactResults %>% 
    filter(Season==season) %>% 
    mutate(team1=as.factor(Lteam), team2=as.factor(Wteam), home=-1*home, outcome=0, ptdiff=Lscore/Wscore) %>% 
    select(team1, team2, home, outcome, ptdiff)
  reg.results <- rbind(sub1, sub2)
  m.bt <- glmer(outcome ~ home +  (1 | team1) + (1 | team2), data = reg.results, family=binomial()) 
  games.to.predict[games.to.predict$season==season,"bt_pred"]<- predict(m.bt, games.to.predict[games.to.predict$season==season,], 
                                                                        type="response")
}

temp <- left_join(games.to.predict, RanksKenPom, by=c("season"="Year", "team1"="Team"))
games.to.predict <- left_join(temp, RanksKenPom, by=c("season"="Year", "team2"="Team"))


colnames(games.to.predict)[which(names(games.to.predict) %in% c("Pyth.x", "Pyth.y" ))] <- c("Team1Pyth", "Team2Pyth")
colnames(games.to.predict)[which(names(games.to.predict) %in% c("Rank.x", "Rank.y" ))] <- c("Team1Rank", "Team2Rank")

games.to.predict <- games.to.predict %>% mutate(Team1Rank = as.numeric(Team1Rank), Team2Rank = as.numeric(Team2Rank), Team1Pyth = as.numeric(Team1Pyth), Team2Pyth = as.numeric(Team2Pyth))

games.to.try = games.to.predict

games.to.try <- games.to.try %>% mutate(rank.pred = 0.5 + 0.014*(Team2Rank - Team1Rank))
games.to.try = games.to.try %>% na.omit()
games.to.try[games.to.try$rank.pred > 0.9, "rank.pred"] = 0.9
games.to.try[games.to.try$rank.pred < 0.1, "rank.pred"] = 0.1

TrnyRes <- TourneyCompactResults %>% filter(Season > 2011) %>% filter(Season < 2016) %>% select(Season, Wteam, Lteam) %>% mutate(Team1Win = 1)
TrnyResLos <- TrnyRes %>% mutate(Team1 = Lteam, Team2 = Wteam, Result = 0) %>% select(Season, Team1, Team2, Result) 
TrnyRes <- TrnyRes %>% mutate(Season = Season, Team1 = Wteam, Team2 = Lteam, Result = Team1Win) %>% select(Season, Team1, Team2, Result) 
TrnyRes <- rbind(TrnyRes, TrnyResLos)

data.for.gbm <- games.to.try %>% select(Id, season, team1, team2, Team1Rank, Team1Pyth, Team2Rank, Team2Pyth, point_diff_pred, bt_pred, rank.pred)
gbm.train <- data.for.gbm %>% filter(season < 2016)
gbm.test <- data.for.gbm %>% filter(season == 2016)

gbm.train.sample <- left_join(gbm.train, TrnyRes, by=c("season"="Season", "team1"="Team1", "team2"="Team2"))
gbm.train.sample = gbm.train.sample %>% na.omit()


#random forest
library(gbm)
gbm.model <- gbm(Result ~ (point_diff_pred + bt_pred) + (Team2Rank - Team1Rank),
              data=gbm.train.sample, distribution = "gaussian", interaction.depth = 2, shrinkage=0.05, n.trees=1000, cv.folds = 10)
gbm.test$Result <- predict(gbm.model, gbm.test, n.trees = 1000)
gbm.test$Result<- ifelse(gbm.test$Result>1,1,gbm.test$Result)
gbm.test$Result <- ifelse(gbm.test$Result<0,0,gbm.test$Result)

gbm.test <- gbm.test %>% mutate(Pred = 0.5*rank.pred + 0.5*Result)
write.csv(gbm.test %>% select(Id, Pred), 'Sample_Submission.csv', row.names=FALSE)

