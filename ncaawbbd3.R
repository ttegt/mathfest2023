library(expm)
library(stringr)
library(sna)

# Retrieve DIII WBB 2022-23 schedule and team names & IDs from masseyratings.com,
# extract needed columns, name columns, and remove unplayed games

ncaambb<-read.csv("https://masseyratings.com/scores.php?s=500055&sub=11620&all=1&mode=3&sch=on&format=1",header=F)
ncaambb<-ncaambb[,c(3,5,6,8)]
names(ncaambb)<-c("Winner","Winning.Score","Loser","Losing.Score")
ncaambb<-ncaambb[ncaambb$Winning.Score>0,]

teams<-read.csv("https://masseyratings.com/scores.php?s=500055&sub=11620&all=1&mode=3&sch=on&format=2",header=F)
names(teams)<-c("Team.ID","Team")
teams$Team<-trimws(teams$Team)

# Loop to create column adjacency matrix, with row index for winner and column
# index for the loser. If ties are involved, they can be dealt with here, too.

adjmatrix<-matrix(0,nrow = nrow(teams),ncol=nrow(teams))
# ties<-rep(0,length(teams))
for(i in 1:nrow(ncaambb)) {
  adjmatrix[ncaambb$Winner[i],ncaambb$Loser[i]]<-adjmatrix[ncaambb$Winner[i],ncaambb$Loser[i]]+1
  
}

# The wins and losses for each team are given by the row and column sums,
# respectively. This also creates a basic standings table

wins<-rowSums(adjmatrix)
losses<-colSums(adjmatrix)
standings<-data.frame(Team=teams[,2],W=wins,L=losses)

# These lines introduce the oracle up and down vectors. The vector
# 'oup' is the number of edges from the teams to the oracle. Here it is one for 
# each team. The vector 'odown' is the number of edges from the oracle to each
# team. Here it is the number of wins + 1. Both of these can be customized.
# They are added to the original matrix. The final command makes the
# new matrix column stochastic.

oup<-rep(1,nrow(teams))
odown<-c(wins+1,0)
om1<-rbind(adjmatrix,oup)
om2<-cbind(om1,odown)
omatrix<-make.stochastic(om2,mode="col")

# The oracle ratings are given by the stationary probability vector for 
# the transition matrix. One easy way to find it is by raising the transition 
# matrix to a high power. Eventually the columns should agree to the displayed
# number of digits. The oracle rating is removed, and the remaining ratings
# are scaled so that the largest rating is 1. (This is just to avoid tiny
# ratings when there are lots of teams.)

orate<-omatrix%^%100
ratingsvector<-orate[1:nrow(teams),1]
ratingsvector<-ratingsvector/max(ratingsvector)
standings$rating<-ratingsvector

# The rankings are determined and added to the standings. The columns are 
# reordered so that the rankings come first.

standings$rank<-rank(-standings$rating)
standings<-standings[,c(5,1,2,3,4)]
standings[order(-standings$rating),]



