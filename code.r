library(tidyverse)

library(UsingR)


library(ggplot2)

library(readxl)


Add_RoundWins<-function(Score)
{
  x<-unlist(strsplit(Score,split ='-'))[1]
  
  return(x)
}

Add_RoundLoss<-function(Score)
{
  x<-unlist(strsplit(Score,split ='-'))[2]
  
  return(x)
}

Add_Round_Difference<-function(RW,RL)
{
  return(RW-RL)
}
# Load Team Big Data Excel 

Team_Big_Data <- read_excel("C:/Users/rowid/Desktop/Team Big Data.xlsx")


head(Team_Big_Data)



# Manpulation Part Start #####################################################

# Format each game date

Team_Big_Data<-Team_Big_Data%>%mutate(Match_Date=as.Date(Date,format ="%d/%m/%y"))

# remove old Date column
Team_Big_Data<-Team_Big_Data[-2]
 

# Add Round Win/Lose columns 

Team_Big_Data<-Team_Big_Data%>%mutate(RW=sapply(X=Score,FUN =Add_RoundWins))


Team_Big_Data<-Team_Big_Data%>%mutate(RL=sapply(X=Score,FUN =Add_RoundLoss))




# convert Types of RW and RL 

Team_Big_Data$RW<-as.integer(Team_Big_Data$RW)

Team_Big_Data$RL<-as.integer(Team_Big_Data$RL)



Team_Big_Data<-Team_Big_Data%>%mutate(RD=mapply(FUN=Add_Round_Difference,Team_Big_Data$RW,Team_Big_Data$RL)) 


Team_Big_Data$RD<-as.integer(Team_Big_Data$RD)



# Add State column

Team_Big_Data<-Team_Big_Data%>%mutate(State=ifelse(Team_Big_Data$RD>0,"W","L"))

Team_Big_Data$State<-as.factor(Team_Big_Data$State)

# Manpulation Part End #####################################################



# analysing Data ###########################################

# show general summary about the columns 
summary(Team_Big_Data)


# show Wins and loss for each Map

 

Team_State_Table<-table(Team_Big_Data$`Map Name`,Team_Big_Data$State)

Team_WinRate_Map<-as.data.frame(prop.table(Team_State_Table[,c('L','W')],margin = 1))

Team_WinRate_Map<-spread(Team_WinRate_Map,Var2,Freq)

colnames(Team_WinRate_Map)<-c("Map Name","Loss Rate","Win Rate")

Team_WinRate_Map%>%ggplot(aes(x=`Map Name`,y=`Win Rate`,fill=`Win Rate`))+
  geom_bar(stat = "identity",alpha=0.7)+theme_minimal()  +labs(title = "WinRate per map")

#show round difference distribution per map 

Team_Big_Data%>%ggplot(aes(`Map Name`,RD,colour=`Map Name`,size=RD))+
      geom_boxplot() + labs(y="Round Difference",title = "Round difference  per Map")















