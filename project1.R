library(dplyr)
library(tidyr)

ogc <- read.csv("oilgascounty.csv")
ogc <- data.frame(ogc, stringsAsFactors = FALSE)

a <- which(colnames(ogc)== "gas2000")
b <- which(colnames(ogc)== "oil_change_group")

#drop the columns that not needed
oc <- ogc[,c(1:(a-1),(b):(b))]
#rename
names(oc)[4] <- "county"
names(oc)[5] <- "rucc"
names(oc)[6] <- "ui"
names(oc)[7] <- "mn"
names(oc)[8] <- "mmn"

#drop county
oc<- select(oc, FIPS:Stabr, rucc:oil_change_group)
#subset that only contains oilyear
oilyears<- select(oc, oil2000:oil2011)
#sum <- summarise_each(oilyears, funs(mean))

#mean of each row in oilyears
oilyears$means <- apply(oilyears, 1, mean)
# mean of each column in oilyears
apply(oilyears, 2, mean)
#new colum in oc, sum 
oc$sum <- rowSums( oc[,8:19] )
#subset of rows with nonzero sums
nonzero <- filter(oc,oc$sum!= 0 )

#ma <- mutate(oilyears, oil2000=(gsub("oil2000","",oil2000)))
#new <- data_frame(ma)

library(ggplot2)
#find out which state to study
summary(oc$Stabr)
#filter data
TX <- filter(oc,oc$Stabr == 'TX' )

g <- ggplot(TX, aes(FIPS, sum))+geom_point(color="firebrick") + geom_smooth(method = lm)
model <- lm(formula = sum ~ FIPS, data = TX)

#scatterplot 
plot(TX$sum, TX$FIPS)

ls.str(nonzero)
summary(TX$oil_change_group)
summary(lm(sum~oil2005, data=TX))
s <- summary(TX, measurevar="sum", groupvars=c("geoid","mn"))
ggplot(TX, aes(x=oil2000))+ geom_histogram(binwidth=.5, colour="black", fill="white")
ggplot(TX, aes(x=oil2000))+ geom_density()


set.seed(200)
g <- ggplot(nonzero, aes(FIPS, sum))+geom_point(color="firebrick") + geom_smooth(method = lm)
model <- lm(formula = sum ~ FIPS, data = nonzero)

AR <- filter(demographs, Stabr== "AR"
for()
score<- filter(demographics, FIPS==st_fips[i]$Rural_Urban_Contimuum_Code_20)

make_index_matrix <- function(st_fips){
  og_ru_score = NULL
  not_ru_score = NULL
  for(i in 1:length(st_fips)){
    oil = min(filter(ogc))
  }
}