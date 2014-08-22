# script to calculate standardized olympic medal ranking
# January 25, 2013

#-------------------------------------------------------------------------------------
# Function to remove leading and trailing white space from character strings
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#-------------------------------------------------------------------------------------
tbl<-read.table("R/olympicmedals/olympics.txt", sep=",", 
	colClasses=c("character", rep("numeric",4)),
	col.names=c("country","gold","silver","bronze","total") )
#	tbl$country<-sub(" \\(.*","",tbl$country) # remove parentheses at end (and extra space)
#	tbl$country<-trim(tbl$country)

tbl2<-read.table("R/olympicmedals/cntrypops.txt", sep=",",
	col.names=c("country","pop"), colClasses=c("character","numeric"))
#	tbl2<-tbl2[,c(-4:-6)]
#	names(tbl2)<-c("country","pop")
#	tbl2$country<-sub("\\[.*","",tbl2$country) # remove wikipedias footnote link
#	tbl2$country<-sub(" \\(.*","",tbl2$country) # remove wikipedias footnote link
#	tbl2$country<-trim(tbl2$country)

###gdp per capita as purchasing power parity index
tblg <- read.table("R/olympicmedals/gdp1.txt", sep=",", strip.white=TRUE,
	col.names=c("country","gdp"), colClasses=c("character","numeric") )

###gdp per capita
tblg2 <- read.table("R/olympicmedals/gdp2.txt", sep=",", strip.white=TRUE,
	col.names=c("country","gdp.nom"), colClasses=c("character","numeric") )

### % living on less than $1.25 & $2/day
tblp1 <- read.table("R/olympicmedals/pov1.txt", sep=",", strip.white=TRUE,
	col.names=c("country","pov1.25","pov2"), colClasses=c("character","numeric","numeric") )

### % living under national poverty line, two sources (using only one now)
tblp2 <- read.table("R/olympicmedals/pov2.txt", sep=",", strip.white=TRUE, fill=TRUE,
	col.names=c("country","cpov","cpov2"), colClasses=c("character","numeric","numeric") )
tblp <- merge(tblp1,tblp2,all=TRUE)
tblp$pov1.25 <- NULL; tblp$cpov2 <- NULL

tbl3<-merge(tbl,tbl2)
tbl3 <- merge(tbl3, tblg)
tbl3 <- merge(tbl3, tblp, all.x=TRUE)
#pop in millions
tbl3$pop <- round( tbl3$pop/10^6, 3)

###################
#### poverty variables incomplete, therfore below is an attempt to fill it in
####european countries based on other european countries
weurope <- c(9,10,13,17,19,20,21,26,27,30,31,35,39,40,46,47,
	52,54,57,58,59,63,67,68,71,72,73,81,82)
fish <- lm(pov2 ~ cpov, data=tbl3, subset=weurope) #no trend, therefore random (and ~normal)
these <- is.na(tbl3$pov2[weurope])
tbl3 <- within(tbl3, pov2[weurope[these]] <- 
	abs(round(rnorm(sum(these), mean(pov2[weurope[!these]]), sd(pov2[weurope[!these]])),2)) )
these <- is.na(tbl3$cpov[weurope])
tbl3 <- within(tbl3, cpov[weurope[these]] <- 
	abs(round(rnorm(sum(these), mean(cpov[weurope[!these]]), sd(cpov[weurope[!these]])),1)) )
####
nas <- is.na(tbl3$pov2)
tbl3[nas,-c(2:5)]
####other missing "western countries" use european countries
west <- c(5,34,55,66)
tbl3 <- within(tbl3, cpov[west] <- 
	abs(round(rnorm(4, mean(cpov[weurope]), sd(cpov[weurope])),1)) )
west <- c(5,14,34,42,55,66,70,83)
tbl3 <- within(tbl3, pov2[west] <- 
	abs(round(rnorm(8, mean(pov2[weurope]), sd(pov2[weurope])),2)) )
####
nas <- is.na(tbl3$pov2)
tbl3[nas,-c(2:5)]
####tropical islands
islands <- c(7,18,22,32,41,60,77)
tbl3[islands,-c(2:5)]
tbl3$cpov[c(18,60)] <- c(35.0,15)
tbl3$pov2[c(7,18,32,60)] <- c(4.87,9.45,15.63,7.22)
####
nas <- is.na(tbl3$pov2)
tbl3[nas,-c(2:5)]
####other - middle east and eastern asia
asia <- c(1,4,6,8,15,23,29,36,37,38,43,45,48,50,51,56,61,64,66,74,76,79,84)
tbl3[asia,-c(2:5)]
tbl3$pov2[c(1,51,74,84)] <- c(40.47,19.31,2.83,21.48)
tbl3$cpov[c(8,45,56,61,64)] <- c(14.2,8.5,24,6.2,16.7)
tbl3$pov2[c(8,45,56,61,64)] <- c(4.75,4.32,26.2,2.44,7.56)

rm(fish,islands,islands2,asia,nas,west,weurope)

#cleaning up columns: first population
#tbl3$pop<-as.character(tbl3$pop)
#tbl3$pop<-sub("\\,","",tbl3$pop)
#tbl3$pop<-sub("\\,","",tbl3$pop)
#tbl3$pop<-sub("\\,","",tbl3$pop)
#tbl3$pop<-sub("\\s+$","",tbl3$pop)
#tbl3$pop<-as.numeric(tbl3$pop)

###############
#rankings
w1 <- c(8,5,3); w2 <- c(3,2,1)
tbl3$rank1 <- trunc( nrow(tbl3)+1 - rank(tbl3$total) ) 
tbl3$rank2 <- trunc( nrow(tbl3)+1 - rank(as.matrix(tbl3[,2:4]) %*% w1) )
tbl3$rank3 <- trunc( nrow(tbl3)+1 - rank(as.matrix(tbl3[,2:4]) %*% w2) )

tbl3$score2 <- as.matrix(tbl3[,2:4]) %*% w1
tbl3$score3 <- as.matrix(tbl3[,2:4]) %*% w2

tbl3$score4 <- round( (as.matrix(tbl3[,2:4]) %*% w1)/ tbl3$pop, 3)
#make score sum to total medals i.e. #of medals won if all had same pop
tbl3$score4 <- round(sum(tbl3$total)/sum(tbl3$score4) * tbl3$score4)
tbl3$rank4 <- trunc( nrow(tbl3)+1-rank(tbl3$score4) )

tbl3
tbl3[order(tbl3$rank1)[1:50],
    c("country", "total", "score5", "rank1", "rank3", "rank4")]


######################

#european countries
europe2 <- match(c(
    "Armenia", "Azerbaijan", "Belarus", "Belgium", "Bulgaria", "Croatia", 
    "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
    "Georgia", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia",
    "Lithuania","Moldova", "Montenegro", "Netherlands","Norway", "Poland", 
    "Portugal", "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", 
    "Spain", "Sweden", "Switzerland","Turkey", "Ukraine", "United Kingdom"),
    tblp$country)

#other mostly stable, not too poor countries
eurish <- match(c(
	"Argentina", "Australia", "Bahamas", "Brazil", "Canada", "Colombia", 
	"Cuba", "Dominican Rep", "Egypt", "Grenada", "Guatemala", "Hong Kong",
	"Jamaica", "Japan", "Kazakhstan", "Mexico", "Morocco",
	"Malaysia", "New Zealand","Puerto Rico", "Singapore", "South Korea", 
	"Taiwan", "Tajikistan", "Trinidad and Tobago", "Tunisia", 
	"United States", "Uzbekistan", "Venezuela"), tbl3$country)  


#### model european country medals
fit1 <- lm(total ~ pop, data=tbl3, subset=europe)
summary(fit1)
plot(tbl3$pop[europe], tbl3$total[europe]); abline(fit1)
identify(tbl3$pop, tbl3$total, tbl3$country, cex=0.5)

shapiro.test(fit1$res)
qplot <- qqnorm(fit1$res); qqline(fit1$res)
identify(qplot$x, qplot$y, tbl3$country[europe[-c(35,37)]], cex=0.5)

#### stations 35 and 37: Turkey and UK outliers & influential
fit2 <- lm(total ~ pop, data=tbl3, subset=europe[-c(35,37)])
summary(fit2)
plot(tbl3$pop[europe], tbl3$total[europe]); abline(fit2)

shapiro.test(fit2$res)
qqnorm(fit2$res); qqline(fit2$res)

#### model european and other stable countries with only population
#### outliers exist, but other than UK no reason to remove them
fit3 <- lm(total ~ pop, data=tbl3, subset=c(europe,eurish)[-37])
summary(fit3)
plot(tbl3$pop[c(europe,eurish)], tbl3$total[c(europe,eurish)])
abline(fit3)
identify(tbl3$pop,tbl3$total,tbl3$country,cex=0.5)

shapiro.test(fit3$res)
qplot <- qqnorm(fit3$res); qqline(fit3$res)
identify(qplot$x, qplot$y, tbl3$country[c(europe,eurish)[-37]], cex=0.5)

#### model european and other stable countries with pop and ppp estimate
fit4 <- lm(total ~ pop+gdp, data=tbl3, subset=c(europe,eurish)[-37])
summary(fit4)

shapiro.test(fit4$res)
qplot <- qqnorm(fit4$res); qqline(fit4$res)
identify(qplot$x, qplot$y, tbl3$country[c(europe,eurish)[-37]], cex=0.5)

#### model all countries with population and poverty %
#### pov2 (% living on less than $2/day) data incomplete, therefore this variable
#### was partially constructed from other variables
fit5 <- lm(total ~ 0+pop+pov2+povpov, data=tbl3, subset=-c(15,36))
summary(fit5)
shapiro.test(fit5$res)
qplot <- qqnorm(fit5$res); qqline(fit5$res)
identify(qplot$x, qplot$y, tbl3$country[-c(15,36)], cex=0.5)

plot(tbl3$pop,tbl3$total)
abline(fit5)
identify(tbl3$pop,tbl3$total,tbl3$country,cex=0.5)


#### countries with no data on poverty
"Australia", "Bahrain","Cuba","Cyprus","Finland","Hong Kong","Italy","Kuwait",
"New Zealand", "North Korea", "Norway","Puerto Rico","Qatar","Saudi Arabia","Singapore",
"Sweden"
