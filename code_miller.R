### Jessica Miller ###
### Kings Analysis ###
###    June 2017   ###

setwd("C:/Users/Elena/Desktop/Github/jessicamiller94-analyst-interview")

# Read in data
comp.data <- read.csv("complete_data.csv", header=T)
teams <- read.csv("teams.csv", header=T)
players <- read.csv("players.csv", header=T)

## Who is the best passer?
## Find this out by looking at every player's pass
## compared to whether or not the shot was eventually made.

# Sort dataset by passer
passers <- comp.data[order(comp.data$passer),]
attach(passers)

sum(is.na(passer)) # 73336 with no passes recorded

# Find proportion of made shots for each passer
made <- tapply(made, passer, FUN=sum)
made.total <- table(passer)
prop.made <- made/made.total
max.passer <- prop.made[order(prop.made)]
order.passer <- made.total[order(made.total)]

## How are these passers able to be stopped?
## Find whether or not the passes and shots taken were
## clustered spatially in any way.

plot(pass_x[which(passer==3321)], pass_y[which(passer==3321)])
points(poss_x[which(passer==3321)], poss_y[which(passer==3321)], col="blue")
points(shot_x[which(passer==3321)], shot_y[which(passer==3321)], col="red")
abline(v=94/2)

# Separate each passer's pass by left/right court
passers$court_half <- ifelse(pass_x <= (94/2), 1, 2)

left.pass <- passers[which(passers$court_half==1 & passers$offense_basket=="L"),]
right.pass <- passers[which(passers$court_half==2 & passers$offense_basket=="R"),]

#plot(left.pass$pass_x[which(left.pass$passer==3321)], left.pass$pass_y[which(left.pass$passer==3321)])
#plot(right.pass$pass_x[which(right.pass$passer==3321)], right.pass$pass_y[which(right.pass$passer==3321)])

# Start doing cluster tests
library(spatstat)

leftpass.locs <- left.pass[,10:12]
rightpass.locs <- right.pass[,10:12]
leftpass.locs[,1] <- as.factor(leftpass.locs[,1])
rightpass.locs[,1] <- as.factor(rightpass.locs[,1])

# Turn datasets into point pattern objects
left.ppp <- ppp(leftpass.locs[,2], leftpass.locs[,3], c(min(leftpass.locs[,2]),max(leftpass.locs[,2])), c(min(leftpass.locs[,3]),max(leftpass.locs[,3])))
marks(left.ppp) <- leftpass.locs[,1]
left.pp.split <- split(left.ppp, f=leftpass.locs[,1])

right.ppp <- ppp(rightpass.locs[,2], rightpass.locs[,3], c(min(rightpass.locs[,2]),max(rightpass.locs[,2])), c(min(rightpass.locs[,3]),max(rightpass.locs[,3])))
marks(right.ppp) <- rightpass.locs[,1]
right.pp.split <- split(right.ppp, f=rightpass.locs[,1])

# Quadrat tests
for (i in 1:610) {
  test <- quadrat.test(left.pp.split[i], alternative="clustered")
  if (test$p.value > .03) {
    print(i)
    print(table(left.pass$passer)[i])}
}

for (i in 1:615) {
  test2 <- quadrat.test(right.pp.split[i], alternative="clustered")
  if (test2$p.value > .03) {
    print(i)
    print(table(right.pass$passer)[i])}
}

# Density example
leftexample <- subset(left.ppp, left.ppp$marks==names(left.pp.split[297]))
plot(density(leftexample))
points(leftexample)
image(density(leftexample)$xcol, density(leftexample)$yrow, t(density(leftexample)$v))

leftexample <- subset(left.ppp, left.ppp$marks==names(left.pp.split[200]))
plot(density(leftexample))
points(leftexample)
image(density(leftexample)$xcol, density(leftexample)$yrow, t(density(leftexample)$v))

leftexample <- subset(left.ppp, left.ppp$marks==names(left.pp.split[444]))
rightexample <- subset(right.ppp, right.ppp$marks==names(right.pp.split[444]))
dev.off()
par(mfrow=c(1,2), mar=c(4,4,4,0))
image(density(leftexample)$xcol, density(leftexample)$yrow, t(density(leftexample)$v), col=rev(heat.colors(12)))
abline(v=max(density(leftexample)$xcol))
p <- abline(v=min(density(leftexample)$xcol))
p <- abline(h=max(density(leftexample)$yrow))
points(leftexample)
par(mar=c(4,0,4,4))
image(density(rightexample)$xcol, density(rightexample)$yrow, t(density(rightexample)$v), col=rev(heat.colors(12)), yaxt="n")
abline(h=max(density(rightexample)$yrow))
points(rightexample)
par(xpd=TRUE)
legend(x=100, y=37, legend=c("Dense","","","Sparse"),fill=heat.colors(4), cex=.6)

player <- "Reggie Evans"
id <- players[which(paste(players[,2], players[,3], sep=" ")==player),1]
id

leftexample <- subset(left.ppp, left.ppp$marks==id)
rightexample <- subset(right.ppp, right.ppp$marks==id)
leftexample <- subset(left.ppp, left.ppp$marks==names(left.pp.split[17]))


