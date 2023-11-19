library(graphics)
mydata <- read.table(file.choose(), header=TRUE)
attach(mydata)

pairs(mydata)
# See variable names
names(mydata)
# Focus on continuous variables
pairs(mydata[, 3:9])


panel.d<-function(x, ...){
usr <- par("usr")
on.exit(par(usr))
par(usr=c(usr[1:2], 0, 0.7))
lines(density(x))	
}

scaled.data<-scale(mydata[3:9])
r <- range(scaled.data)
pairs(scaled.data, diag.panel=panel.d, xlim=r, ylim=r)


library(lattice)
splom(mydata[3:9])
splom(mydata[3:9], groups=mydata$GENDER)
splom(mydata[3:9], groups=mydata$GENDER, col=c("red","blue"))

splom(~mydata[3:9], groups=GENDER, data=mydata)
splom(~mydata[5:9], groups=GENDER, data=mydata, col=1, pch=c(1, 2))
splom(~mydata[5:9], groups=GENDER, data=mydata, col=1, pch=c(1, 2), cex=c(0.5, 0.5))

# See also the function coplot that handles conditional plots

cloud(BMI ~ SBP * DBP, data=mydata, groups=GENDER)

print(cloud(BMI ~ SBP * DBP, data=mydata, groups=GENDER))

print(cloud(BMI ~ SBP * DBP, data=mydata, groups=GENDER, scales = list(draw=FALSE)))

print(cloud(BMI ~ SBP * DBP, data=mydata, groups=GENDER, scales = list(draw=FALSE), screen=list(z=30, x=-30, y=0)))

print(cloud(BMI ~ SBP * DBP, data=mydata, groups=GENDER, main="1", scales = list(draw=FALSE)), split=c(1,1, 2,1), more=TRUE)
print(cloud(CHOL ~ SBP * DBP, data=mydata, groups=GENDER, main="2", scales = list(draw=FALSE)), split=c(2,1, 2,1))


# Plots by groups 

# Calculate regression lines by gender
fit.F<-lm(DBP~SBP, subset=GENDER=="Female")
fit.M<-lm(DBP~SBP, subset=GENDER=="Male")

# Add points to empty plot. Females points in red and males points in blue. 
plot(SBP, DBP, type="n", main="Linear relationship by gender")
points(SBP[GENDER=="Female"], DBP[GENDER=="Female"] , col="red")
points(SBP[GENDER=="Male"], DBP[GENDER=="Male"] , col="blue")

# Add regression lines with color
abline(fit.F, col="red")
abline(fit.M, col="blue")
# Add legend
legend("topleft", c("Female", "Male"), col=c("red", "blue"), pch = 1, title = "Gender")



# xyplots 


xyplot(SBP ~ DBP, data=mydata)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              

xyplot(SBP ~ DBP | GENDER, data=mydata)



##### NBA EXAMPLE 

getwd()  # Ask for current working directory
setwd("/Users/jcroman/Dropbox/SDSU_courses/STAT510") # Set working directory ; Use your own path
getwd()  # Verify working directory

nba <- read.csv(file="NBA2015Data.csv", header=TRUE)

names(nba) # column names

# We won't use attach(nba) in this example


plot(nba$MIN, nba$PTS)

hist(nba$MIN)
hist(nba$PTS)  

# Let's model log(PTS) vs MIN; log = natural log 

# Be careful a few players have PTS=0
which(nba$PTS==0)

# Let's look at their data

nba[which(nba$PTS==0), ]

# These players played 4 games of less; let's remove them from the data set

nba.r <- nba[-which(nba$PTS==0), ]

dim(nba)
dim(nba.r)  # 6 players have been removed

plot(nba.r$MIN, log(nba.r$PTS))

log.fit <- lm(log(PTS) ~ MIN, data=nba.r)
summary(log.fit)
abline(log.fit, col="red")

xyplot( log(PTS) ~ MIN, data=nba.r)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              

xyplot( log(PTS) ~ MIN | TEAM, data=nba.r)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              

xyplot(log(PTS) ~ MIN | TEAM, groups=TEAM, type=c("p"), data=nba.r)

xyplot(log(PTS) ~ MIN | TEAM, groups=TEAM, type=c("p","r"), data=nba.r)
        










  


