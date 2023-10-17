data("BOD")
BOD
df<-read.csv('https://raw.githubusercontent.com/saturn70/R_Projects/main/BOD/BOD%20.csv')
df


head(df,3)
tail(df,3)

#Rows
nrow(df) # 31

#Columns
ncol(df) # 3

#Rows,Columns
dim(df) # 31 3

summary(df)

Time<-df$Time
Time

Demand<-df$Demand
Demand

#Time
Q1<-quantile(Time, probs = 0.25)
Q1

Q2<-quantile(Time, probs = 0.50)
Q2

mean(Time)

Q3<-quantile(Time, probs = 0.75)
Q3

max(Time)

# Demand
min(Demand)

Q1<-quantile(Demand, probs = 0.25)
Q1

Q2<-quantile(Demand, probs = 0.50)
Q2

mean(Demand)

Q3<-quantile(Demand, probs = 0.75)
Q3

max(Demand)


str(df)

boxplot(df)

plot(Time)
plot(Demand)


hist(Time)
hist(Demand)

barplot(Time)
barplot(Demand)

#barplot
barplot(Demand,xlab = "X",ylab = "Y",las = 2,col = "orange")

#histogram
hist(Time,xlab="X",ylab="Y",col="orange",las=1,border="white",breaks=10)
hist(Demand,xlab="X",ylab="Y",col="orange",las=1,border="white",breaks=10)

#scatter Plot
plot(df$Time~ df$Demand, # The plot in formula syntax; y ~ x
     xlim = c(5,20), # Range of x-axis
     ylim = c(0,20), # Range of y-axis
     pch = 19, # Plotting symbol change to filled-in circle
     xlab = "Demand", # x-axis label
     ylab = "Time", # y-axis label
     las = 1,col="blue") # Rotate numbers on y-axis


# Boxplot 
boxplot(df$Time~ df$Demand,
        ylab = "Time", # x-axis label
        xlab = "Demand", # y-axis label
        las = 1, # Rotate numbers on y-axis
        xlim=c(0,10),
        ylim = c(0,20), # Range of y-axis
        col = "lightblue", # Boxplot fill color
        border = "darkred", # Boxplot border color
        pch = 19) # Symbol for outliers

# Create a line plot
plot(df$Time~ df$Demand,type = "l", col = "blue",  main = "Time vs Demand")

library(ggplot2)
ggplot(data = df, aes(x = Time, y = Demand)) +geom_point()
ggplot(data = df, aes(x = Demand, y = Time)) +geom_point()


#Line Plot
ggplot(data = df, aes(x = Time, y = Demand)) +geom_line()
ggplot(data = df, aes(x = Demand, y = Time)) +geom_line()


##Bar Plot
ggplot(data = df, aes(x = Demand)) +geom_bar()
ggplot(data = df, aes(x = Time)) +geom_bar()

#histogram
ggplot(data = df, aes(x = Time))  +geom_histogram()
ggplot(data = df, aes(x = Demand)) +geom_histogram()
ggplot(data = df, aes(y = Time)) +geom_histogram()
ggplot(data = df, aes(y = Demand))  +geom_histogram()

#BoxPlot
ggplot(data = df, aes(x = Time, y =Demand)) +geom_boxplot()
ggplot(data = df, aes(x = Demand, y =Time)) +geom_boxplot()


#Violin Plot
ggplot(data = df, aes(x = Time, y =Demand)) +geom_violin()
ggplot(data = df, aes(x = Demand, y =Time)) +geom_violin()


m<-mean(Time)
m

T<-t.test(x=Time, mu = m)
T

TT<-t.test(x=Time, mu = 25)
TT

if (TT$p.value < 0.05) {
  cat("The sample mean is significantly different from the hypothesized mean.\n")
} else {
  cat("There is no significant difference between the sample mean and the hypothesized mean.\n")
}

#anova one way
an1<-lm(Time~Demand)
anova(an1)

#F test
result <- var.test(Time,Demand, ratio = 1, alternative = "two.sided")
result

result <- var.test(Demand, Time, ratio = 1, alternative = "two.sided")
result


#correaltion Test
cor.test(Demand, Time, method = "pearson")
cor.test(Time,Demand, method = "spearman")
cor.test(Time,Demand, method = "kendall")

#pricipal component analysis
prcomp(df)

#Factor Analysis
factanal(df, factors = 1)

wilcox.test(Time,Demand,paired = TRUE)


# Perform a Friedman test

DF <- matrix(df$Time,df$Demand)
DF

friedman_result <- friedman.test(DF)
friedman_result

# Check if there are statistically significant differences
if (friedman_result$p.value < 0.05) {
  cat("There are statistically significant differences between groups.\n")
} else {
  cat("There are no statistically significant differences between groups.\n")
}

library(BayesFactor)
ttestBF(df$Time,df$Demand)

wilcox.test(df$Time,df$Demand)

#runs test
x <- factor(sign(rnorm(df$Demand)))  # randomness
runs.test(x)

