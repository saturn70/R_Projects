rm(list=ls())

df<-read.csv('https://raw.githubusercontent.com/saturn70/R_Projects/main/Trees/trees.csv')
df
as.data.frame(df)

head(df,3)
tail(df,3)

#Rows
nrow(df) # 31

#Columns
ncol(df) # 3

#Rows,Columns
dim(df) # 31 3

summary(df)

Girth<-df$Girth
Girth

Height<-df$Height
Height

Volume<-df$Volume
Volume



min(Height)
max(Height)

min(Volume)
max(Volume)

#Girth
min(Girth)

Q1<-quantile(Girth, probs = 0.25)
Q1

Q2<-quantile(Girth, probs = 0.50)
Q2

mean(Girth)

Q3<-quantile(Girth, probs = 0.75)
Q3

max(Girth)

#Height
min(Height)

Q1<-quantile(Height, probs = 0.25)
Q1

Q2<-quantile(Height, probs = 0.50)
Q2

mean(Height)

Q3<-quantile(Height, probs = 0.75)
Q3

max(Height)

#Volume
min(Volume)

Q1<-quantile(Volume, probs = 0.25)
Q1

Q2<-quantile(Volume, probs = 0.50)
Q2

mean(Volume)

Q3<-quantile(Volume, probs = 0.75)
Q3

max(Volume)

str(df)

boxplot(df)
boxplot(Volume)

# Calculate the IQR for a specific column (e.g., "conc1")
q1 <- quantile(Volume, 0.25)
q3 <- quantile(Volume, 0.75)
iqr <- q3 - q1
iqr

# Define the lower and upper bounds for outliers
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr
lower_bound
upper_bound

# Identify outliers
outliers <- Volume[Volume< lower_bound | Volume > upper_bound]
outliers


plot(Girth)
plot(Height)
plot(Volume)

hist(Girth)
hist(Height)
hist(Volume)

barplot(Girth)
barplot(Volume)
barplot(Height)

#barplot
barplot(Volume,xlab = "X",ylab = "Y",las = 2,col = "orange")

#histogram
hist(Girth,xlab="X",ylab="Y",col="orange",las=1,border="white",breaks=10)
hist(Height,xlab="X",ylab="Y",col="orange",las=1,border="white",breaks=10)
hist(Volume,xlab="X",ylab="Y",col="orange",las=1,border="white",breaks=10)

#scatter Plot
plot(trees$Volume ~ trees$Height, # The plot in formula syntax; y ~ x
     xlim = c(60,90), # Range of x-axis
     ylim = c(0,100), # Range of y-axis
     pch = 19, # Plotting symbol change to filled-in circle
     xlab = "Height", # x-axis label
     ylab = "Volume", # y-axis label
     las = 1,col="blue") # Rotate numbers on y-axis

plot(trees$Volume ~ trees$Girth, # The plot in formula syntax; y ~ x
     xlim = c(0,30),# Range of x-axis
     ylim = c(0,100), # Range of y-axis
     pch = 19, # Plotting symbol change to filled-in circle
     xlab = "Girth", # x-axis label
     ylab = "Volume", # y-axis label
     las = 1,col="blue") # Rotate numbers on y-axis


plot(trees$Height ~ trees$Girth, # The plot in formula syntax; y ~ x
     xlim = c(0,90), # Range of x-axis
     ylim = c(0,100), # Range of y-axis
     pch = 19, # Plotting symbol change to filled-in circle
     xlab = "Girth", # x-axis label
     ylab = "Volume", # y-axis label
     las = 1,col="blue") # Rotate numbers on y-axis


# Boxplot 
boxplot(trees$Volume~trees$Girth,
        ylab = "Volume", # x-axis label
        xlab = "Girth", # y-axis label
        las = 1, # Rotate numbers on y-axis
        ylim = c(0,200), # Range of y-axis
        col = "lightblue", # Boxplot fill color
        border = "darkred", # Boxplot border color
        pch = 19) # Symbol for outliers

# Create a line plot
plot(trees$Volume~trees$Girth,type = "l", col = "blue", xlab = "Girth", ylab = "Volume", main = "Girth vs Volume")

library(ggplot2)
ggplot(data = df, aes(x = Girth, y = Height)) +geom_point()
ggplot(data = df, aes(x = Girth, y = Volume)) +geom_point()

ggplot(data = df, aes(x = Volume, y = Girth)) +geom_point()
ggplot(data = df, aes(x = Volume, y = Height)) +geom_point()

ggplot(data = df, aes(x = Height, y = Volume)) +geom_point()
ggplot(data = df, aes(x = Height, y = Girth)) +geom_point()

#Line Plot
ggplot(data = df, aes(x = Girth, y = Height)) +geom_line()
ggplot(data = df, aes(x = Girth, y = Volume)) +geom_line()

ggplot(data = df, aes(x = Volume, y = Girth)) +geom_line()
ggplot(data = df, aes(x = Volume, y = Height)) +geom_line()

ggplot(data = df, aes(x = Height, y = Volume)) +geom_line()
ggplot(data = df, aes(x = Height, y = Girth)) +geom_line()

##Bar Plot
ggplot(data = df, aes(x = Girth))  +geom_bar()
ggplot(data = df, aes(x = Height)) +geom_bar()
ggplot(data = df, aes(x = Volume)) +geom_bar()

ggplot(data = df, aes(y = Girth))  +geom_bar()
ggplot(data = df, aes(y = Height)) +geom_bar()
ggplot(data = df, aes(y = Volume)) +geom_bar()

#histogram
ggplot(data = df, aes(x = Girth))  +geom_histogram()
ggplot(data = df, aes(x = Height)) +geom_histogram()
ggplot(data = df, aes(x = Volume)) +geom_histogram()

ggplot(data = df, aes(y = Girth))  +geom_histogram()
ggplot(data = df, aes(y = Height)) +geom_histogram()
ggplot(data = df, aes(y = Volume)) +geom_histogram()

#BoxPlot
ggplot(data = df, aes(x = Girth, y = Height)) +geom_boxplot()
ggplot(data = df, aes(x = Girth, y = Volume)) +geom_boxplot()

ggplot(data = df, aes(x = Volume, y = Girth)) +geom_boxplot()
ggplot(data = df, aes(x = Volume, y = Height)) +geom_boxplot()

ggplot(data = df, aes(x = Height, y = Volume)) +geom_boxplot()
ggplot(data = df, aes(x = Height, y = Girth)) +geom_boxplot()

#Violin Plot
ggplot(data = df, aes(x = Girth, y = Height)) +geom_violin()
ggplot(data = df, aes(x = Girth, y = Volume)) +geom_violin()

ggplot(data = df, aes(x = Volume, y = Girth)) +geom_violin()
ggplot(data = df, aes(x = Volume, y = Height)) +geom_violin()

ggplot(data = df, aes(x = Height, y = Volume)) +geom_violin()
ggplot(data = df, aes(x = Height, y = Girth)) +geom_violin()


library(plot3D)
scatter3D(x = Girth,  y = Volume, z = Height, pch = 19)
scatter3D(x = Height, y = Girth,  z = Volume, pch = 19)
scatter3D(x = Volume, y = Height, z = Girth, pch = 19)

library(gplots)
heatmap.2(as.matrix(df), dendrogram = TRUE, trace = 'none', key = TRUE)

m<-mean(Girth)
m

T<-t.test(x=Girth, mu = m)
T

TT<-t.test(x=Girth, mu = 25)
TT

if (TT$p.value < 0.05) {
  cat("The sample mean is significantly different from the hypothesized mean.\n")
} else {
  cat("There is no significant difference between the sample mean and the hypothesized mean.\n")
}

#anova one way
an1<-lm(Volume~Height)
anova(an1)

#anova two way      
an2<-lm(Volume~ Height*Girth)
anova(an2)


#F test
result <- var.test(Volume, Height, ratio = 2, alternative = "two.sided")
result

result <- var.test(Volume, Height, ratio = 1, alternative = "two.sided")
result


#chisq.test(table(Volume,Height))

#correaltion Test
cor.test(Volume, Girth, method = "pearson")
cor.test(Height, Volume, method = "spearman")

#pricipal component analysis
prcomp(df)

#Factor Analysis
factanal(df, factors = 1)


