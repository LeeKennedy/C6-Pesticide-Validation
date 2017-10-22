library("ProjectTemplate")
load.project()

#Import data
# Data should be in labelled columns with no row labels.
#Input <- read.csv("data.csv", as.is=TRUE, header = TRUE)
Input <- read_excel("~/Desktop/Trial Pesticide Data.xlsx", 
                    sheet = "Sheet4")

#Always look at the data
boxplot(Input, frame = FALSE)

# Review data frame
summary(Input)

#convert to a two element stack
xs <- stack(Input)

# Run ANOVA
anova1 <- aov(values ~ ind, data = xs)

#Review results
summary(anova1)

#Check for significant differences
TukeyHSD(anova1)

#Repeatability & Interim Precision
mean.sqr <- summary(anova1)[1][[1]][[3]]
ncount <- as.numeric(length(anova1$effects))/as.numeric(length(anova1$coefficients))
sdr <- sqrt(mean.sqr[2])
interim <- sqrt((mean.sqr[1]-mean.sqr[2])/ncount)
sdR <- sqrt(sdr^2 + interim^2)
sdr
sdR

#Export
#capture.output(summary(anova1), file = "test.doc" )

