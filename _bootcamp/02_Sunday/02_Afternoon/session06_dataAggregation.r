gDat <- read.delim("data/gapminderDataFiveYear.txt")
str(gDat) # 'data.frame':  1704 obs. of  6 variables:

## built-in apply() function operates on a MATRIX, not a data.frame
## so we must make one!

## challenge: get the life expectancy data for three countries (eg cambodia,
## canada, rwanda) in a MATRIX, one column per country; make sure column names
## give country and rownames give year

## "brute force"
tDat <- with(gDat,
             cbind(Cambodia = lifeExp[country == "Cambodia"],
                   Canada = lifeExp[country == "Canada"],
                   Rwanda = lifeExp[country == "Rwanda"]))
rownames(tDat) <- with(gDat, year[country == "Canada"])
str(tDat)
tDat

## optional:
## reshape2 -- worth learning for future
library(reshape2)
library(plyr) # to use '.' to quote an expression

tmp <- acast(subset(gDat,
                    subset = country %in% c("Cambodia", "Canada", "Rwanda")),
             year ~ country, value.var = "lifeExp")
## or using acast()-native subsetting:
acast(gDat, subset = . (country %in% c("Cambodia", "Canada", "Rwanda")),
      year ~ country, value.var = "lifeExp")

## see, we get the same thing!
identical(tDat, tmp)

## now we have the matrix and can demo apply()
apply(tDat, 1, mean)
apply(tDat, 2, median) # note payoff from good dimnames
rowMeans(tDat) # FAST use for big datasets
which.min(tDat[1, ])
apply(tDat, 1, which.min)
colnames(tDat)[apply(tDat, 1, which.min)]
data.frame(tDat, min_country = colnames(tDat)[apply(tDat, 1, which.min)])
apply(tDat, 2, summary)
apply(tDat, 2, quantile, probs = c(0.25, 0.75))

## exercise
## compute the interquartile range of lifeExp by country
## IQR()

## built-in aggregate() function computes on a vector, split out by an
## associated factor
aggregate(lifeExp ~ continent, gDat, FUN = mean)
aggregate(lifeExp ~ year * continent, gDat, FUN = mean)
aggregate(lifeExp ~ continent, gDat, quantile, probs = c(0.25, 0.75))
aggregate(country ~ continent, gDat, function(x) length(unique(x)))

## exercise
## compute the min and max of gdpPercap by country for 4 countries of your choice

# reshape2: melt and cast

# won't work if we load with row.names in first column, and we want these to be their own column for melting anyway
inflam <- read.csv("data/inflammation-01-metadata-rich.csv", header=TRUE, row.names=1)

# so we load it without row names and it works fine
inflam <- read.csv("data/inflammation-01-metadata-rich.csv", header=TRUE)
inflam.melted <- melt(inflam) #this works

# but this is more specific
inflam.melted <- melt(inflam, variable.name="Day", value.name="inflammation_level")

dcast(inflam.melted, PatientID ~ Day) # getting back our starting data set

dcast(inflam.melted, FavColor ~ Treatment, sum) # but we can really do all kinds of stuff here... including arbitrary functions...
dcast(inflam.melted, FavColor ~ Treatment + Coast, sum) 
dcast(inflam.melted, FavColor ~ Treatment + Coast, mean) #can't divide by zero, so get NaNs 


# but we can really do all kinds of stuff here... including arbitrary functions...
return_YES <- function(x){"YES"}

dcast(inflam.melted, FavColor ~ Treatment + Coast, return_YES) 
