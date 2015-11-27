library(ggplot2)
str(diamonds)

head(diamonds$cut)
## factors - special type of variables, categorizing data
class(diamonds)

??factor

head(diamonds)

p <- ggplot(diamonds, aes(cut))
p + geom_histogram()

p <- ggplot(diamonds, aes(clarity))
p+geom_histogram()

p <- ggplot(diamonds, aes(clarity, fill = cut))
p+ geom_histogram()

my.data <- read.csv(...)
names(my.data)