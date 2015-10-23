## loda data
tvs <- read.table('.txt')

## Filter outliers
tvs <- tvs[tvs$size > 80,]
tvs <- tvs[tvs$size < 10,]

## Set breaks for histogram
braks <- seq(10, 80, by=5)

## Set the layout: 2 col, 4 rowsþ
par(mfrow=c(4,2))

## Draw histograms
hist(tvs[tvs$year==2004]$size, breaks = breaks)