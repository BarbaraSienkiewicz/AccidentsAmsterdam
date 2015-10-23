
## Set colors for plot, select what should be drwan in different color
fill_colors <- c()
for (i in 1:`length(accidents$colname)) {
  if (accidents$colname[i]==1) {
    fill_colors <- c(fill_colors, '#821122')
  } else {
    fill_colors <- c(fill_colors, '#cccccc')
  }
}

barplot(accidents$col, names.arg=accidents$Jaar_vkl, col = fill_colors, border = NA,
        xlab="Year", ylab='...')