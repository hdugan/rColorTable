#### Make 2-page color table of all colors available in R ####
# R color convention is color, color1, color2, color3, and color4
# with shades getting progressively darker
# For example skyblue, skyblue1, skyblue2, skyblue3, skyblue4
# This table prints only the full name, and fullname4 if available
# This table does not show greyscale colors grey0:grey100

# R colors minus 100 shades of grey 
cols = colors()[c(2:151,362:657)]

# Duplicate colors
dupCols = list()
for (i in 1:4){
  dupCols[[i]] <- cols[grep(pattern = i,x = cols)]
}
dupNames = gsub(pattern = '1',replacement = '',x = dupCols[[1]])

# All Color Names 
allNames = cols[!grepl('1',cols) & !grepl('2',cols) & !grepl('3',cols) & !grepl('4',cols)]
indx = match(dupNames,allNames)

# Output color table 
output = data.frame(col = allNames,c1=NA,c2=NA,c3=NA,c4=NA,stringsAsFactors = F)
for (i in 1:4){
  output[,i+1][indx] = dupCols[[i]]
}

# Set rectangles x-axis start and end points 
x1 = c(0,3,3.5,4,4.5)
x2 = c(3,3.5,4,4.5,5)

# 2 page color chart 
pdf('rColorTable.pdf',height = 10)
par(mar=c(0,6,0,6))
# First page
plot(0, type="n", ylab="", xlab="",axes=FALSE, ylim=c(69,0), xlim=c(1,5))
axis(2,at = 1:69,labels = allNames[1:69],las=1,cex.axis=0.6)
axis(4,at= indx[indx<70],labels = paste(allNames[indx[indx<70]],4,sep=''),cex.axis=0.6,las=1)
for (j in 1:69) {
  for (i in 1:5) {
    #k = j*5 + i
    if (!is.na(output[j,i])){
      rect(x1[i],j-0.5, x2[i],j+0.5, border="black", col=output[j,i])
    }
  }
}
# Second Page 
plot(0, type="n", ylab="", xlab="",axes=FALSE, ylim=c(138,70),xlim=c(1,5))
axis(2,at = 70:138,labels = allNames[70:138],las=1,cex.axis=0.6)
axis(4,at= indx[indx>=70],labels = paste(allNames[indx[indx>=70]],4,sep=''),cex.axis=0.6,las=1)
for (j in 70:138) {
  for (i in 1:5) {
    #k = j*5 + i
    if (!is.na(output[j,i])){
      rect(x1[i],j-0.5, x2[i],j+0.5, border="black", col=output[j,i])
    }
  }
}
dev.off()
