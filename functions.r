# Functions

# Convert pixels to cm2, based on conversionRates
convertPxlToCm2 <- function(lemnaRow, conversionRates = conversionRates)
{
  tray <- lemnaRow[8]
  date <- lemnaRow[12]
  ROI <- lemnaRow[1]
  Area <- as.numeric(lemnaRow[3])
  column <- as.numeric(substr(ROI, 2, nchar(ROI)))
  conversion <- conversionRates[conversionRates$Tray == tray & conversionRates$Date == date, ]
  # define which conversion needs to be taken (august pictures are composed out of two pictures and thus have two conversion rates, depending on the column)
  conversionNr <- NA
  if (dim(conversion)[1] > 1)
  {
    # get sequence of columns per conversion rate
    columns <- lapply(conversion$Columns, function(col){
      startCol <- as.numeric(strsplit(col, ';')[[1]][1])
      endCol <- as.numeric(strsplit(col, ';')[[1]][2])
      return(seq(startCol, endCol))})
    # define which conversionNr the column is matching to
    for (i in 1:length(ranges))
    {
      if (column %in% ranges[[i]]){conversionNr <- i}
    }
  }
  #else if (dim(conversion)[1] == 1){conversionNr <- 1}
  else if (conversion$Columns == 'all'){conversionNr <- 1}
  else {print(paste('ERROR: no matching conversion rate found for :', tray, ROI, date, sep = '_')); next()}
  
  pxlToCm2 <- conversion$Conversion[conversionNr]
  cm2 <- Area * pxlToCm2
  return(cm2)
}
