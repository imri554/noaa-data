# INSTALL AND LOAD PACKAGES ################################

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(ggplot2, pacman, rio, tidyverse, gridExtra)

library(ggplot2)

# LOAD AND PREPARE DATA ####################################
par("mar")
par(mar = c(1, 1, 1, 1))

df <- import("/Users/imrihaggin1/Desktop/NOAA data/Historic_distance_shore-1978-2016.xlsx") %>%
  as_tibble() %>%
  print()

mat <- import("/Users/imrihaggin1/Desktop/NOAA data/Historic_distance_shore-1978-2016.xlsx") %>%
  print()

# BASIC GRAPHS ##################################################


# REGIONAL VS FEDERAL COMPARISONS

x = "Alewive"

pdfPath = "~/Desktop/NOAA data/tonPlots.pdf"
pdf(file = pdfPath)
par(mfrow = c(1, 1))

#federal vs regional catch

#fed:reg line graph over years

#matplot for the weights and stuff

#tons

for (x in unique(df$FUS_NAME)) {
  
  filtered <- df %>%
    filter(FUS_NAME == x)

  filtered <- select(filtered, "YEAR", "3-200 TONS", "0-3 TONS")
  
  #filtered_removed_outliers <- filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons`[!filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons` %in% boxplot.stats(filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons`)$out]
  #print(boxplot.stats(filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons`)$out)
  
  #df_melt <- tidyr::gather(filtered, variable, value, "0-3 TONS", "3-200 TONS")
  
  #filter df for outliers
  #melt_values_removed_outliers <- df_melt[!df_melt %in% boxplot.stats(df_melt$value)$out]
  
  #plot(df_melt, type = "l", main = paste("Ratio (Federal : Regional) Tons of Catch for", x), xlab = "Year", ylab = "Federal : Regional Ratio (Tons)")
  #plot(filteredTS)
  
  
  #matplot is for wide format
  matplot(filtered$YEAR, select(filtered, "3-200 TONS", "0-3 TONS"), type = c("l"), lty = 1, lwd = 2, lend = par("lend"), pch=NULL ,col = 1:2, xlab = "Year", ylab = "Tons of Catch", main = paste("Tons of Catch for", x, "(Federal vs Regional)") )
  legend("topleft", c("3-200 TONS (Federal)", "0-3 TONS (Regional)"), col = 1:2, lty = 1, lwd = 2)
  
  
  #plot is for long format
  #plot(melt_values_removed_outliers$YEAR, melt_values_removed_outliers$value, type = 'l', main = paste("Tons of Catch for", x, "Federal vs Regional"), xlab = "Year", ylab = "tons", col = "#001AFD")
 
}

#Dollars
pdfPath = "~/Desktop/NOAA data/dollarPlots.pdf"
pdf(file = pdfPath)
par(mfrow = c(1, 1))
for (x in unique(df$FUS_NAME)) {
  
  filtered <- df %>%
    filter(FUS_NAME == x)
  
  filtered <- select(filtered, "YEAR", "3-200 DOLLARS", "0-3 DOLLARS")
  
  
  matplot(filtered$YEAR, select(filtered, "3-200 DOLLARS", "0-3 DOLLARS"), type = c("l"), lty = 1, lwd = 2, lend = par("lend"), pch=NULL ,col = 1:2, xlab = "Year", ylab = "Dollars of Catch (Thousands)", main = paste("Dollars of Catch for", x, "(Federal vs Regional)") )
  legend("topleft", c("3-200 DOLLARS (Federal)", "0-3 DOLLARS (Regional)"), col = 1:2, lty = 1, lwd = 2)
  
  
  
}
pdfPath = "~/Desktop/NOAA data/poundsPlots.pdf"
pdf(file = pdfPath)
par(mfrow = c(1, 1))
for (x in unique(df$FUS_NAME)) {
  
  filtered <- df %>%
    filter(FUS_NAME == x)
  
  filtered <- select(filtered, "YEAR", "3-200 POUNDS", "0-3 POUNDS")
  
  
  matplot(filtered$YEAR, select(filtered, "3-200 POUNDS", "0-3 POUNDS"), type = c("l"), lty = 1, lwd = 2, lend = par("lend"), pch=NULL ,col = 1:2, xlab = "Year", ylab = "Pounds of Catch (Thousands)", main = paste("Pounds of Catch for", x, "(Federal vs Regional)") )
  legend("topleft", c("3-200 Pounds (Federal)", "0-3 Pounds (Regional)"), col = 1:2, lty = 1, lwd = 2)
  
  
  
}
















 
for (x in unique(df$FUS_NAME)) {
 
    filtered <- df %>%
      filter(FUS_NAME == x)
    
     #filtered_removed_outliers <- filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons`[!filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons` %in% boxplot.stats(filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons`)$out]
     #print(boxplot.stats(filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons`)$out)
     
    plot(filtered$YEAR, filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons`, type = "l", main = paste("Ratio (Federal : Regional) Tons of Catch for", x), xlab = "Year", ylab = "Federal : Regional Ratio (Tons)")
     
     #also have the box plot next to it!!!
     
     summary(filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons`)
     filtered_removed_outliers <- filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons`[!filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons` %in% boxplot.stats(filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons`)$out]
     filtered$`Ratio 3 - 200 Tons : 0 - 3 Tons` %>%
       boxplot(
         horizontal = T,  # Horizontal
         axes = TRUE,
         notch  = T,      # Confidence interval for median
         main   = paste("Boxplot of ", x, " x"," Total Pounds"),
         xlab   = paste("Total Pounds of ", x)
       )
  
}


df %>% summary()  # Summary for entire table

unique(df$FUS_NAME)

#pdfPath = "~/Desktop/NOAA data/preliminaryPlots.pdf"
#pdf(file = pdfPath)
par(mfrow = c(1, 1))

for (x in unique(df$FUS_NAME)) {
  filtered <- df %>%
    filter(FUS_NAME == x) 
  
  filtered
  
  plot(filtered$YEAR, filtered$`TOTAL DOLLARS`, type = "l", main = paste(x, " x Total Dollars "), xlab = "Year", ylab = "Total Dollars (Thousands)")
  plot(filtered$YEAR, filtered$`TOTAL TONS`, type = "l", main = paste(x, " x Total Tons"), xlab = "Year", ylab = "Total Tons")
  plot(filtered$YEAR, filtered$`TOTAL POUNDS`, type = "l", main = paste(x, " x Total Pounds"), xlab = "Year", ylab = "Total Pounds (Thousands)")
  
}

filtered <- df %>%
  filter(FUS_NAME == "Crab, other") 
summary(filtered$`TOTAL POUNDS`)

plot(filtered)

df %>%    # Summary for one variable
  select("3-200 DOLLARS") %>%
  summary()

# QUARTILES ################################################

#boxplots

for (x in unique(df$FUS_NAME)) {
  filtered <- df %>%
    filter(FUS_NAME == x) 
  
  filtered_removed_outliers <- filtered$`TOTAL POUNDS`[!filtered$`TOTAL POUNDS` %in% boxplot.stats(filtered$`TOTAL POUNDS`)$out]
  
  
  filtered_removed_outliers %>%
    boxplot(
      horizontal = T,  # Horizontal
      axes = TRUE,
      notch  = T,      # Confidence interval for median
      main   = paste("Boxplot of ", x, " x"," Total Pounds"),
      xlab   = paste("Total Pounds of ", x)
    )
  
  }

summary(filtered$`TOTAL POUNDS`)

boxplot.stats(filtered$`TOTAL POUNDS`)



# DENDDROGRAM ###########################
filtered <- df %>%
  filter(YEAR == 1978) 
summary(filtered$`TOTAL POUNDS`)

distance<-dist(filtered)

# Calculate clusters
hc <- filtered %>%  # Get data
  dist %>%    # Compute distance/dissimilarity matrix
  hclust      # Compute hierarchical clusters

# Plot dendrogram
hc %>% plot(labels = filtered$FUS_NAME)

# Draw boxes around clusters
hc %>% rect.hclust(k = 2, border = "gray80")  # 2 boxes
hc %>% rect.hclust(k = 3, border = "gray60")  # 3 boxes
hc %>% rect.hclust(k = 4, border = "gray40")  # 4 boxes

# QUARTILES ################################################

# Tukey's five-number summary: minimum, lower-hinge,
# median, upper-hinge, maximum. No labels.
fivenum(filtered)

# Boxplot stats: hinges, n, CI for median, outliers
boxplot(df, notch = T, horizontal = T)
boxplot.stats(df)
  
# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)

