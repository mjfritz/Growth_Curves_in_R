#This was formerlly titled parse_SparkControl.R . That file still exists, but is now not updated.
# Depends on openxlsx, dplyr, scales for log transformations and the here package for file pathing.
library(openxlsx)
library(tidyverse)
library(dplyr)
library(scales)
library(here)

# To setup for this, fill in the plate_template.xlsx table and save it to correlate to the plate.
# It has columns for well ID (first) in letterNumber format (ex, B11 for Row 2/B, column 11 on the plate),
# strain, condition, plate series name and number combo, and sample replicate number within the plate.
# Plate number should be the same down the whole table.
# Blanks should be listed as "blank" in the strain column.(Label empty/unwanted wells as "blank" too. Relying on the first 3 reads
# from each well as the blank for that well.)
# Can take advantage of "sort" function to make filling in replicate number easier after all other
# columns have been filled in.


#Smaller helper function to conver the data into "long" format (where it starts with a column for each sample.)
melt_plate <- function(.data){
  plate <- data.frame(t(.data), stringsAsFactors = FALSE)
  colnames(plate) <- plate[1, ]
  plate <- plate[-1, ]
  plate$sample_name <- rownames(plate)
  plate <- plate %>% separate(sample_name, sep = "\\.",
                              c("strain", "condition", "plate", "rep"), remove = FALSE)
  sample_data <- plate %>% dplyr::select(tail(names(.), 5))
  plate_m <- pivot_longer(plate, 1:(ncol(plate)-5),
                          names_to = "time", values_to = "absorbance")
  plate_m$time <- as.numeric(gsub("X", "", plate_m$time)) ## Because time was the column names, it is a character
  return( setNames(list(plate_m, sample_data), c("melted_plate", "sample_data")))
}

#Helper funciton to correlate the plate_template with the absorbance data that has well names, as well as
#clean up the blanks, and format names and times for different pipelines.
assign_wells <- function(templatewithpath, datasheet, output = c("python", "prism", "R"), remove_emptys = c(TRUE, FALSE)){
  df <- read.xlsx(templatewithpath, rowNames = FALSE, colNames = TRUE) #Read in template
  if (remove_emptys == TRUE){
    blanks <- df[df$Strain == "blank", "Well_ID"] # Find blank wells
    df <- df[!(df$Strain == "blank"), ] # Remove blank wells from template
  }
  if (output %in% c("python", "R")){
    df$ID <- paste(df$Strain, df$Condition, df$Plate, df$Replicate, sep = ".") # Create column of new names
  } else if (output == "prism"){
    df$ID <- paste(df$Strain, df$Condition, df$Replicate, sep = "-") # Simply so that ordering is possible
  }
  if (remove_emptys == TRUE) {
    datasheet <- datasheet[ , -which(names(datasheet) %in% blanks)] # Remove blanks from data sheet
  }
  for (ref in df$Well_ID){
    names(datasheet)[names(datasheet) == ref] <- df[ df$Well_ID == ref, "ID" ]
  }
  return(datasheet)
}


"
Problem here is that the metadata section will expand with
the number of cycles and the number of methods used.
"

# get_metadata <- function(filewithpath, start_row){
#   df <- read.xlsx(filewithpath, sheet = 1,
#                   rows = start_row:(start_row+ 5), cols = 1:5,
#                   skipEmptyRows = TRUE, skipEmptyCols = TRUE,
#                   rowNames = TRUE, colNames = FALSE)
#   return(df)
# }

"
This may not be expandable to all cases.
"

# Main function for reading 48 hour growth curves from the Tecan Spark reader output.
# Takes the xlsx file from Tecan SparkControl output
# and a 2-column data frame with well name and unique sample name. For each of those,
# it expects the full path and file name. (I use the here package to make that easier for me.)
# Also writes a cleaned copy of the data to a new spreadsheet.


# After working with it, I realize that this is a little bloated, and the meta data and multiple things in the list aren't really needed.
# The read.xlsx calls to specific rows is dependent on the method used to get absorbance readings. To modify for a 24 hour method,
# you will need to adjust the speceific rows  in the metadata's and sheets[[]]'s read.xlsx calls. 
# Ex. I think 24 hours will not have two separate chunks of data, so there will only be one range for meta data, and one range for the first sheet.
# This would also remove the need to bind_rows, but would need to be assigned to gd, then.
parse_growth_Spark <- function(datawithpath, templatewithpath, output = c("python", "prism", "R"),
                              remove_emptys = c(TRUE, FALSE), outputfoldername){
  sheets <- vector("list", 2)
  
  
  #These rows and columns are dependent on the number of cycles and timepoints
  sheets[[1]] <- read.xlsx(datawithpath,
                         rows = 54:198, cols = 2:99,
                         skipEmptyCols = TRUE,
                         rowNames = FALSE, colNames = TRUE, check.names = TRUE)
  sheets[[2]] <- read.xlsx(datawithpath,
                         rows = 221:365, cols = 2:99,
                         skipEmptyCols = TRUE,
                         rowNames = FALSE, colNames = TRUE, check.names = TRUE)
  
  #For simplicities sake, going to add 24 hours in seconds to all the times in the second data set to combine.
  sheets[[2]]$`Time..s.` <- sheets[[2]]$`Time..s.` + 86400
  gd <- bind_rows(sheets[[1]], sheets[[2]])
  
  if (output == "python"){
    gd$time <- round(gd$`Time..s.`/60) # Converting to minutes
  } else if (output == "prism"){
    t <- round(gd$`Time..s.`)
    hours <- t%/%3600
    t <- t%%3600
    minutes <- t%/%60
    seconds <- t%%60
    gd$time <- paste(hours, minutes, seconds, sep = ":")
  } else if (output == "R"){
    gd$time <- gd$`Time..s.`/60/60 # Converting to hours
  }

  gd <- subset(gd, select=-c(`Time..s.`, `Temp....C.`)) # Remove unwanted columns
  gd <- assign_wells(templatewithpath, gd, output, remove_emptys) # Rename columns based on samples
  gd <- gd[ , order(names(gd))]
  gd <- gd %>% relocate(time)
  

  # Creating a melted data frame for graphing
  mp <- melt_plate(gd)

  dir.create(here(outputfoldername), showWarnings = FALSE)
  if (output == "prism"){
    fn <- paste0(gsub(".xlsx", "", basename(datawithpath), fixed = TRUE), "_parsedforPrism.csv")
    fullfn <-here::here(outputfoldername, fn)
    cat("Saving: ", fullfn, "\n")
    write.csv(gd, file = fullfn, row.names = FALSE)
  } else if (output == "python") {
    fn <- paste0(gsub(".xlsx", "", basename(datawithpath), fixed = TRUE), "_parsedforPython.tsv")
    fullfn <-here::here(outputfoldername, fn)
    cat("Saving: ", fullfn, "\n")
    write.table(gd, file = fullfn, sep = "\t", row.names = FALSE)
  } else if (output == "R") {
    fn <- paste0(gsub(".xlsx", "", basename(datawithpath), fixed = TRUE), "_parsedforR.csv")
    fullfn <-here::here(outputfoldername, fn)
    cat("Saving: ", fullfn, "\n")
    write.csv(gd, file = fullfn, row.names = FALSE)
  }
  else {
    print("Not saving parsed file.")
  }

  return(setNames(list(gd, mp[[1]], mp[[2]]), c("data", "melted", "sample_data")))
}


# For 48 hour growth curves on the older VersaMax machines. Still unsure about input difference between machines.
# REQUIRES that the ouput have been saved as an xlsx file. (Probably able to be modified for saving as a csv, BUT
# the two options for output are .txt and .xls, both of which require resaving/checking.)
# For other length data, make sure that the # of BLOCKS is 1, and modify the read.xlsx call to either replace the startRow parameter with
# a rows parameter or just modify it. For Less than 48 hours, you can remove the secondhalf bit, as well as the bind_rows call that combines the data frames.
parse_growth_VersaMax_xlsx <- function(datawithpath, templatewithpath, output = c("python", "prism", "R"),
                                       remove_emptys = c(TRUE, FALSE), outputfoldername){
  df <- read.xlsx(datawithpath,
                  startRow = 3,
                  skipEmptyCols = TRUE,
                  rowNames = FALSE, colNames = TRUE, check.names = TRUE) %>%
    rename("temp" = 'Temperature..C.') %>%
    filter(!is.na(temp))
  
  # The time formats are really funky. For time <24 hours, it's in a decimal format. For Time >= 24, it's in a hybrid "day.hour:minutes:seconds" format.
  firsthalf <- df[!(grepl("\\:", df$Time)), ] %>%
    mutate(time = round(as.numeric(Time)*24, 2)) %>%
    relocate(time) %>% select(-c(Time, temp))
  secondhalf <- df[grepl("\\:", df$Time), ] %>%
    separate(Time, into = c("days", "hours", "minutes", "seconds"), sep = "[.:]", convert = TRUE, fill = "left") %>%
    mutate(time = round(days*24 + hours + minutes/60 + seconds/(60*60), 2)) %>%
    relocate(time) %>% select(-c(temp, days, hours, minutes, seconds))
  
  # Combine and assign sample names
  gd <- bind_rows(firsthalf, secondhalf) %>% arrange(time)
  gd <- assign_wells(templatewithpath, gd, output, remove_emptys) # Rename columns based on samples
  gd <- gd[ , order(names(gd))]
  gd <- gd %>% relocate(time)
  
  # created melted plate
  mp <- melt_plate(gd)
  
  # save in various formats
  dir.create(here(outputfoldername), showWarnings = FALSE)
  if (output == "prism"){
    fn <- paste0(gsub(".xlsx", "", basename(datawithpath), fixed = TRUE), "_parsedforPrism.csv")
    fullfn <-here::here(outputfoldername, fn)
    cat("Saving: ", fullfn, "\n")
    write.csv(gd, file = fullfn, row.names = FALSE)
  } else if (output == "python") {
    fn <- paste0(gsub(".xlsx", "", basename(datawithpath), fixed = TRUE), "_parsedforPython.tsv")
    fullfn <-here::here(outputfoldername, fn)
    cat("Saving: ", fullfn, "\n")
    write.table(gd, file = fullfn, sep = "\t", row.names = FALSE)
  } else if (output == "R") {
    fn <- paste0(gsub(".xlsx", "", basename(datawithpath), fixed = TRUE), "_parsedforR.csv")
    fullfn <-here::here(outputfoldername, fn)
    cat("Saving: ", fullfn, "\n")
    write.csv(gd, file = fullfn, row.names = FALSE)
  }
  else {
    print("Not saving parsed file.")
  }
  
  return(setNames(list(gd, mp[[1]], mp[[2]]), c("data", "melted", "sample_data")))
}

# This follows Chris Marshall's scripts. There is also support within growthcurver to produce the d_gc and pdf with SummarizeGrowth.
# Note, I have since moved to using growthrates package to calculate these values, and general AUC calculations.
Marshall_curves <- function(data, melted, sample_data, trimtime, pdfpathandname){
  df <- data.frame(data())
  num_analyses <- length(names(df)) - 1
  d_gc <- data.frame(sample = character(num_analyses),
                     k = numeric(num_analyses),
                     n0  = numeric(num_analyses),
                     r = numeric(num_analyses),
                     t_mid = numeric(num_analyses),
                     t_gen = numeric(num_analyses),
                     auc_l = numeric(num_analyses),
                     auc_e = numeric(num_analyses),
                     sigma = numeric(num_analyses),
                     stringsAsFactors = FALSE)
  d_fit <- vector("list", num_analyses)
  names(d_fit) <- names(df[-1])
  trim_at_time <- trimtime # Change as necessary, would like to see the curves first.
  
  # df_t_all <- data.frame(t(df))
  # colnames(df_t_all) <- df_t_all[1,]
  # df_t_all <- df_t_all[-1,]
  # df_t_all$sample_name <- rownames(df_t_all)
  # df_t_all <- df_t_all %>% separate(sample_name, sep="\\.", c("strain", "condition", "plate","rep"), remove = FALSE)
  # sample_data <- df_t_all[, 289:292]
  m_df_all <- melted
  
  # Now, loop through all of the columns in the data frame. For each column,
  # run Growthcurver, save the most useful metrics in the output data frame,
  # and make a plot of all the growth curve data and their best fits.
  
  # First, create a plot for each of the wells in the 96-well plate.
  # Uncomment the next line to save the plots from your 96-well plate to a 
  # pdf file.
  pdf(pdfpathandname, height = 8.5, width = 11)
  par(mfcol = c(6,9))
  par(mar = c(0.25,0.25,0.25,0.25))
  y_lim_max <- max(df[,setdiff(names(df), "time")]) - min(df[,setdiff(names(df), "time")])
  
  n <- 1    # keeps track of the current row in the output data frame
  for (col_name in names(df)) {
    
    # Don't process the column called "time". 
    # It contains time and not absorbance data.
    if (col_name != "time") {
      
      # Create a temporary data frame that contains just the time and current col
      d_loop <- df[, c("time", col_name)]
      
      # Do the background correction.
      # Background correction option 1: subtract the minimum value in a column
      #                                 from all measurements in that column
      min_value <- min(d_loop[, col_name])
      d_loop[, col_name] <- d_loop[, col_name] - min_value
      # Background correction option 2: subtract the mean value of blank wells
      #                                 over the course the experiment
      #                                 (Replace B2, D8, G11 with the column
      #                                  names of your media-only wells)
      #d$blank <- apply(d[, c("B2", "D8", "G11")], 1, mean)
      #d$A1 <- d$A1 - d$blank
      
      # Now, call Growthcurver to calculate the metrics using SummarizeGrowth
      gc_fit <- SummarizeGrowth(data_t = d_loop[, "time"], 
                                data_n = d_loop[, col_name],
                                t_trim = trim_at_time,
                                bg_correct = "min")
      
      # Now, add the metrics from this column to the next row (n) in the 
      # output data frame, and increment the row counter (n)
      d_gc$sample[n] <- col_name
      d_gc[n, 2:9] <- c(gc_fit$vals$k,
                        gc_fit$vals$n0,
                        gc_fit$vals$r,
                        gc_fit$vals$t_mid,
                        gc_fit$vals$t_gen,
                        gc_fit$vals$auc_l,
                        gc_fit$vals$auc_e,
                        gc_fit$vals$sigma)
      d_fit[col_name] <- gc_fit["data"]
      n <- n + 1
      
      # Finally, plot the raw data and the fitted curve
      # Here, I'll just print some of the data points to keep the file size smaller
      n_obs <- length(gc_fit$data$t)
      idx_to_plot <- 1:20 / 20 * n_obs
      plot(gc_fit$data$t[idx_to_plot], gc_fit$data$N[idx_to_plot], 
           pch = 20, 
           xlim = c(0, trim_at_time), 
           ylim = c(0, y_lim_max),
           cex = 0.6, xaxt = "n", yaxt = "n")
      text(x = trim_at_time / 4, y = y_lim_max, labels = col_name, pos = 1)
      lines(gc_fit$data$t, predict(gc_fit$model), col = "red")
    }
  }
  # Uncomment the next line to save the plots from your 96-well plate to a file
  dev.off()
  
  #look for outliers with bad fits (large sigma values)
  gc_out <- as_tibble(d_gc)
  
  # Plot a histogram of the sigma values in order to check for outliers
  hist(gc_out$sigma, main = "Histogram of sigma values", xlab = "sigma")
  
  # Show the top 5 samples with the largest sigma value 
  # (with the worst model fit to the growth curve data)
  gc_out %>% top_n(10, sigma) %>% arrange(desc(sigma))
  
  pca_gc_out <- as_tibble(gc_out)
  rownames(pca_gc_out) <- gc_out$sample
  print(pca_gc_out)
  
  pca_test <- d_gc
  rownames(pca_test) <- pca_test$sample
  pca_test <- pca_test[ ,-1]
  
  # Do the PCA
  pca.res <- prcomp(pca_gc_out %>% dplyr::select(k:sigma), center=TRUE, scale=TRUE)
  pca.test <- prcomp(pca_test, center = TRUE, scale = TRUE)
  
  # Plot the results
  pca <- as_data_frame(list(PC1=pca.test$x[,1],
                     PC2=pca.test$x[,2],
                     samples = rownames(pca.test$x))) %>%
    ggplot(aes(x=PC1,y=PC2, label=samples)) +
    geom_text(size = 3,position=position_jitter(width=1,height=1))
  plot(pca)
  return( setNames(list(d_gc, d_fit, sample_data), c("data", "summarizedGrowth", "samples")) )
}

gc_plot <- function(.data, colors, title, growthscale){
  choice <- growthscale
  if (choice == "log2"){
    p <- ggplot(data = .data, mapping = aes(x=time, y=absorbance)) +
      geom_point( aes(color=strain), size = 0.2) + 
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(12,48, 4)) +
      labs(x="Time(h)", y="Absorbance at OD600", title = title) +
      scale_y_continuous(
        trans = log2_trans(),
        breaks = trans_breaks("log2", function(x) 2^x),
        labels = trans_format("log2", math_format(2^.x)))
  } else if (choice == "natural_log"){
    p <- ggplot(data = .data, mapping = aes(x=time, y=absorbance)) +
      geom_point( aes(color=strain), size = 0.2) + 
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(12,48, 4)) +
      labs(x="Time(h)", y="Absorbance at OD600", title = title) +
      scale_y_continuous(
        trans = log_trans(),
        breaks = trans_breaks("log", function(x) exp(x)),
        labels = trans_format("log", math_format(e^.x))
      )
  } else if (choice == "log10"){
    p <- ggplot(data = .data, mapping = aes(x=time, y=absorbance)) +
      geom_point( aes(color=strain), size = 0.2) + 
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(12,48, 4)) +
      labs(x="Time(h)", y="Absorbance at OD600", title = title) +
      scale_y_log10(breaks = 10^(0:3), labels = trans_format("log10", math_format(10^.x)))
  } else if (choice == "straight"){
    p <- ggplot(data = .data, mapping = aes(x=time, y=absorbance)) +
      geom_point( aes(color=strain), size = 0.2) + 
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(12,48, 4)) +
      labs(x="Time(h)", y="Absorbance at OD600", title = title)    
  } else print("Try again with a scale choice.")
  return(p)
}

# Convenience function to quickly get initial idea of what each plate looks like. Returns plots in a list.
plot_from_melted_plate <- function(.data, colors, title, yscale = c("log10", "natural_log", "log2", "straight")){
  conds <- unique(.data$condition)
  choice <- yscale
  plist <- lapply(conds, function(cond){
    title.cond <- paste(title, ", ", cond)
    temp <- .data %>% subset(condition == cond)
    p <- gc_plot(temp, colors, title.cond, choice)
    return(p)
  })
  return(plist)
}

# Prefered convenience function to get idea of what each plate looks like. Returns a facted plot object.
plot_from_melted_plate_faceted <- function(.data, colors, title, yscale = c("log10", "natural_log", "log2", "straight")){
  choice <- yscale
  p <- gc_plot(.data, colors, title, choice)
  p <- p + facet_grid(condition ~.)
  return(p)
}

# Convenience function that formats data for plotting
# Requires the strain and condtions factor inputs to ordered lists of the factor names.
format_GC <- function(df, strainfactors, conditionfactors){
  newdf <- df %>% mutate(
    strain = factor(strain, levels = strainfactors, ordered = TRUE),
    condition = factor(condition, levels = conditionfactors, ordered = TRUE),
    time = round(time*60, 0)/60 ) %>%
    rename("OD" = absorbance) %>%
    group_by(condition, strain, sample_name) %>%
    mutate(OD.corrected = OD - min(OD)) %>% ungroup(condition, strain, sample_name)
  return(newdf)
}
#If you are not correcting by the minimum OD, you should still have values in your data frame with sample_name == "blank".
#To use the average of these as the baseline, insert a line before the "newdf <- ..." line of code that reads:
#  baseline <- mean(df[ df$sample_name == "blank", absorbance])
#Then replace both of the lines "group_by( ..." and "mutate(OD.corrected ..." with:
#  mutate(OD.corrected = OD - baseline)'


# Convenience function to calculate the mean absorbance values for BASELINED data.
# Requires a list of the grouping variables, quotes.
mean_OD <- function(df, groupingvariables){
  dots <- lapply(groupingvariables, as.symbol)
  means.df <- df %>% group_by(.dots = dots) %>%
    summarize_at(vars(OD.corrected), list( means = mean, sds = sd, se = ~ sd(.)/sqrt(n()))) %>%
    mutate(lower = means - se, upper = means + se) %>% ungroup(all_of(groupingvariables))
  return(means.df)
}

# Convenience funtion to plot means with error ribbon. These are unfacted and with minimal formatting to allow later modifcation.
# Does expect a named character vector for colors
bare_meanplot <- function(df, namedcolors){
  ggplot(data = df, mapping = aes(x = time, y = means, color = strain)) +
    scale_color_manual(values = namedcolors) +
    scale_fill_manual(values = namedcolors) +
    geom_line() +
    geom_ribbon(mapping = aes(ymin = lower, ymax = upper, fill = strain),
                color = NA, alpha = 0.2) +
    labs(fill = "Genotype", color = "Genotype", x = "Time (h)", y = expression(Absorbance~(OD[600]))) +
    theme(
      strip.text.y = element_text(margin = margin(5, 5, 5, 5, unit = "pt"))) 
}