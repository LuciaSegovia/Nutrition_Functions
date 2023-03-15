
#---
#Title: Group Summariser
#Author: Thomas Codd
#Version: V1.1.0
#Changelog: V1.0.0 -> V1.1.0; Added the ability to take preset weights into account.
#Github: https://github.com/TomCodd/Nutrition_Functions
#---

library(tidyverse)
library (docstring)

Group_Summariser <- function(df, group_ID_col, secondary_sort_col, input_weighting_column, sep_row = F, seq_col = F, weighting_col = F, round_weighting = T){
  
  #' Insert summary rows between groups of rows in a data frame
  #' 
  #' @description This function analyses a data frame, sorting it based on the groups detailed in the group_ID_col,
  #' and inserts summary/mean rows in between each group. 
  #' 
  #' For this to work the data frame must be structured such that it has a group ID column of some sort, where the group
  #' of each item is listed. All the data columns that need to be averaged need to be numeric also. 
  #' 
  #' A secondary option is for the sorting of items within their groups, using the secondary_sort_col parameter. 
  #' 
  #' @param df Required - The data.frame that summary rows need to be inserted into.
  #' @param group_ID_col Required - The column name specifying the groups that summary rows are created for.
  #' @param secondary_sort_col Optional - Specify the column that the results should be sorted by after they're sorted into groups.
  #' @param input_weighting_column Optional - Specify a column which contains set weightings. If selected, these weightings will be used in the summariser instead of a set average. Where partial weightings are given for an item, the remaining matches will have their weightings split evenly between them.
  #' @param sep_row Optional - if set to \code{TRUE}, The Summariser will insert an empty row after each summary row, to help reading and separation.
  #' @param seq_col Optional - if set to \code{TRUE}, The Summariser will insert a sequence column, numbering each item that goes into a summary row.
  #' @param weighting_col Optional - if set to \code{TRUE}, The Summariser will insert a weighting factor for each item that goes into a summary row.
  #' @return A data.frame that mirrors \code{df}, but after each group a summary row is inserted, containing the mean of the data columns.
  
  
  # Data input checking ----
  
  #These checks are run on the inputs to make sure the data frames are data frames, and that the string input is just a string, and the string inputs are a legitimate column name
  
  numeric_cols <- select_if(df, is.numeric)
  
  example_colname <- colnames(df[1])
  
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))
  stopifnot("df contains no numeric columns. Please ensure data columns are numeric" = length(numeric_cols) != 0)
  stopifnot("The group_ID_col is not a character or string - please input a character or string that is a column name in df, e.g. 'column one'" = is.character(group_ID_col))
  stopifnot("The group_ID_col is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = group_ID_col %in% colnames(df))
  
  if(!missing(secondary_sort_col)){
    stopifnot("The secondary_sort_col is not a character or string - please input a character or string that is a column name in df, e.g. 'column one'" = is.character(secondary_sort_col))
    stopifnot("The secondary_sort_col is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = secondary_sort_col %in% colnames(df))
  }
  
  if(!missing(input_weighting_column)){
    stopifnot("The input_weighting_column is not a character or string - please input a character or string that is a column name in df, e.g. 'column two - weightings'" = is.character(input_weighting_column))
    stopifnot("The input_weighting_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two - weightings'" = input_weighting_column %in% colnames(df))
  }
  
  
  # Table sorting ----
  
  #This makes sure the group ID column is a character string, finds the unique ID's and sorts by them
  
  
  df[[group_ID_col]] <- as.character(df[[group_ID_col]])
  group_IDs <- df[[group_ID_col]]
  group_ID_list <- sort(unique(group_IDs))
  
  #This creates a new table, which will be the basis for the results table, using a subset of the main table only including the first group ID
  df <- as.data.frame(df)
  sorted_table <- df[df[[group_ID_col]] == group_ID_list[1],]
  
  weighting_col_present <- F
  
  if(!missing(input_weighting_column)){
    weighting_col_present <- T
    weighting_col <- T
    weightings_column <- sorted_table[[input_weighting_column]]
    weightings_column[weightings_column == ""] <- NA
    sorted_weights <- sum(as.numeric(weightings_column), na.rm = T)
    number_of_NA <- sum(is.na(weightings_column))
    remaining_total <- 1-sorted_weights
    if(remaining_total < 0){
      message(paste0("Error - weighting values for item ID ", unique(sorted_table[[group_ID_col]]), " are greater than 1. Weighting cannot be completed."))
      stop()
    }
    weightings_column[is.na(weightings_column)] <- remaining_total/number_of_NA
    sorted_table[[input_weighting_column]] <- as.numeric(weightings_column)
    weighting_total <- sum(sorted_table[[input_weighting_column]])
    if(weighting_total != 1){
      message(paste0("Error - weighting values for item ID ", unique(sorted_table[[group_ID_col]]), " do not total 1. Weighting cannot be completed."))
      stop()
    }
  }
  
  if(!missing(secondary_sort_col)){
    sorted_table <- sorted_table[order(sorted_table[, secondary_sort_col]),]
  }
  
  
  # Starting table setup ----
  
  #This creates a list for a new row, then iterates over the first subtable, generated averaged values where appropriate for the data columns.
  
  new_row <- c()
  
  for (i in 1:ncol(sorted_table)){
    new_row_entry <- NA
    unique_entries <- unique(sorted_table[[i]])
    if(length(unique_entries) == 1){
      if(paste(unique_entries) == "NA"){
        new_row_entry <- NA
      } else {
        new_row_entry <- paste(unique_entries)
      }
    } else {
      if(!missing(input_weighting_column)){
        if(colnames(sorted_table)[i] == input_weighting_column){
          new_row_entry <- 1
        } else {
          new_row_entry <- weighted.mean(as.numeric(sorted_table[[i]]), sorted_table[[input_weighting_column]], na.rm = F)
        }
      } else {
        new_row_entry <- mean(as.numeric(sorted_table[[i]]), na.rm = TRUE)
      }
    }
    new_row <- append(new_row, new_row_entry)
  }
  
  group_col_num <- which(colnames(sorted_table) == group_ID_col)
  group_ID_value <- new_row[group_col_num]
  new_row[group_col_num] <- paste0("SUMMARY ROW - ", group_ID_value)
  sorted_table <- rbind(sorted_table, new_row)
  
  if(weighting_col == T & weighting_col_present == F){
    if(round_weighting == T){
      sorted_table$Weighting_Factor <- c(replicate((nrow(sorted_table) - 1), round((1/(nrow(sorted_table) - 1)), 2)), "")
    } else if(round_weighting == F){
      sorted_table$Weighting_Factor <- c(replicate((nrow(sorted_table) - 1), (1/(nrow(sorted_table) - 1))), "")
    }
  }
  
  if(seq_col == T){
    sorted_table$Sequence <- c(1:(nrow(sorted_table) - 1), "")
  }
  if(sep_row == T){
    sorted_table[nrow(sorted_table) + 1,] <- ""
  }
  
  # Secondary tables setup ----
  
  #This repeats the process above, appending the results to the starter table.
  
  for (i in 2:length(group_ID_list)){
    
    secondary_table <- df[df[[group_ID_col]] == group_ID_list[i],]
    
    if(!missing(input_weighting_column)){
      weightings_column <- secondary_table[[input_weighting_column]]
      weightings_column[weightings_column == ""] <- NA
      sorted_weights <- sum(as.numeric(weightings_column), na.rm = T)
      number_of_NA <- sum(is.na(weightings_column))
      remaining_total <- 1-sorted_weights
      if(remaining_total < 0){
        message(paste0("Error - weighting values for item ID ", unique(secondary_table[[group_ID_col]]), " are greater than 1. Weighting cannot be completed."))
        stop()
      }
      weightings_column[is.na(weightings_column)] <- remaining_total/number_of_NA
      secondary_table[[input_weighting_column]] <- as.numeric(weightings_column)
      weighting_total <- sum(secondary_table[[input_weighting_column]])
      if(weighting_total != 1){
        message(paste0("Error - weighting values for item ID ", unique(secondary_table[[group_ID_col]]), " do not total 1. Weighting cannot be completed."))
        stop()
      }
    }
    
    if(!missing(secondary_sort_col)){
      secondary_table <- secondary_table[order(secondary_table[secondary_sort_col]),]
    }
    
    
    new_row <- c()
    
    for (j in 1:ncol(secondary_table)){
      new_row_entry <- NA
      unique_entries <- unique(secondary_table[[j]])
      if(length(unique_entries) == 1){
        if(paste(unique_entries) == "NA"){
          new_row_entry <- NA
        } else {
          new_row_entry <- paste(unique_entries)
        }
      } else {
        if(!missing(input_weighting_column)){
          if(colnames(secondary_table)[j] == input_weighting_column){
            new_row_entry <- 1
          } else {
            new_row_entry <- weighted.mean(as.numeric(secondary_table[[j]]), secondary_table[[input_weighting_column]], na.rm = F)
          }
        } else {
          new_row_entry <- mean(as.numeric(secondary_table[[j]]), na.rm = TRUE)
        }      
      }
      new_row <- append(new_row, new_row_entry)
    }
    group_col_num <- which(colnames(secondary_table) == group_ID_col)
    group_ID_value <- new_row[group_col_num]
    new_row[group_col_num] <- paste0("SUMMARY ROW - ", group_ID_value)
    
    secondary_table <- rbind(secondary_table, new_row)
    
    if(weighting_col == T & weighting_col_present == F){
      if(round_weighting == T){
        secondary_table$Weighting_Factor <- c(replicate((nrow(secondary_table) - 1), round((1/(nrow(secondary_table) - 1)), 2)), "")
      } else if(round_weighting == F){
        secondary_table$Weighting_Factor <- c(replicate((nrow(secondary_table) - 1), (1/(nrow(secondary_table) - 1))), "")
      }
    }
    
    if(seq_col == T){
      secondary_table$Sequence <- c(1:(nrow(secondary_table) - 1), "")
    }
    
    sorted_table <- rbind(sorted_table, secondary_table)
    
    if(sep_row == T){
      sorted_table[nrow(sorted_table) + 1,] <- ""
    }
    
  }
  
  return(sorted_table)
  
}