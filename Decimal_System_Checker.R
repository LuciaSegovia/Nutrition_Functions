
#---
#title: Decimal System Checker
#author: Thomas Codd
#Github: https://github.com/TomCodd/Nutrition_Functions
#---

library(docstring)

Decimal_System_Checker <- function(df, first, second, third, fourth) {

  #' A tool to test the integrity of a decimal system in a dataframe.
  #'
  #' @description This function reads in a dataframe, as well as the names of 2-4 columns which comprise the decimal system within that dataframe. 
  #' It then checks the integrity of each series of decimal identities in each row against the rest of the decimal identities within that row, picking up any inconsistencies.
  #' Any inconsistencies are reported, eith in console messages or in a error report dataframe.
  #'
  #'
  #' @param df Required - The data frame containing the decimal system.
  #' @param first Required - The first column of the decimal system - the most basic item ID; e.g. 01 .
  #' @param second Required - The second column of the decimal system - the first subdivision from the base ID; e.g. 01.005 .
  #' @param third Optional - The third column of the decimal system - the second subdivision from the base ID and the 
  #' first from the second ID; e.g. 01.005.03 .
  #' @param fourth Optional - The fourth column of the decimal system - the third subdivision from the base ID, second subdivision 
  #' from the second ID, and first subdivision from the third ID; e.g. 01.005.03.01 .
  #' 
  #' @return An R dataframe detailing the errors found for each item in the decimal system.
  #' 
  #' @examples #Two examples will be covered - one that results in the output error table, another that produces the output messages only (not recommended for large dataframes). 
  #' 
  #' #First, we must create a test dataframe:
  #' test_df <- data.frame(
  #'     c("Merlot",
  #'       "pinot grigio",
  #'       "Chateauneuf-du-Pape",
  #'       "Tokaji",
  #'       "Champagne",
  #'       "Sauvignon Blanc",
  #'       "Chardonnay",
  #'       "Malbec"),
  #'     c("01", "01", "01", "01", "02", "02", "02", "02"),
  #'     c("02.01", "01.01", "01.02", "01.02", "02.01", "02.01", "02.02", "02.02"),
  #'     c("02.01.0111", "01.01.0131", "01.02.0001", "01.02.2031", "02.01.1001", "02.01.1001", "02.02.3443", "02.03.4341"),
  #'     c("02.01.0111.01", "01.01.0131.04", "01.02.0001.01", "01.02.2031.03", "02.01.1001.06", "02.01.1001.06", "02.01.3443.02", "02.02.4341.03")
  #'   )
  #'   
  #'   #Then we should rename the columns of the dataframe:
  #'   
  #'   colnames(test_df) <-
  #'     c("Wine names",
  #'       "ID1",
  #'       "ID2",
  #'       "ID3",
  #'       "ID4"
  #'     )
  #'     
  #'  #This first line runs the dataframe, and has an output variable listed. This means that as well as putting a message in the console when an error is found, all the error reports will be saved to a dataframe too.   
  #'   
  #'  output_test <- Decimal_System_Checker(test_df, first = "ID1", second = "ID2", third = "ID3", fourth = "ID4")
  #'   
  #'  #However, if we only want to get the readouts and not have an error dataframe to refer back to, then the code can be run like so:
  #'  
  #'  Decimal_System_Checker(test_df, first = "ID1", second = "ID2", third = "ID3", fourth = "ID4")
  #'   
  #'  #This will do the same thing as the previous run, producing error printouts, but it will not create an error report dataframe.

  
  tertiary <- F #Sets if a third layer of the decimal system is used
  if (!missing(third)) {
    tertiary <- T
    message(cat("Tertiary decimal level used"))
  }
  
  
  quaternary <- F #Sets if a fourth layer of the decimal system is used
  if (!missing(fourth)) {
    quaternary <- T
    message(cat("Quaternary decimal level used"))
  }
  
  stopifnot( #This is a check to make sure the different levels of the decimal system are used correctly - i.e. the fourth level isn't being used without the third level being defined.
    "quaternary (fourth) decimal system rank used, without a tertiary (third) rank. Please input a third rank." = ((tertiary == T &&
                                                                                                                      quaternary == F) | (tertiary == T && quaternary == T)
    )
  )
  
  #This if/else block should show if there are duplicates in the lowest order of the decimal system.
  
  if(quaternary == T){
    duplicate_codes <- as.data.frame(table(df[[fourth]]))
    duplicate_codes <- duplicate_codes[duplicate_codes$Freq > 1,]
    if(length(duplicate_codes) > 0){
      message(paste0("duplicate codes found in fourth level: ", duplicate_codes$Var1))
    }
  } else if(tertiary == T){
    duplicate_codes <- data.frame(table(df[[third]]))
    duplicate_codes <- duplicate_codes[duplicate_codes$Freq > 1,]
    if(length(duplicate_codes) > 0){
      message(paste0("duplicate codes found in third level: ", duplicate_codes$Var1))
    }
  } else {
    duplicate_codes <- data.frame(table(df[[second]]))
    duplicate_codes <- duplicate_codes[duplicate_codes$Freq > 1,]
    if(length(duplicate_codes) > 0){
      message(paste0("duplicate codes found in second level: ", duplicate_codes$Var1))
    }
  }
  
  output_df <- data.frame(matrix(ncol = 5, nrow = nrow(df)))
  colnames(output_df) <- c("primary code", "secondary code", "tertiary code", "quaternary code", "error")
  
  total_error_count <- 0
  
  for(i in 1:nrow(df)){

    row_error_count <- 0
    row_error_code <- c()
        
    primary_value <- as.character(df[[first]][i])
    secondary_value <- as.character(df[[second]][i])
    
    secondary_value_split <- unlist(strsplit(secondary_value, ".", fixed = T))
    
    #This block adds duplicate error codes to the row codes character string
    if(quaternary == T){
      if(df[[fourth]][i] %in% duplicate_codes$Var1){
        row_error_code <- c(row_error_code, paste0("duplicate codes found in fourth level: ", df[[fourth]][i]))
        row_error_count <- row_error_count + 1
      }
    } else if(tertiary == T){
      if(df[[third]][i] %in% duplicate_codes$Var1){
        row_error_code <- c(row_error_code, paste0("duplicate codes found in third level: ", df[[third]][i]))
        row_error_count <- row_error_count + 1
        
      }
    } else {
      if(df[[second]][i] %in% duplicate_codes$Var1){
        row_error_code <- c(row_error_code, paste0("duplicate codes found in fourth level: ", df[[second]][i]))
        row_error_count <- row_error_count + 1
        
      }
    }
    
    if(secondary_value_split[1] != primary_value){
      print(df[[fourth]][i])
      message(paste0("The first part of the secondary code (", secondary_value_split[1], ") does not match the primary code (", primary_value, "); ", secondary_value, " vs. ", primary_value, "."))
      row_error_code <- c(row_error_code, paste0("The first part of the secondary code (", secondary_value_split[1], ") does not match the primary code (", primary_value, "); ", secondary_value, " vs. ", primary_value, "."))
      total_error_count <- total_error_count + 1
      row_error_count <- row_error_count + 1
    }
    
    if(tertiary == T){
      tertiary_value <- as.character(df[[third]][i])
      tertiary_value_split <- unlist(strsplit(tertiary_value, ".", fixed = T))

      if(tertiary_value_split[1] != primary_value){
        if(quaternary == T){
          print(df[[fourth]][i])
        } else if(tertiary == T){
          print(df[[third]][i])
        } else {
          print(df[[second]][i])
        }
        message(paste0("The first part of the tertiary code (", tertiary_value_split[1], ") does not match the primary code (", primary_value, "); ", tertiary_value, " vs. ", primary_value, "."))
        row_error_code <- c(row_error_code, paste0("The first part of the tertiary code (", tertiary_value_split[1], ") does not match the primary code (", primary_value, "); ", tertiary_value, " vs. ", primary_value, "."))
        total_error_count <- total_error_count + 1
        row_error_count <- row_error_count + 1
        
      }
      
      if(tertiary_value_split[2] != secondary_value_split[2]){
        if(quaternary == T){
          print(df[[fourth]][i])
        } else if(tertiary == T){
          print(df[[third]][i])
        } else {
          print(df[[second]][i])
        }
        message(paste0("The second part of the tertiary code (", tertiary_value_split[2], ") does not match the secondary code (", secondary_value_split[2], "); ", tertiary_value, " vs. ", secondary_value, "."))
        row_error_code <- c(row_error_code, paste0("The second part of the tertiary code (", tertiary_value_split[2], ") does not match the secondary code (", secondary_value_split[2], "); ", tertiary_value, " vs. ", secondary_value, "."))
        total_error_count <- total_error_count + 1
        row_error_count <- row_error_count + 1
      }
    }
    
    if(quaternary == T){
      quaternary_value <- as.character(df[[fourth]][i])
      quaternary_value_split <- unlist(strsplit(quaternary_value, ".", fixed = T))
      
      if(quaternary_value_split[1] != primary_value){
        if(quaternary == T){
          print(df[[fourth]][i])
        } else if(tertiary == T){
          print(df[[third]][i])
        } else {
          print(df[[second]][i])
        }
        message(paste0("The first part of the quaternary code (", quaternary_value_split[1], ") does not match the primary code (", primary_value, "); ", quaternary_value, " vs. ", primary_value, "."))
        row_error_code <- c(row_error_code, paste0("The first part of the quaternary code (", quaternary_value_split[1], ") does not match the primary code (", primary_value, "); ", quaternary_value, " vs. ", primary_value, "."))
        total_error_count <- total_error_count + 1
        row_error_count <- row_error_count + 1
      }
      
      if(quaternary_value_split[2] != secondary_value_split[2]){
        if(quaternary == T){
          print(df[[fourth]][i])
        } else if(tertiary == T){
          print(df[[third]][i])
        } else {
          print(df[[second]][i])
        }
        message(paste0("The second part of the quaternary code (", quaternary_value_split[2], ") does not match the secondary code (", secondary_value_split[2], "); ", quaternary_value, " vs. ", secondary_value, "."))
        row_error_code <- c(row_error_code, paste0("The second part of the quaternary code (", quaternary_value_split[2], ") does not match the secondary code (", secondary_value_split[2], "); ", quaternary_value, " vs. ", secondary_value, "."))
        total_error_count <- total_error_count + 1
        row_error_count <- row_error_count + 1

      }
      
      if(quaternary_value_split[3] != tertiary_value_split[3]){
        if(quaternary == T){
          print(df[[fourth]][i])
        } else if(tertiary == T){
          print(df[[third]][i])
          } else {
        print(df[[second]][i])
          }
        message(paste0("The third part of the quaternary code (", quaternary_value_split[3], ") does not match the tertiary code (", tertiary_value_split[3], "); ", quaternary_value, " vs. ", tertiary_value, "."))
        row_error_code <- c(row_error_code, paste0("The third part of the quaternary code (", quaternary_value_split[3], ") does not match the tertiary code (", tertiary_value_split[3], "); ", quaternary_value, " vs. ", tertiary_value, "."))
        total_error_count <- total_error_count + 1
        row_error_count <- row_error_count + 1
      }
    }      
  
    if(row_error_count > 0){
      
      
      
      output_df[i, 1] <- primary_value
      output_df[i, 2] <- secondary_value
      output_df[i, 5] <- paste0(row_error_code, collapse = " - ")
      
      if(tertiary == T){
        output_df[i, 3] <- tertiary_value
      }
      
      if(quaternary == T){
        output_df[i, 4] <- quaternary_value
      }
    }
  }
  output_df <- output_df[rowSums(is.na(output_df)) != ncol(output_df), ] 
  return(output_df)
}