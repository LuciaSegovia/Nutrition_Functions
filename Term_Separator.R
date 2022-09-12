library(tidyverse)
library(fuzzyjoin)
library(rhandsontable)
library(shiny)
library (docstring)



############################################
test_dataset <- read.csv("C:/Users/sbztbc/OneDrive - The University of Nottingham/Documents/Consumption_Data/Tanzania/Tanzania_HCD_food_items.csv")
separation_terms <- c(",", "and")
############################################

df <- test_dataset
second_test_df <- data.frame(c(1:4), c("food, item, 1 and 2", "oranges and bananas", "jams, jellies", "the same thing, twice"), c(TRUE, FALSE, TRUE, FALSE), c(TRUE, TRUE, FALSE, FALSE))

Column_Creator <- function(df, name){ #This function creates an empty column.
  new_column_name <- paste0("separate by '", name, "'?")
  df$x <- FALSE
  colnames(df)[colnames(df) == "x"] <- new_column_name
  return(df)
}

Term_Counter <- function(string, term_list){ #This function counts the number of times an item in a list of strings appears in a main string.
  term_number <- 0
  for (i in 1:length(term_list)){
    term <- term_list[i]
    term_number <- term_number + lengths(regmatches(string, gregexpr(term, string)))
  }
  return(term_number)
}

#Currently, the input is two dataframes, stripped down to their ID and item name 
#columns, in that order

Term_Separator <- function(df, separation_terms = c(",", "and")){ #Focus term is a string that 
  #makes the filtering more lenient - use to catch more items with this term in 
  #them. Default is "raw".
  
  #' A GUI interface to match rows in two dataframes to each other via a fuzzy 
  #' string search
  #' 
  #' @description This function reads in two dataframes, both comprised of an ID
  #' row and a name row. The name rows are matched based on fuzzy search 
  #' suggestions and human confirmation using the GUI interface.
  #' 
  #' 
  #' @param df Required - The primary data frame, with items that need matches.
  #' The first column must be the ID column, the second must be the item names.
  #' @param separation_terms Optional - Specify a list of strings. The terms in 
  #' the list are used as potential separation points for the names in df1. 
  #' Defaults to "," and "and".
  #' @return A dataframe consisting of the contents of \code{df} with the 
  #' specified terms seperated.
  
  function_environment <- environment()
  
  # Data input checking ----
  
  #These checks are run on the inputs to make sure the data frames are data frames and the correct length, and that the string input is just a string
  
  stopifnot("df is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df))
  stopifnot("df is too long - please make sure the input dataframes are two columns in length." = (length(df) == 2))

  if(!missing(separation_terms)){
    stopifnot("The separation terms are not a character or string - please input a character or string, e.g. c(',', 'and')" = is.character(separation_terms))
  }
  
  # Data pre-processing ----
  
  #Starting checks - the timer is started, dataframe metadata is gathered, and columns are renamed
  #Also column name creation, as some quirk means it doesn't work when its wanted later on
  
  start_time <- Sys.time() #Start time for the timer is set
  
  
  df_names <- colnames(df) #original column names are taken for preservation
  
  for (i in (1:length(separation_terms))){ #This creates a column for each of the seperation terms.
    df <- Column_Creator(df, separation_terms[i])
  }
  
  new_column_names <- c("order", "Item Code", "Item Name") #Creates a list of new column names to be applied to the table
  for (i in (3:ncol(df))){
    additional_name <- colnames(df)[i]
    new_column_names <- c(new_column_names, additional_name) #This adds the seperation term column names to the new column name list
  }

  df <- transform(df, order = Term_Counter(df[,2], separation_terms)) #This creates the order column, and for each row it counts the number of times a seperation term appears in the name column and adds the number here
  
  df <- df %>% relocate(order) #puts the order column to the front of the dataframe
  colnames(df) <- new_column_names #renames the column names
  df <- df[order(-df$order),] #sorts the data frame in descending order by the "order" column - the names with the most separation terms should appear  at the top.
  
  sep_df <- df[df$order > 0,]
  no_sep_df <- df[df$order == 0,]
  
  # RShiny - Match confirmation ----

  DF <- sep_df
  
  ui <- (fluidPage( #this outlines the main page design
    fluidRow(
      column(12,
             h1("Select terms to separate", align = "center"))),
    fluidRow(
      column(12,
             actionButton("saveBtn", "All terms separated"))),
    fluidRow(
      column(12,
             br())),
    fluidRow(
      column(12,
             rHandsontableOutput("table", height = "500px"))),
  )
  )
  
  server <- (function(input, output, session){
    
    values <- reactiveValues(data = DF) #Imports the data as reactiveValues
    
    observeEvent(input$table,{
      input_table <- as.data.frame(hot_to_r(input$table)) #Makes the table "hot" - i.e. interact-able with rhandsontable
      values$data <- input_table
    })
    
    output$table <- renderRHandsontable({
      rhandsontable(values$data, rowHeaders = NULL) %>% #outputs the data table
        hot_col(1:3, readOnly = TRUE) %>% #Outputs the table, and makes it so that only the checkbox columns are editable
        hot_col(1, width = 0.5) %>% #makes the order column unseeable
        hot_col(4:ncol(df), type = "checkbox") #makes the seperation columns a checkbox
    })
    
    observeEvent(input$saveBtn, { #Controls what happens when the save button is pressed
      output_table <- as.data.frame(hot_to_r(input$table)) #Creates an output table from the current data table
      print(output_table)
      selected_for_seperation <- output_table[which(apply(output_table, 1, any)),] #Finds any column with TRUE in it - i.e. if a checkbox was ticked
      not_selected_for_separation <- output_table[-which(apply(output_table, 1, any)),2:3]
      separated_terms_df <- output_table[-c(1:nrow(output_table)),]
      export_table <- data.frame()
      for (i in 1:nrow(selected_for_seperation)){
        focus_row <- selected_for_seperation[i,]
        true_locations <- (which(focus_row == TRUE, arr.ind = TRUE))
        true_columns <- true_locations[,2]
        offset_columns <- (true_columns - 3) > 0
        terms_to_sep <- separation_terms[offset_columns]
        separation_list <- str_split(focus_row[1,3], paste(terms_to_sep, collapse = "|"))
        for (c in 1:length(separation_list[[1]])){
          new_row <- c(paste0(focus_row[1,2], "_TSEP_", c), separation_list[[1]][c])
          print(new_row)
          export_table <- rbind(export_table, new_row)
        }
      }
      assign(paste0("TermSep_Output_", Sys.time()), export_table, envir = .GlobalEnv)
      stopApp()
        })
  })
  shinyApp(ui, server)
  
  #print(term_sep_sheet)
}
