CARTBEQ_standardised <-
  function(df,
           CARTBmcg,
           CARTAmcg,
           CRYPXBmcg,
           CARTBEQmcg_std,
           CARTBEqmcg,
           VITA_RAEmcg,
           VITAmcg,
           RETOLmcg,
           comment_col,
           item_ID,
           compile = T,
           carotene = T,
           comment = T) {
    input_length <-
      max(
        length(CARTBmcg),
        length(CARTAmcg),
        length(CRYPXBmcg),
        length(CARTBEQmcg_std),
        length(CARTBEqmcg),
        length(VITA_RAEmcg),
        length(VITAmcg),
        length(RETOLmcg),
        length(comment_col),
        length(item_ID)
      )
    stopifnot("No input values assigned (input max length < 1). Please assign input values." = input_length > 1)

    comments_col_asstr <- as.character(substitute(comment_col))
    
    recalculated_CARTBEQmcg_std_list <- c()
    comment_list <- c()
    
    for(i in 1:input_length){
      if (CARTBmcg[i] != "" &
          CARTAmcg[i] != "" &
          CRYPXBmcg[i] != "" &
          !is.na(CARTBmcg[i]) & !is.na(CARTAmcg[i]) & !is.na(CRYPXBmcg[i])) {
        recalculated_CARTBEQmcg_std <-
          (as.numeric(CARTBmcg[i]) + 0.5 * as.numeric(CARTAmcg[i]) + 0.5 * as.numeric(CRYPXBmcg[i]))
        if (comment == T) {
          recorded_comment <-
            paste0("CARTBEQ_standardised calculated using standard equation")
        }
      } else if (compile == T &
                 CARTBEQmcg_std[i] != "" & !is.na(CARTBEQmcg_std[i])) {
        recalculated_CARTBEQmcg_std <- as.numeric(CARTBEQmcg_std[i])
        if (comment == T) {
          recorded_comment <-
            paste0("CARTBEQ_standardised compiled from previous CARTBEQmcg_std value")
        }
      } else if (compile == T &
                 CARTBEqmcg[i] != "" & !is.na(CARTBEqmcg[i])) {
        recalculated_CARTBEQmcg_std <- as.numeric(CARTBEqmcg[i])
        if (comment == T) {
          recorded_comment <-
            paste0("CARTBEQ_standardised compiled from previous CARTBEqmcg value")
        }
      } else if (carotene == T & CARTBmcg[i] != "" & !is.na(CARTBmcg[i])) {
        recalculated_CARTBEQmcg_std <- as.numeric(CARTBmcg[i])
        if (comment == T) {
          recorded_comment <-
            paste0("CARTBEQ_standardised compiled from previous CARTBmcg value")
        }
      } else if (VITA_RAEmcg[i] != "" &
                 !is.na(VITA_RAEmcg[i]) &
                 RETOLmcg[i] != "" & !is.na(RETOLmcg[i])) {
        recalculated_CARTBEQmcg_std <-
          (as.numeric(VITA_RAEmcg[i]) - as.numeric(RETOLmcg[i])) * 12
        if (comment == T) {
          recorded_comment <-
            paste0("CARTBEQ_standardised calculated from VITA_RAE and RETOL")
        }
      } else if (VITAmcg[i] != "" &
                 !is.na(VITAmcg[i]) &
                 RETOLmcg[i] != "" & !is.na(RETOLmcg[i])) {
        recalculated_CARTBEQmcg_std <-
          (as.numeric(VITAmcg[i]) - as.numeric(RETOLmcg[i])) * 6
        if (comment == T) {
          recorded_comment <-
            paste0("CARTBEQ_standardised calculated from VITA and RETOL")
        }
      } else {
        recalculated_CARTBEQmcg_std <- NA
        if (comment == T) {
          recorded_comment <-
            paste0("CARTBEQ_standardised could not be calculated")
        }
      }
      
      if(recalculated_CARTBEQmcg_std < 0){
        recalculated_CARTBEQmcg_std <- 0
        recorded_comment <- paste0(recorded_comment, " - recalculated_CARTBEQmcg_std calculated to be less than 0. Value reset to 0")
      }
      
      comment_list <- c(comment_list, paste0(comment_col[i], " - ", recorded_comment))
      recalculated_CARTBEQmcg_std_list <- c(recalculated_CARTBEQmcg_std_list, recalculated_CARTBEQmcg_std)
      message(
       paste0(
         "Item ",
         item_ID[i],
          " CARTBEQ_standardised calculated to be ",
          recalculated_CARTBEQmcg_std,
          "mcg. ",
          recorded_comment,
          "."
        )
      )
    }
    
    return_df <- df
    
    if (comment == T) {
      prev_comment_col_name <- as.character(substitute(comment_col))[3]
      eval(parse(text = paste0("return_df$", prev_comment_col_name, " <- comment_list")))
    }

    return_df$recalculated_CARTBEQmcg_std <- recalculated_CARTBEQmcg_std_list
    return(return_df)
  }

##Testing area:

test_df <- data.frame(
  c(
    "test_01",
    "test_02",
    "test_03",
    "test_04",
    "test_05",
    "test_06",
    "test_07",
    "test_08",
    "test_09",
    "test_10"
  ),
  c(
    "Merlot",
    "pinot grigio",
    "Chateauneuf-du-Pape",
    "Tokaji",
    "Champagne",
    "Sauvignon Blanc",
    "Chardonnay",
    "Malbec",
    "Cabernet Sauvignon",
    "Pinot Noir"
  ),
  c(NA, 105, "", 130, NA, "", 111, NA, 112, 101),
  c(0, 35, 23, 27, 6, 34, NA, 18, "", 40),
  c(110, 67, 72, NA, 160, 102, 98, 37, 28, 60),
  c("", 107, 102, NA, "", NA, 72, "", "", 143),
  c(159, 103, 132, NA, "", "", "", 78, NA, 92),
  c(13, 8, NA, 15, 13, NA, NA, NA, 7, 10),
  c(12, 11, 8, 13, 3, 1, 10, 15, 3, 6),
  c(0, 7, 12, NA, 5, 2, 10, 6, "", 1),
  c(
    "Real values, but for Maize, not Merlot",
    "Completely fictional values",
    "Fictional values #2",
    "More fictional values",
    "Fictional #4",
    "Fictional no. 5",
    "fictional 6",
    "more fiction",
    "again, fiction",
    "Fictional number 9"
  )
)

colnames(test_df) <-
  c(
    "ID",
    "food_name",
    "CART B (mcg)",
    "CART A (mcg)",
    "CRYP XB (mcg)",
    "CART B eq (std) (mcg)",
    "CART B eq (mcg)",
    "Vit A RAE (mcg)",
    "Vit A (mcg)",
    "Retinol (mcg)",
    "comments"
  )

test_df_2 <-
  CARTBEQ_standardised(
    df = test_df,
    item_ID = test_df$ID,
    CARTBmcg = test_df$`CART B (mcg)`,
    CARTAmcg = test_df$`CART A (mcg)`,
    CARTBEQmcg_std =  test_df$`CART B eq (std) (mcg)`,
    CARTBEqmcg = test_df$`CART B eq (mcg)`,
    CRYPXBmcg = test_df$`CRYP XB (mcg)`,
    VITA_RAEmcg = test_df$`Vit A RAE (mcg)`,
    VITAmcg = test_df$`Vit A (mcg)`,
    RETOLmcg = test_df$`Retinol (mcg)`,
    comment_col = test_df$comments,
    compile = T,
    carotene = T,
    comment = T
  )