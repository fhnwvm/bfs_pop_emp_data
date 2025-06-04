library(data.table)
library(dplyr)
rm(list = ls())
# Load and convert to data.table
statpop <- fread("_data/ag-b-00.03-vz2023statpop/STATPOP2023.csv", sep = ";")
statpop_noloc <- fread("_data/ag-b-00.03-vz2023statpop/STATPOP2023_NOLOC.csv", sep = ";")
statent <- fread("_data/ag-b-00.03-22-STATENT2021/STATENT_2021.csv")

#Define which columns aren't necessary to be kept from both tables
excluded_cols <- c("RELI", "GDENR", "X_KOORD", "Y_KOORD", "E_KOORD", "N_KOORD")

#Merge the 2 tables by "RELI" make sure the NOLOC tables gets an _n suffix added to all it's variables
corrected_statpop <- merge(statpop, statpop_noloc, by = "RELI", all.x = TRUE, suffixes = c("", "_n"))

#Define from which colums we need to find the difference to correct the original data
all_cols <- names(corrected_statpop)
cols_to_correct <- setdiff(all_cols, c(excluded_cols, paste0(excluded_cols, "_n")))
cols_to_correct <- setdiff(names(statpop), excluded_cols)

#subtract the noloc data from the orignal data where applicable
for (col in cols_to_correct) {
  n_col <- paste0(col, "_n")
  corrected_statpop[, (col) := get(col) - fifelse(is.na(get(n_col)), 0, get(n_col))]
}

#drop the columns ending in _n now that they have been subtracted
cols_to_drop <- grep("_n$", names(corrected_statpop), value = TRUE)
corrected_statpop[, (cols_to_drop) := NULL]

#If fewer than 3 people of each sex are present in a cell, assume the real value to be half of the total population but less than 3
corrected_statpop[, ew_total_m := fifelse(
  B20BTOT == 3 & B20BMTOT == 3 & B20BWTOT == 3, 1.5,
  fifelse(B20BTOT == 3 & B20BMTOT == 3 & B20BWTOT == 0, 3,
          fifelse(B20BTOT > 3 & B20BMTOT == 3 & B20BWTOT == 3, round(B20BTOT / 2.0, 2),
          fifelse(B20BTOT > 3 & B20BWTOT > 3 & B20BMTOT == 3, B20BTOT - B20BWTOT,
                          B20BMTOT))))]

corrected_statpop[, ew_total_w := fifelse(
  B20BTOT == 3 & B20BMTOT == 3 & B20BWTOT == 3, 1.5,
  fifelse(B20BTOT == 3 & B20BWTOT == 3 & B20BMTOT == 0, 3,
          fifelse(B20BTOT > 3 & B20BMTOT == 3 & B20BWTOT == 3, round(B20BTOT / 2.0, 2),
          fifelse(B20BTOT > 3 & B20BMTOT > 3 & B20BWTOT == 3, B20BTOT - B20BMTOT,
                          B20BWTOT))))
]
  
#Sum the age groups based on if there are more or less than 3 people 
    setm <- c(paste0("B20BM", sprintf("%02d", 1:19)))
    setw <- c(paste0("B20BW", sprintf("%02d", 1:19)))
    
    corrected_statpop[, `:=`(
      temp_total_m = rowSums(.SD > 3) * 0 + rowSums(.SD * (.SD > 3)), # only values > 3
      temp_total_three_m = rowSums(.SD <= 3) * 0 + rowSums(.SD * (.SD <= 3)) # only values ≤ 3
    ), .SDcols = setm]
    
    corrected_statpop[, `:=`(
      temp_total_w = rowSums(.SD > 3) * 0 + rowSums(.SD * (.SD > 3)),
      temp_total_three_w = rowSums(.SD <= 3) * 0 + rowSums(.SD * (.SD <= 3))
    ), .SDcols = setw]

#Add the modified age group totals back into the table to get more accurate values
    setDT(corrected_statpop)  
    
    bm_cols <- sprintf("B20BM%02d", 1:19)
    new_cols_m <- sprintf("ew_%s_m", 
                        c("0_4","5_9","10_14","15_19","20_24","25_29","30_34",
                          "35_39","40_44","45_49","50_54","55_59","60_64",
                          "65_69","70_74","75_79","80_84","85_89","90_older"))
    
    for (i in seq_along(bm_cols)) {
      b_col <- bm_cols[i]
      ew_col <- new_cols_m[i]
      
      corrected_statpop[, (ew_col) := fifelse(
        get(b_col) > 3,
        get(b_col),
        fifelse(
          (ew_total_m - temp_total_m) == 0 | temp_total_three_m == 0,
          0,
          round((get(b_col) / temp_total_three_m) * (ew_total_m - temp_total_m), 2)
        )
      )]
    }
    
    # Female
    bw_cols <- sprintf("B20BW%02d", 1:19)
    new_cols_w <- sprintf("ew_%s_w", 
                          c("0_4","5_9","10_14","15_19","20_24","25_29","30_34",
                            "35_39","40_44","45_49","50_54","55_59","60_64",
                            "65_69","70_74","75_79","80_84","85_89","90_older"))
    

    for (i in seq_along(bw_cols)) {
      b_col <- bw_cols[i]
      ew_col <- new_cols_w[i]
      
      corrected_statpop[, (ew_col) := fifelse(
        get(b_col) > 3,
        get(b_col),
        fifelse(
          (ew_total_w - temp_total_w) == 0 | temp_total_three_w == 0,
          0,
          round((get(b_col) / temp_total_three_w) * (ew_total_w - temp_total_w), 2)
        )
      )]
    }

    #remove uneccessary columns
    cols_to_keep <- c("RELI", "X_KOORD", "Y_KOORD", "E_KOORD", "N_KOORD", "B20BTOT", "B20BMTOT", "B20BWTOT",
                      sprintf("ew_%s_m", 
                              c("0_4","5_9","10_14","15_19","20_24","25_29","30_34",
                                "35_39","40_44","45_49","50_54","55_59","60_64",
                                "65_69","70_74","75_79","80_84","85_89","90_older")),
                      sprintf("ew_%s_w",
                              c("0_4","5_9","10_14","15_19","20_24","25_29","30_34",
                                "35_39","40_44","45_49","50_54","55_59","60_64",
                                "65_69","70_74","75_79","80_84","85_89","90_older")))
    
    corrected_statpop <- corrected_statpop %>%
      select(all_of(cols_to_keep)) %>%
      rename(TOT = B20BTOT) 

    # Save new Datafile
    fwrite(corrected_statpop, "_data/ag-b-00.03-vz2020statpop/STATPOP_Corrected_2020.csv")


    merged <-  merge(corrected_statpop, statent, by = "RELI", all = TRUE)
    merged[, E_KOORD := fifelse(!is.na(E_KOORD.x), E_KOORD.x, E_KOORD.y)]
    merged[, N_KOORD := fifelse(!is.na(N_KOORD.x), N_KOORD.x, N_KOORD.y)]
    cols_to_keep <- c("RELI", "E_KOORD", "N_KOORD", "TOT", "B20BWTOT", "B20BMTOT")
    corrected_statpop <- merged %>%
      select(all_of(cols_to_keep)) 
    fwrite(corrected_statpop, "_data/input/2020.csv")
    