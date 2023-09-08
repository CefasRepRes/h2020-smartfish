# data organisation script ------------------------------------------------

## a script `sourced` by the .Rmd to organise the data for the analysis.

# info --------------------------------------------------------------------

## use changelog
# snippet changelog

## use snippets for todos
# snippet todo-bug
# snippet todo-check-me
# snippet todo-document-me
# snippet todo-fix-me
# snippet todo-optimise-me
# snippet todo-test-me

# use snippets for code chunks
# snippet saveplot
# snippet loadlatestdata


# change log --------------------------------------------------------------

## changelog

##! CHANGE LOG: (20221014 13:31:07) modified to allow for zero catches in haul- and species-specific length bins. 


# additional libraries ----------------------------------------------------


# additional setup --------------------------------------------------------


# raw data ----------------------------------------------------------------

## read in raw data
# use data.table::fread for csv
# use readxl::read_xlsx for xslx
# use odbc::dbConnect in "Connections" tab for accdb
# use odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/SG14/.../test.mdb") for mdb / accdb / etc
if (file.exists(here("data", len_nm))) {
  len_all <- read_xlsx(here("data", len_nm), sheet = "length data")
} else {
  len_all <- fread(here("data", gsub("xlsx", "csv", len_nm)))
}
len_all_dt <- data.table(len_all)


# variable specifications -------------------------------------------------

## fix factor levels
len_all_dt$`CODEND (top/bottom)` <- factor(len_all_dt$`CODEND (top/bottom)`,
                                           levels = c("U", "L"),
                                           labels = c("Upper", "Lower"))
len_all_dt$CONDITION <- factor(len_all_dt$CONDITION,
                               levels = c("D", "M", "N"),
                               labels = c("Day", "Mixed", "Night"))
len_all_dt$SPECIES <- factor(len_all_dt$SPECIES)


# sort data ---------------------------------------------------------------

## sort to ensure consistency
setorder(len_all_dt, 
         "TRIAL", # constant: "C5979R"
         "TRIP.CODE", # variable: "C5979R_2", "C5979R_3" or "C5979R_4"
         "HAUL", # variable: 1 to 28 (unique between TRIP.CODE)
         "MESH", # constant: 87
         "RIG (port/starboard)", # variable: "P" or "S"
         "SPECIES", # variable: 56 species, including those in "sp_cd"
         "CODEND (top/bottom)",  # variable: "L" or "U"
         "DISCARD", # variable: "N" or "Y"
         "CATEGORY", # constant: 1
         "SEX", # variable: "B", "F", "M" or "U" (mostly "U")
         "RF", # variable: 1 to 28.75
         "LENGTH.CM", # variable: 4 to 168
         "N.AT.LENGTH", # variable: 1 to 33 (mostly 1 or 2)
         "N.RAISED.HAUL", # variable: 1 to 287.5 (mostly 1)
         "POSITION", # variable: "F" or "H"
         "CONDITION", # variable: "D", "M" or "N"
         "LIGHTRIG", # variable: "P" or "S"
         "LIGHTS", # variable: "NO" or "YES"
         "YES", # empty
         "NO"# empty
)


# remove redundancy -------------------------------------------------------

## catches in some LENGTH.CM (within variable columns) are DISCARD == Y & N
## (ignores SEX, which is not in this analysis)
if (0) { # set to 1 to run
  
  ## get unique combos of variable columns, not RF, N.AT.LENGTH, N.RAISED.HAUL
  foo <- unique(len_all_dt[, .(TRIP.CODE, HAUL, `RIG (port/starboard)`, 
                               SPECIES, `CODEND (top/bottom)`, SEX,
                               CONDITION, POSITION, LIGHTS, LENGTH.CM,
                               DISCARD)])
  
  ## count the number of DISCARD values for each combo 
  foo <- foo[, 
             .(n_uni_disc_vals = length(unique(DISCARD))), 
             by = .(TRIP.CODE, HAUL, `RIG (port/starboard)`,
                    SPECIES, `CODEND (top/bottom)`, SEX,
                    CONDITION, POSITION, LIGHTS, LENGTH.CM)]
  
  ## find subset where there are >1 DISCARD values
  fii <- subset(foo, n_uni_disc_vals > 1)
  
  ## examples:
  
  ### WHG (whiting)
  ex_whg <- subset(len_all_dt, 
                   SPECIES == "WHG" & 
                     `RIG (port/starboard)` == "S" & 
                     `CODEND (top/bottom)` == "Upper" & 
                     HAUL == 10 & 
                     LENGTH.CM == 43)[, .(SPECIES, HAUL, CONDITION, LIGHTS, 
                                          `CODEND (top/bottom)`, 
                                          LENGTH.CM, N.AT.LENGTH, RF, 
                                          N.RAISED.HAUL, DISCARD)]
  print(ex_whg)
  
  ### HAD (haddock)
  ex_had <- subset(len_all_dt, 
                   SPECIES == "HAD" & 
                     `RIG (port/starboard)` == "P" & 
                     `CODEND (top/bottom)` == "Upper" & 
                     HAUL == 7 & 
                     LENGTH.CM == 42)[, .(SPECIES, HAUL, CONDITION, LIGHTS, 
                                          `CODEND (top/bottom)`, 
                                          LENGTH.CM, N.AT.LENGTH, RF, 
                                          N.RAISED.HAUL, DISCARD)]
  print(ex_had)
  
  ## mostly in HAD, WHG and MEG
  print(fii[, .N, by = SPECIES][order(N, decreasing = TRUE), ])
  
  ## get all corresponding records from len_all_dt
  fee <- len_all_dt[fii, on = .(TRIP.CODE, HAUL, `RIG (port/starboard)`, 
                                SPECIES, `CODEND (top/bottom)`, SEX,
                                CONDITION, POSITION, LIGHTS, LENGTH.CM)]
  
}

## combine and remove over DISCARD (& SEX)
len_all_dt <- len_all_dt[,
                         .(RF = weighted.mean(RF, N.AT.LENGTH),
                           N.AT.LENGTH = sum(N.AT.LENGTH),
                           N.RAISED.HAUL = sum(N.RAISED.HAUL)),
                         by = setdiff(colnames(len_all_dt), 
                                      c("DISCARD",
                                        "RF", 
                                        "N.AT.LENGTH", 
                                        "N.RAISED.HAUL"))]

## check worked for examples above
if (0) { # set to 1 to run
  
  ### WHG (whiting)
  ex_whg_fixed <- subset(len_all_dt, 
                         SPECIES == "WHG" & 
                           `RIG (port/starboard)` == "S" & 
                           `CODEND (top/bottom)` == "Upper" & 
                           HAUL == 10 & 
                           LENGTH.CM == 43)[, .(SPECIES, HAUL, CONDITION, LIGHTS, 
                                                `CODEND (top/bottom)`, 
                                                LENGTH.CM, N.AT.LENGTH, RF, 
                                                N.RAISED.HAUL)]
  print(ex_whg_fixed)
  
  ### HAD (haddock)
  ex_had_fixed <- subset(len_all_dt, 
                         SPECIES == "HAD" & 
                           `RIG (port/starboard)` == "P" & 
                           `CODEND (top/bottom)` == "Upper" & 
                           HAUL == 7 & 
                           LENGTH.CM == 42)[, .(SPECIES, HAUL, CONDITION, LIGHTS, 
                                                `CODEND (top/bottom)`, 
                                                LENGTH.CM, N.AT.LENGTH, RF, 
                                                N.RAISED.HAUL)]
  print(ex_had_fixed)
  
}


# reduce to variables of interest -----------------------------------------

## remove footrope rigged hauls:
len_all_dt <- subset(len_all_dt, POSITION != "F")

## variables to drop
drp_vars <- c("TRIAL", "TRIP.CODE", "MESH", "RIG (port/starboard)", 
              "CATEGORY", "SEX", "POSITION", "LIGHTRIG", "YES", "NO")
len_all_dt[, drp_vars] <- NULL

## variables to rename
rnm_vars <- c("CODEND" = "CODEND (top/bottom)", "SPECIES_CODE" = "SPECIES")
setnames(len_all_dt, rnm_vars, names(rnm_vars))


# reduce to contrasts of interest -----------------------------------------

## species of interest
len_all_dt$SPECIES_NAME <- factor(len_all_dt$SPECIES_CODE,
                                  levels = sp_cd,
                                  labels = names(sp_cd))

## limit to species of interest
len_all_dt <- len_all_dt[!is.na(SPECIES_NAME), ]

## remove mixed condition
len_all_dt <- subset(len_all_dt, CONDITION != "Mixed")

## refactorise
len_all_dt$SPECIES_CODE <- factor(len_all_dt$SPECIES_CODE)
len_all_dt$CONDITION <- factor(len_all_dt$CONDITION)


# add subsample fraction --------------------------------------------------

## add sub-sample fraction
len_all_dt[, Q := (1 / RF)]


# ensure all size classes present -----------------------------------------

## get unique haul data
foo1 <- unique(len_all_dt[, c("HAUL", "CONDITION", "LIGHTS", "CODEND")])

## define length distribution probability
probs <- c(0, 1) # c(0.01, 0.99)

## get the length bin limits for haul and species
sp_len_bins <- len_all_dt[, .("lower" = round(quantile(LENGTH.CM, na.rm = TRUE, prob = probs[1])), 
                              "upper" = round(quantile(LENGTH.CM, na.rm = TRUE, prob = probs[2]))), 
                          by = .(SPECIES_NAME, SPECIES_CODE, HAUL)]

## expand limits to all possible 1cm length bins for haul and species
foo2 <- sp_len_bins[, .("LENGTH.CM" = lower:upper), 
                    by = .(SPECIES_NAME, SPECIES_CODE, HAUL)]

## merge them
fii <- merge(foo1, foo2, all = TRUE, allow.cartesian = TRUE)

## remove some columns
fii[, c("RF", "N.AT.LENGTH", "N.RAISED.HAUL", "Q") := NA]

## merge in the additional missing length bins
foo_dt <- merge(len_all_dt, fii, all.y = TRUE, 
                by = c("HAUL", "CONDITION", "LIGHTS", "CODEND", 
                       "SPECIES_NAME", "SPECIES_CODE", "LENGTH.CM"))

## remove and rename unnecessary columns
foo_dt[, c("RF.y", "N.AT.LENGTH.y", "N.RAISED.HAUL.y", "Q.y") := NULL] 
colnames(foo_dt) <- gsub("\\.x", "", colnames(foo_dt))

## set NAs in N.AT.LENGTH and N.RAISED.HAUL to 0
setnafill(foo_dt, fill = 0, 
          cols = c("N.AT.LENGTH", "N.RAISED.HAUL"))

## set NAs in RF and Q to 1
setnafill(foo_dt, fill = 1, 
          cols = c("RF", "Q"))

## check worked for examples above
if (0) { # set to 1 to run
  
  ### only LIGHTS == "YES" for some hauls
  subset(foo_dt, SPECIES == "CTC" & HAUL == 25)$LIGHTS
  
  ### and only LIGHTS == "NO" for other hauls
  subset(foo_dt, SPECIES == "HAD" & HAUL == 2)$LIGHTS
  
}

## check worked for examples above
if (0) { # set to 1 to run
  
  ### how many zeros are there in the new dataset?
  zeros_tab <- table(foo_dt$N.AT.LENGTH, useNA = "ifany")
  
  ### how many zeros are there in the old dataset?
  nozeros_tab <- table(foo_dt$N.AT.LENGTH, useNA = "ifany")
  
  ### combine the datasets
  fii <- rbindlist(list("no zeros" = len_all_dt, "zeros" = foo_dt), 
                   use.names = TRUE, idcol = "dataset")
  
  ### plot bars
  pp <- ggplot(fii, aes(x = N.AT.LENGTH, fill = dataset)) + 
    geom_bar(alpha = 0.4, position = position_dodge(0)) + 
    facet_wrap(~ HAUL) + 
    sgg()
  
}

## rename
len_all_dt <- foo_dt

## check: haddock numbers
if (do_chks) {
  had <- sum(subset(len_all_dt, SPECIES_CODE == "HAD")[, "N.RAISED.HAUL"])
  had_30_40 <- sum(subset(len_all_dt, SPECIES_CODE == "HAD" & 
                            LENGTH.CM >= "30" & LENGTH.CM <= "40" )[, "N.RAISED.HAUL"])
  had_30_40 / had
}

# save data ---------------------------------------------------------------

## save out data
if (save_out) {
  nm <- paste0(prj_nm, "_data.RData")
  save(len_all_dt, file = here("data", nm))
}

