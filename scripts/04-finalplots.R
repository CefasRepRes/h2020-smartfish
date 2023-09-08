# final plot preparations -------------------------------------------------

## a script `sourced` by the .Rmd to make the final plots.


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


# additional libraries ----------------------------------------------------


# additional setup --------------------------------------------------------


# total catches -----------------------------------------------------------

## load the data
nm <- paste0("total_catches_results_", comp_stat, "_R1.RData")
load(here("results", nm))

## total catch plots ------------------------------------------------------

## plot data
dd <- rbindlist(d_lst)
hh <- rbindlist(h_lst)

## ensure correct plot order
dd$SPECIES_NAME <- factor(dd$SPECIES_NAME, levels = names(tot_sp_cd))
hh$SPECIES_NAME <- factor(hh$SPECIES_NAME, levels = names(tot_sp_cd))

## plot dataset
aa <- rbindlist(list(dd, hh), fill = TRUE, id = "Type")
aa[, Type := factor(Type, levels = 1:2, labels = c("Actual", "Fitted"))]

## plot fits
total_catches_fit_p <- ggplot() +
  stat_summary(data = subset(aa, Type == "Fitted"),
               aes(x = LENGTH.CM, y = fit / Q, fill = Lights),
               alpha = 0.3,
               fun.data = mean_se,
               geom = "ribbon") +
  stat_summary(data = subset(aa, Type == "Fitted"),
               aes(x = LENGTH.CM, y = fit / Q, colour = Lights, 
                   linetype = "Fitted"),
               size = 1,
               fun = mean,
               geom = "line") +
  stat_summary(data = subset(aa, Type == "Fitted"),
               aes(x = LENGTH.CM, y = fit / Q, colour = Lights, 
                   shape = Type, linetype = "Actual"),
               size = 1,
               fun = mean,
               geom = "line") +
  stat_summary(data = subset(aa, Type == "Actual"),
               aes(x = LENGTH.CM, y = TOTAL_N / Q, colour = Lights, 
                   shape = Type, linetype = "Actual"), 
               fun = mean,
               size = 2,
               geom = "point") +
  stat_summary(data = subset(aa, Type == "Actual"),
               aes(x = LENGTH.CM, y = TOTAL_N / Q, colour = Lights), 
               fun.data = mean_se,
               alpha = 0.5,
               geom = "linerange") +
  facet_wrap(~ SPECIES_NAME + `Time of day`,
             scales = "free", ncol = 2) +
  guides(shape = guide_legend(override.aes = list(shape = c(16, NA),
                                                  linetype = c("blank", "solid"))),
         colour = guide_legend(override.aes = list(shape = c(NA, NA),
                                                   linetype = c("blank", "blank"))),
         linetype = guide_legend(override.aes = list(shape = c(16, NA),
                                                     linetype = c("blank", "solid")))) +
  labs(x = "Length (cm)", y = "Average number in haul", linetype = "Type", shape = "Type") +
  sgg()

## save plot
if (make_plots) {
  nm <- "total_catches_allspecies_bestmod.pdf"
  nm <- gsub("\\.pdf", "_R1\\.pdf", nm) # revision
  cairo_pdf(here("plots", nm), 
            width = 10, height = 7)
  print(total_catches_fit_p)
  dev.off()
  nm <- "total_catches_allspecies_bestmod.jpg"
  nm <- gsub("\\.jpg", "_R1\\.jpg", nm) # revision
  jpeg(here("plots", nm), 
       res = 300, width = 480 * 7, height = 480 * 5)
  print(total_catches_fit_p)
  dev.off()
}


## total catch table ------------------------------------------------------

## produce combined AICc table for all species
aictab_allsps <- rbindlist(lapply(lapply(ms_lst, `[[`, "AICc table"), 
                                  data.table), 
                           idcol = "Species")

## produce combined BIC table for all species
bictab_allsps <- rbindlist(lapply(lapply(ms_lst, `[[`, "BIC table"), 
                                  data.table), 
                           idcol = "Species")

## change "condition" to  "time of day"
aictab_allsps$Modnames <- gsub("condition", "time of day", aictab_allsps$Modnames)
bictab_allsps$Modnames <- gsub("condition", "time of day", bictab_allsps$Modnames)

## write out model simplification table
nm <- paste0("total_catches_mod_simp_aicctab_allsps.csv")
write.csv(aictab_allsps, file = here("results", nm), row.names = FALSE)
nm <- paste0("total_catches_mod_simp_bictab_allsps.csv")
write.csv(bictab_allsps, file = here("results", nm), row.names = FALSE)


# catch split -------------------------------------------------------------

## load the data
nm <- paste0("prop_upper_results_", comp_stat, "_R1.RData")
load(here("results", nm))

## catch split plots ------------------------------------------------------

## plot data
dd <- rbindlist(d_lst)
hh <- rbindlist(h_lst)

## ensure correct plot order
dd$SPECIES_NAME <- factor(dd$SPECIES_NAME, levels = names(spl_sp_cd))
hh$SPECIES_NAME <- factor(hh$SPECIES_NAME, levels = names(spl_sp_cd))

## plot dataset
aa <- rbindlist(list(dd, hh), fill = TRUE, id = "Type")
aa[, Type := factor(Type, levels = 1:2, labels = c("Actual", "Fitted"))]

## plot fits
prop_upper_fit_p <- ggplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  stat_summary(data = subset(aa, Type == "Fitted"),
               aes(x = LENGTH.CM, y = fit, fill = Lights),
               alpha = 0.3,
               fun.data = mean_se,
               geom = "ribbon") +
  stat_summary(data = subset(aa, Type == "Fitted"),
               aes(x = LENGTH.CM, y = fit, colour = Lights, 
                   linetype = "Fitted"),
               size = 1,
               fun = mean,
               geom = "line") +
  stat_summary(data = subset(aa, Type == "Fitted"),
               aes(x = LENGTH.CM, y = fit, colour = Lights, 
                   shape = Type, linetype = "Actual"),
               size = 1,
               fun = mean,
               geom = "line") +
  stat_summary(data = subset(aa, Type == "Actual"),
               aes(x = LENGTH.CM, y = UPPER_N / TOTAL_N, colour = Lights, 
                   shape = Type, linetype = "Actual"), 
               fun = mean,
               size = 2,
               geom = "point") +
  stat_summary(data = subset(aa, Type == "Actual"),
               aes(x = LENGTH.CM, y = UPPER_N / TOTAL_N, colour = Lights), 
               fun.data = mean_se,
               alpha = 0.5,
               geom = "linerange") +
  facet_wrap(~ SPECIES_NAME + `Time of day`, 
             scales = "free", ncol = 2) +
  guides(shape = guide_legend(override.aes = list(shape = c(16, NA),
                                                  linetype = c("blank", "solid"))),
         colour = guide_legend(override.aes = list(shape = c(NA, NA),
                                                   linetype = c("blank", "blank"))),
         linetype = guide_legend(override.aes = list(shape = c(16, NA),
                                                     linetype = c("blank", "solid")))) +
  labs(x = "Length (cm)", y = "Average number in haul", linetype = "Type", shape = "Type") +
  sgg()

## save plot
if (make_plots) {
  nm <- "prop_upper_allspecies_bestmod.pdf"
  nm <- gsub("\\.pdf", "_R1\\.pdf", nm) # revision
  cairo_pdf(here("plots", nm), 
            width = 10, height = 7)
  print(prop_upper_fit_p)
  dev.off()
  nm <- "prop_upper_allspecies_bestmod.jpg"
  nm <- gsub("\\.jpg", "_R1\\.jpg", nm) # revision
  jpeg(here("plots", nm), 
       res = 300, width = (480 * 7), height = (480 * 5))
  print(prop_upper_fit_p)
  dev.off()
}


## catch split table ------------------------------------------------------

## produce combined AICc table for all species
aictab_allsps <- rbindlist(lapply(lapply(ms_lst, `[[`, "AICc table"), 
                                  data.table), 
                           idcol = "Species")

## produce combined BIC table for all species
bictab_allsps <- rbindlist(lapply(lapply(ms_lst, `[[`, "BIC table"), 
                                  data.table), 
                           idcol = "Species")

## change "condition" to  "time of day"
aictab_allsps$Modnames <- gsub("condition", "time of day", aictab_allsps$Modnames)
bictab_allsps$Modnames <- gsub("condition", "time of day", bictab_allsps$Modnames)

## write out model simplification table
nm <- paste0("prop_upper_mod_simp_aicctab_allsps.csv")
write.csv(aictab_allsps, file = here("results", nm), row.names = FALSE)
nm <- paste0("prop_upper_mod_simp_bictab_allsps.csv")
write.csv(bictab_allsps, file = here("results", nm), row.names = FALSE)


# final data for MDR ------------------------------------------------------

## load data
load(here("data", "h2020-smartfish_data.RData"))

## final data to csv for MDR
write.csv(len_all_dt, 
          file = here("data", "h2020-smartfish-analysis-data.csv"),
          row.names = FALSE)

