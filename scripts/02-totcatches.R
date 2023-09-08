# total catches script ----------------------------------------------------

## a script `sourced` by the .Rmd to run the total catch analysis.


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

##! CHANGE LOG: (20221017 07:11:56) Commented lines 66-84 that expands data to all size classes - done in 00-orgdata.R


# additional libraries ----------------------------------------------------


# additional setup --------------------------------------------------------


# read in data ------------------------------------------------------------

## read in processed data
nm <- paste0(prj_nm, "_data.RData")
if (file.exists(here("data", nm))) {
  load(here("data", nm))
} else {
  stop("please run 00-orgdata.R before this script")
}


# data preparation --------------------------------------------------------

## Data prepared by:
### (1) limiting to species of interest
### (2) calculating total numbers caught
### (3) weighted mean of sub-sampling ratio (expressed as a fraction)
len_sub_dt <- len_all_dt[SPECIES_NAME %in% names(tot_sp_cd), # (1)
                         .(TOTAL_N = sum(N.AT.LENGTH), # (2)
                           Q = 1 / weighted.mean(RF, N.AT.LENGTH)), # (3)
                         by = .(SPECIES_NAME,
                                HAUL,
                                LENGTH.CM,
                                CONDITION,
                                LIGHTS)]

## fix Q
setnafill(len_sub_dt, nan = NA, cols = "Q", fill = 1)


# check: are catches sufficient for analysis? -----------------------------

## set inclusion limit: % hauls that catch 1 fish of any size
incl_limit <- 0.6

## calculate test statistic
incl_test <- len_sub_dt[, 
                        sum(TOTAL_N == 1) / .N,
                        by = SPECIES_NAME]

## test: if fail test, then omit from species from analysis
test_res <- incl_test[which(V1 >= incl_limit), SPECIES_NAME]
if (length(test_res) > 0) {
  if (show_msgs) {
    msg <- paste0("\tMessage: omitting ", test_res, " from totCatches analysis because they are too few in catches.\n")
    message(msg)
  }
  len_sub_dt <- subset(len_sub_dt, !SPECIES_NAME %in% test_res)
}


# model data preparations -------------------------------------------------

## fix factors
len_sub_dt$SPECIES_NAME <- factor(len_sub_dt$SPECIES_NAME)
len_sub_dt$CONDITION <- factor(len_sub_dt$CONDITION)
len_sub_dt$LIGHTS <- factor(len_sub_dt$LIGHTS)


# holding objects ---------------------------------------------------------

## holding objects for saturated models (m0)
m0_lst <- vector("list", nlevels(len_sub_dt$SPECIES_NAME))
names(m0_lst) <- levels(len_sub_dt$SPECIES_NAME)

## holding objects for diagnostic statistics (a)
a_lst <- vector("list", nlevels(len_sub_dt$SPECIES_NAME))
names(a_lst) <- levels(len_sub_dt$SPECIES_NAME)

## holding objects for data (d)
d_lst <- vector("list", nlevels(len_sub_dt$SPECIES_NAME))
names(d_lst) <- levels(len_sub_dt$SPECIES_NAME)

## holding objects for plots (p)
p_lst <- vector("list", nlevels(len_sub_dt$SPECIES_NAME))
names(p_lst) <- levels(len_sub_dt$SPECIES_NAME)

## holding objects for revision plots (p)
r1_lst <- vector("list", nlevels(len_sub_dt$SPECIES_NAME))
names(r1_lst) <- levels(len_sub_dt$SPECIES_NAME)

## holding objects for model simplifications (ms)
ms_lst <- vector("list", nlevels(len_sub_dt$SPECIES_NAME))
names(ms_lst) <- names(m0_lst)

## holding objects for model predictions (h)
h_lst <- vector("list", nlevels(len_sub_dt$SPECIES_NAME))
names(h_lst) <- levels(len_sub_dt$SPECIES_NAME)


# model fitting -----------------------------------------------------------

## start i loop
for (i in names(m0_lst)) {
  
  ## message
  if (show_msgs) message(paste0("\tmodel fitting for ", i))
  
  ## get data
  d <- subset(len_sub_dt, SPECIES_NAME == i)
  
  ## fix data
  d$SPECIES_NAME <- factor(d$SPECIES_NAME)
  d$HAUL <- factor(d$HAUL)
  d$LIGHTS <- factor(d$LIGHTS, levels = c("NO", "YES"))
  d$CONDITION <- factor(d$CONDITION, levels = c("Day", "Night"))
  d$EPS_H_L <- factor(d$TOTAL_N)
  
  ## add interaction between lights and condition
  d[, LC := interaction(LIGHTS, CONDITION)]
  
  # ## parallel setup
  # cl <- makeCluster(nc)
  
  ## fit saturated model
  m0 <- bam(
    formula = TOTAL_N ~ # response variable
      LIGHTS + CONDITION + LC + # linear fixed effects
      s(LENGTH.CM, by = LIGHTS, m = 1) + s(LENGTH.CM, by = CONDITION, m = 1) + s(LENGTH.CM, by = LC, m = 1) + # non-linear fixed effects
      s(LENGTH.CM, HAUL, bs = "fs", m = 1) + # non-linear random effect
      s(EPS_H_L, bs = "re") + # overdispersion parameter
      offset(log(Q)), # sub-sampling fraction
    family = poisson(),
    data = d,
    method = "ML") #, chunk.size = (10000 / nc), cluster = cl)
  
  # ## stop cluster
  # stopCluster(cl)
  
  ## add to holding objects
  m0_lst[[i]] <- m0 # saturated model
  d_lst[[i]] <- d # data
  
  ## end i loop
}


# model simplification ----------------------------------------------------

## list of simplified models
simplified_models <- c(
  "saturated",
  "no interaction",
  "no lights",
  "no condition",
  "no length",
  "no length or lights",
  "no length or condition"
)

## start i loop
for (i in names(ms_lst)) {
  
  ## message
  if (show_msgs) message(paste0("\tmodel simplification for ", i))
  
  ## grab the data
  d <- d_lst[[i]]
  
  ## grab the saturated model
  m0 <- m0_lst[[i]]
  
  ## create empty holding list for simplified models
  s_lst <- vector("list", length(simplified_models))
  names(s_lst) <- simplified_models
  
  ## saturated model: no simplification
  s_lst[["saturated"]] <- m0
  
  # ## parallel setup
  # cl <- makeCluster(nc)
  
  ## fit simplified model: no interaction
  m <- update(m0, 
              . ~ . 
              - LC
              - s(LENGTH.CM, by = LC, m = 1))
  s_lst[["no interaction"]] <- m
  rm(m)
  gc()
  
  ## fit simplified model: no lights
  m <- update(m0, 
              . ~ . 
              - LIGHTS
              - LC
              - s(LENGTH.CM, by = LIGHTS, m = 1)
              - s(LENGTH.CM, by = LC, m = 1))
  s_lst[["no lights"]] <- m
  rm(m)
  gc()
  
  ## fit simplified model: no condition
  m <- update(m0, 
              . ~ . 
              - CONDITION
              - LC
              - s(LENGTH.CM, by = CONDITION, m = 1)
              - s(LENGTH.CM, by = LC, m = 1))
  s_lst[["no condition"]] <- m
  rm(m)
  gc()
  
  ## fit simplified model: no length
  m <- update(m0, 
              . ~ . 
              - s(LENGTH.CM, by = LIGHTS, m = 1)
              - s(LENGTH.CM, by = CONDITION, m = 1)
              - s(LENGTH.CM, by = LC, m = 1)
              - s(LENGTH.CM, HAUL, bs = "fs", m = 1))
  s_lst[["no length"]] <- m
  rm(m)
  gc()
  
  ## fit simplified model: no length or lights
  m <- update(m0, 
              . ~ . 
              - LIGHTS
              - LC
              - s(LENGTH.CM, by = LIGHTS, m = 1)
              - s(LENGTH.CM, by = CONDITION, m = 1)
              - s(LENGTH.CM, by = LC, m = 1)
              - s(LENGTH.CM, HAUL, bs = "fs", m = 1))
  s_lst[["no length or lights"]] <- m
  rm(m)
  gc()
  
  ## fit simplified model: no length or condition
  m <- update(m0, 
              . ~ . 
              - CONDITION
              - LC
              - s(LENGTH.CM, by = LIGHTS, m = 1)
              - s(LENGTH.CM, by = CONDITION, m = 1)
              - s(LENGTH.CM, by = LC, m = 1)
              - s(LENGTH.CM, HAUL, bs = "fs", m = 1))
  s_lst[["no length or condition"]] <- m
  rm(m)
  gc()
  
  # ## stop cluster
  # stopCluster(cl)
  
  ## compare simplified models
  ll <- sapply(s_lst, function(v) as.numeric(logLik(v)))
  k <- sapply(s_lst, function(v) attributes(logLik(v))$df)
  aicc_tab <- aictabCustom(logL = ll, K = k, 
                           modnames = names(s_lst), nobs = nrow(d))
  bic_tab <- bictabCustom(logL = ll, K = k, 
                          modnames = names(s_lst), nobs = nrow(d))
  
  ## write out model simplification table
  nm <- paste0("total_catches_mod_simp_aicctab_", gsub(" ", "_", i), ".csv")
  write.csv(aicc_tab, file = here("results", nm), row.names = FALSE)
  nm <- paste0("total_catches_mod_simp_bictab_", gsub(" ", "_", i), ".csv")
  write.csv(bic_tab, file = here("results", nm), row.names = FALSE)
  
  ## identify best model based on comp_stat
  if (comp_stat == "AICc") {
    best_model <- which(names(s_lst) == aicc_tab$Modnames[1])
  } else {
    best_model <- which(names(s_lst) == bic_tab$Modnames[1])
  }
  
  ## add simplified models, aicc and bic tables and best model to holding vector
  ms_lst[[i]] <- list("simplified models" = s_lst,
                      "AICc table" = aicc_tab,
                      "BIC table" = bic_tab,
                      "best model" = best_model)
  
  ## get diagnostic statistics
  a <- appraise(s_lst[[best_model]])
  
  ## add to holding objects
  a_lst[[i]] <- a # diagnostic statistics 
  
  ## end i loop
}


# model fit ---------------------------------------------------------------

## start i loop
for (i in names(ms_lst)) {
  
  ## message
  if (show_msgs) message(paste0("\tmodel inference for ", i))
  
  ## grab the data
  d <- d_lst[[i]]
  
  ## grab the best model
  best_model <- ms_lst[[i]]$`best model`
  m <- ms_lst[[i]]$`simplified models`[[best_model]]
  
  ## create prediction data
  n <- d

  # ## parallel setup
  # cl <- makeCluster(nc)

  ## add predictions to data
  # h <- predict(object = m, newdata = n, type = "response", se.fit = TRUE, 
  #              cluster = cl)
  h <- predict(object = m, newdata = n, type = "response", se.fit = TRUE)
  
  # ## stop cluster
  # stopCluster(cl)
  
  ## add predictions to prediction data
  n <- data.table(transform(n,
                            fit = h$fit,
                            fitse = h$se.fit))
  
  ## modified data for plot
  d[, c("Lights", "Time of day") := list(factor(LIGHTS, 
                                                levels = c("NO", "YES"), 
                                                labels = c("Absent", "Present")),
                                         CONDITION)]
  n[, c("Lights", "Time of day") := list(factor(LIGHTS, 
                                                levels = c("NO", "YES"), 
                                                labels = c("Absent", "Present")),
                                         CONDITION)]
  
  ## plot dataset
  aa <- rbindlist(list(d, n), fill = TRUE, id = "Type")
  aa[, Type := factor(Type, levels = 1:2, labels = c("Actual", "Fitted"))]
  
  ## plot
  p <- ggplot() +
    stat_summary(data = subset(aa, Type == "Fitted"),
                 aes(x = LENGTH.CM, y = fit / Q, fill = Lights),
                 alpha = 0.2,
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
                 alpha = 0.5,
                 geom = "point") +
    stat_summary(data = subset(aa, Type == "Actual"),
                 aes(x = LENGTH.CM, y = TOTAL_N / Q, colour = Lights), 
                 fun.data = mean_se,
                 alpha = 0.5,
                 geom = "linerange") +
    facet_wrap(~ SPECIES_NAME + `Time of day`) +
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
    nm <- paste0("total_catches_", i, "_bestmod.pdf")
    nm <- gsub("\\.pdf", "_R1\\.pdf", nm) # revision
    cairo_pdf(here("plots", nm), 
              width = 10, height = 7)
    print(p)
    dev.off()
    nm <- paste0("total_catches_", i, "_bestmod.jpg")
    nm <- gsub("\\.jpg", "_R1\\.jpg", nm) # revision
    jpeg(here("plots", nm), 
         res = 300, width = (480 * 8), height = (480 * 5))
    print(p)
    dev.off()
  }
  
  ## add to holding objects
  d_lst[[i]] <- d # data
  h_lst[[i]] <- n # predictions
  p_lst[[i]] <- p # plot
  
  ## end i loop
}


# model inference ---------------------------------------------------------

## get the best model number
best_mods <- unlist(lapply(ms_lst, "[[", "best model"))

## create holding list for best models
best_mods_lst <- vector("list", length = length(best_mods))
names(best_mods_lst) <- names(best_mods)

## get the best models
for (i in 1:length(best_mods)) {
  best_mods_lst[[i]] <- ms_lst[[i]]$`simplified models`[[best_mods[i]]]
}

## create holding list for marginal effect fits
best_inf_lst <- vector("list", length = length(best_mods))
names(best_inf_lst) <- names(best_mods)

## get the marginal effect fits
em_lst <- vector("list", length(best_mods_lst))
for (i in 1:length(best_mods_lst)) {
  m <- best_mods_lst[[i]]
  d <- d_lst[[i]]
  specs <- na.omit(exp_trts[match(labels(terms(m)), exp_trts)])
  em_lst[[i]] <- emmeans(object = m, 
                         data = d, 
                         specs = specs,
                         type = "response", 
                         at = list(LENGTH.CM = seq(min(d$LENGTH.CM), 
                                                   max(d$LENGTH.CM))))
  best_inf_lst[[i]] <- as.data.table(confint(contrast(em_lst[[i]], 
                                                      simple = specs[1], 
                                                      name = specs[1])))
}

## combine
best_inf <- rbindlist(best_inf_lst, idcol = "Species", fill = TRUE)

## emmeans produces a warning about nesting - see why with code
if (do_chks) {
  with(d_lst$Haddock, table(HAUL, CONDITION))
}

## refactorise
best_inf$Species <- factor(best_inf$Species, 
                           levels = names(tot_sp_cd))
best_inf$Lights <- factor(gsub(" effect", "", best_inf$LIGHTS), 
                          levels = c("NO", "YES", "No effect"), 
                          labels = c("Absent", "Present", "No effect"))
best_inf$Lights[which(is.na(best_inf$LIGHTS))] <- "No effect"
best_inf$`Time of day` <- factor(gsub(" effect", "", best_inf$CONDITION),
                                 levels = c("Day", "Night", "Both"), 
                                 labels = c("Day", "Night", "Both"))
best_inf$`Time of day`[which(is.na(best_inf$`Time of day`))] <- "Both"

## make plot
total_catch_emmeans_p <- ggplot(best_inf, 
                                aes(x = `Time of day`, y = ratio, colour = Lights)) + 
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey") +
  geom_point(size = 3, position = position_dodge(width = 0.35)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.5, position = position_dodge(width = 0.35), linewidth = 1) + 
  facet_wrap(~ Species) +
  xlab("Time of day") +
  ylab("Estimated effect relative\n to overall marginal mean") +
  scale_color_manual(values = c("#F8766D", "#00BFC4", "gray50")) +
  sgg()

## save plot
if (make_plots) {
  nm <- "total_catches_allspecies_emmeans.pdf"
  nm <- gsub("\\.pdf", "_R1\\.pdf", nm) # revision
  cairo_pdf(here("plots", nm), 
            width = 10, height = 7)
  print(total_catch_emmeans_p)
  dev.off()
  nm <- "total_catches_allspecies_emmeans.jpg"
  nm <- gsub("\\.jpg", "_R1\\.jpg", nm) # revision
  jpeg(here("plots", nm), 
       res = 300, width = 480 * 7, height = 480 * 5)
  print(total_catch_emmeans_p)
  dev.off()
}


# save results ------------------------------------------------------------

## if saving
if (save_out) {
  
  ## message
  if (show_msgs) message("\tsaving...")
  
  ## get list of holding objects and plots
  save_lst <- ls(pattern = "_lst$|_p$")
  
  ## results name
  save_nm <- paste0("total_catches_results_", comp_stat, "_R1.RData")
  
  ## save them
  save(list = save_lst, file = here("results", save_nm))
  
}
