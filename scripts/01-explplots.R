# exploratory plots script ------------------------------------------------

## a script `sourced` by the .Rmd to run the first analysis.


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


# read in data ------------------------------------------------------------

## read in processed data
nm <- paste0(prj_nm, "_data.RData")
if (file.exists(here("data", nm))) {
  load(here("data", nm))
} else {
  stop("please run 00-orgdata.R before this script")
}


# tidy data for plotting --------------------------------------------------

## modified data for plot
len_all_dt[, c("Lights", "Time of day") := list(factor(LIGHTS,
                                                       levels = c("NO", "YES"), 
                                                       labels = c("Absent", "Present")),
                                                CONDITION)]


# basic plots -------------------------------------------------------------

## number of hauls per lights and condition
hauls_cond_pie <- ggplot(len_all_dt[SPECIES_NAME %in% names(sp_cd), 
                                    .("nHAULS" = length(unique(HAUL))), by = .(`Time of day`)], 
                         aes(x = "", y = nHAULS, fill = `Time of day`)) + 
  geom_bar(stat = "identity") + 
  coord_polar("y") + 
  geom_text(aes(label = nHAULS), 
            family = "Times New Roman", size = 16 / 2,
            position = position_stack(vjust = 0.5)) +
  theme_void(base_size = 16, base_family = "Times New Roman") +
  labs(title = "Number of hauls per time of day")
if (make_plots) {
  cairo_pdf(here("plots", "expl_hauls_cond_pie.pdf"), 
            width = 7, height = 7)
  print(hauls_cond_pie)
  dev.off()
  jpeg(here("plots", "expl_hauls_cond_pie.jpg"), 
       res = 300, width = (480 * 5), height = (480 * 5))
  print(hauls_cond_pie)
  dev.off()
}

## length histograms by species
len_sps_hist <- ggplot(len_all_dt[SPECIES_NAME %in% names(sp_cd), ], 
                       aes(x = LENGTH.CM, y = N.RAISED.HAUL)) + 
  geom_col() + 
  facet_wrap(~ SPECIES_NAME, scales = "free") +
  labs(x = "Length class (cm)",
       y = "Frequency") +
  sgg()
if (make_plots) {
  cairo_pdf(here("plots", "expl_len_sps_hist.pdf"), 
            width = 10, height = 7)
  print(len_sps_hist)
  dev.off()
  jpeg(here("plots", "expl_len_sps_hist.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 5))
  print(len_sps_hist)
  dev.off()
}


# table 1 -----------------------------------------------------------------

## read data
trip_dat <- read_xlsx(here("data", len_nm), sheet = "haul.gear", range = "A2:W30")

## convert to data.table
trip_dt <- data.table(trip_dat)

## improve LIGHTS RIG (port/starboard) labels
trip_dt[, `light rig` := factor(`LIGHTS RIG (port/starboard)`, 
                                levels = c("S", "P"),
                                labels = c("Starboard", "Port"))]

## improve Condition labels
trip_dt[, `time of day` := factor(`Light conditions (Day/Night/Mix)`, 
                                  levels = c("N", "D"),
                                  labels = c("Night", "Day"))]

## convert times
trip_dt[, Shot.Time := as.POSIXct(paste(Shot.Date, 
                                        gsub("[0-9]{4}-[0-9]{2}-[0-9]{2} ", "", Shot.Time)))]
trip_dt[, Haul.Time := as.POSIXct(paste(Haul.Date, 
                                        gsub("[0-9]{4}-[0-9]{2}-[0-9]{2} ", "", Haul.Time)))]

## time difference in hours
trip_dt[, Time.diff := Haul.Time - Shot.Time]

## collect species data
foo <- dcast(data = len_all_dt[, .(N = sum(N.RAISED.HAUL), Q = mean(Q)), 
                               by = .(Lights, HAUL, CONDITION, SPECIES_NAME)], 
             value.var = list("N", "Q"), formula = Lights + HAUL + CONDITION ~ SPECIES_NAME)

## combine with species data
trip_dt <- merge(foo, trip_dt, by = "HAUL")

## make the table
trip_sum <- trip_dt[`LIGHTS POSITION (headline/footrope)` == "H",
                    .(`n haul` = .N,
                      `mean depth` = mean(Haul.Depth),
                      `sd depth` = sd(Haul.Depth),
                      `mean time` = mean(Time.diff),
                      `sd time` = sd(Time.diff),
                      `total time` = sum(Time.diff)),
                    # by = .(`light rig`, `time of day`)]
                    by = .(`time of day`)]

## save table
write.csv(trip_sum, file = here("results", "haul-summary-stats.csv"), 
          row.names = FALSE)

## additional table
trip_sum_exp <- trip_dt[, .(`Haul` = rep(1:(nrow(trip_dt) / 2), each = 2),
                            "Time of day" = `time of day`,
                            "Lights" = Lights,
                            "Shot time" = format(Shot.Time, "%Y-%m-%d %H:%M"),
                            "Haul duration (h)" = as.numeric(Time.diff),
                            "Depth (m)" = Haul.Depth,
                            `N_Haddock`, `Q_Haddock`, 
                            `N_Grey gurnard`, `Q_Grey gurnard`,
                            `N_Megrim`, `Q_Megrim`,
                            `N_Whiting`, `Q_Whiting`, 
                            `N_Northern squid`, `Q_Northern squid`)]

## save table
write.csv(trip_sum_exp, file = here("results", "haul-stats.csv"), 
          row.names = FALSE)


# effect plots ------------------------------------------------------------

## mean (and bootstrap CIs) of catch by CONDITION when LIGHTS on or off
catch_sps_effects_bCI <- ggplot(len_all_dt[SPECIES_NAME %in% names(sp_cd), ], 
                                aes(x = `Time of day`, y = N.RAISED.HAUL, 
                                    colour = Lights)) + 
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6), size = 3) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.6), size = 1.25) +
  facet_wrap(~ SPECIES_NAME) +
  labs(x = "Time of day",
       y = "Mean number caught per haul") +
  sgg()
if (make_plots) {
  cairo_pdf(here("plots", "expl_catch_sps_effects_bCI.pdf"), 
            width = 10, height = 7)
  print(catch_sps_effects_bCI)
  dev.off()
  jpeg(here("plots", "expl_catch_sps_effects_bCI.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 5))
  print(catch_sps_effects_bCI)
  dev.off()
}

## boxplot of catch by CONDITION when LIGHTS on or off
foo <- len_all_dt[SPECIES_NAME %in% names(sp_cd), 
                  .("sum.raised.n.haul" = sum(N.RAISED.HAUL)),
                  by = .(`Time of day`, Lights, SPECIES_NAME, HAUL)]
# catch_sps_effects_box <- ggplot(len_all_dt[SPECIES_NAME %in% names(sp_cd), ], 
#                                 aes(x = `Time of day`, y = N.RAISED.HAUL, 
#                                     colour = Lights)) + 
catch_sps_effects_box <- ggplot(foo, 
                                aes(x = `Time of day`, y = sum.raised.n.haul, 
                                    colour = Lights)) + 
  geom_boxplot() +
  facet_wrap(~ SPECIES_NAME, scales = "free") +
  labs(x = "Time of day",
       y = "Number caught per haul") +
  # y = expression(sqrt("Number caught per haul"))) +
  # scale_y_sqrt() +
  sgg()
if (make_plots) {
  cairo_pdf(here("plots", "expl_catch_sps_effects_box.pdf"), 
            width = 10, height = 7)
  print(catch_sps_effects_box)
  dev.off()
  jpeg(here("plots", "expl_catch_sps_effects_box.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 5))
  print(catch_sps_effects_box)
  dev.off()
}

## proportion of total catch in upper codend by SPECIES

## After discussion (MD & SG), we agreed that we shouldn"t be plotting raw numbers but rather numbers raised and plots that don"t use numbers raised shouldn"t be used. This could be modified to use "N.RAISED.HAUL", but as it"s not need for the ms, we"re commenting out...

# foo <- len_all_dt[SPECIES_NAME %in% names(sp_cd),
#                   .(UPPER_N = sum(N.AT.LENGTH * as.numeric(CODEND == "Upper")),
#                     TOTAL_N = sum(N.AT.LENGTH)), 
#                   by = .(SPECIES_NAME,
#                          LENGTH.CM,
#                          `Time of day`,
#                          Lights)]
# uprop_sps_effects <- ggplot(foo, 
#                             aes(x = LENGTH.CM, y = UPPER_N / TOTAL_N, 
#                                 group = Lights, colour = Lights)) + 
#   geom_point() + 
#   geom_line() +
#   facet_wrap(~ SPECIES_NAME + `Time of day`, scales = "free_x") +
#   labs(x = "Length (cm)",
#        y = "Proportion in upper codend") +
#   sgg()
# if (make_plots) {
#   cairo_pdf(here("plots", "expl_uprop_sps_effects.pdf"), 
#             width = 10, height = 7)
#   print(uprop_sps_effects)
#   dev.off()
#   jpeg(here("plots", "expl_uprop_sps_effects.jpg"), 
#        res = 300, width = (480 * 7), height = (480 * 5))
#   print(uprop_sps_effects)
#   dev.off()
# }


# overdispersion plot -----------------------------------------------------

## mean (and bootstrap CIs) of catch per haul (all lengths combined)
catch_sps_overdisp_bCI <- ggplot(len_all_dt[SPECIES_NAME %in% names(sp_cd), ],
                                 aes(x = HAUL, y = N.RAISED.HAUL)) +
  stat_summary(fun = mean, geom = "point",
               position = position_dodge(width = 0.6), size = 3) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.6), size = 1.25) +
  facet_wrap(~ SPECIES_NAME, scales = "free") +
  labs(x = "Haul number",
       y = "Mean number caught per haul") +
  sgg()
if (make_plots) {
  cairo_pdf(here("plots", "expl_catch_overdisp_bCI.pdf"),
            width = 10, height = 7)
  print(catch_sps_overdisp_bCI)
  dev.off()
  jpeg(here("plots", "expl_catch_overdisp_bCI.jpg"),
       res = 300, width = (480 * 7), height = (480 * 5))
  print(catch_sps_overdisp_bCI)
  dev.off()
}

## mean (and bootstrap CIs) of catch per haul (all lengths combined)
catch_sps_lengths_bCI <- ggplot(len_all_dt[SPECIES_NAME %in% names(sp_cd), ], 
                                aes(x = LENGTH.CM, y = N.RAISED.HAUL)) + 
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(width = 0.6), size = 3) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2,
               position = position_dodge(width = 0.6), size = 1.25) +
  facet_wrap(~ SPECIES_NAME, scales = "free") +
  labs(x = "Haul number",
       y = "Mean number caught per haul") +
  sgg()
if (make_plots) {
  cairo_pdf(here("plots", "expl_catch_lengths_bCI.pdf"), 
            width = 10, height = 7)
  print(catch_sps_lengths_bCI)
  dev.off()
  jpeg(here("plots", "expl_catch_lengths_bCI.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 5))
  print(catch_sps_lengths_bCI)
  dev.off()
}


# revision plots ----------------------------------------------------------

# new figure 2b
d <- data.table(read_xlsx(here("data", "light-measures.xlsx"), 
                          range = "A3:D193", col_names = NA))
colnames(d) <- c("wavelength (nm)", paste(c("90", "45", "0"), "degrees"))
dl <- melt(d, id.vars = "wavelength (nm)")
fig2b <- ggplot(dl, aes(x = `wavelength (nm)`, y = value, colour = variable)) + 
  geom_line() +
  labs(colour = "Measurement angle",
       y = expression("Irradiance (mW m"^-2*" nm"^-1*")"),
       x = "Wavelength (nm)") +
  xlim(c(350, 750)) +
  theme(legend.position = "top") +
  sgg() +
  plot_annotation(tag_levels = list("b"), tag_prefix = "(", tag_suffix = ")")
if (make_plots) {
  cairo_pdf(here("plots", "figure2b_R1.pdf"),
            width = 7, height = 7)
  print(fig2b)
  dev.off()
  jpeg(here("plots", "figure2b_R1.jpg"),
       res = 300, width = (480 * 5), height = (480 * 5))
  print(fig2b)
  dev.off()
}

# new figure 3
fig3 <- catch_sps_effects_box / len_sps_hist + 
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
  sgg()
if (make_plots) {
  cairo_pdf(here("plots", "figure3_R1.pdf"),
            width = 10, height = 10)
  print(fig3)
  dev.off()
  jpeg(here("plots", "figure3_R1.jpg"),
       res = 300, width = (480 * 7), height = (480 * 7))
  print(fig3)
  dev.off()
}
