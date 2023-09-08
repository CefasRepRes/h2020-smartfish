# analysis options --------------------------------------------------------

# project name
prj_nm <- 'h2020-smartfish'

# which species (FAO 3-alpha species code)
tot_sp_cd <- c('Haddock' = 'HAD',
               'Grey gurnard' = 'GUG', 
               'Megrim' = 'MEG', 
               'Whiting' = 'WHG')
spl_sp_cd <- c('Haddock' = 'HAD',
               'Grey gurnard' = 'GUG', 
               'Northern squid' = 'NSQ', 
               'Whiting' = 'WHG')
sp_cd <- c(tot_sp_cd, spl_sp_cd)[!duplicated(c(tot_sp_cd, spl_sp_cd))]

# length data file name
len_nm <- 'C5979R Data_entered & checked - v04.xlsx'

# number of computer "cores" to use for computation
nc <- 3

# comparison statistic: AICc, BIC
comp_stat <- 'BIC'

# MCMC settings
setts <- list('na' = 10,
              'nb' = 100,
              'ni' = 500,
              'nt' = 1)
setts.m <- 100
if (!testing) setts <- lapply(setts, function(v) v * setts.m)
setts$nc <- 3
