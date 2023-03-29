# import packages
library(dplyr)
library(tidyverse)
# import data
data(doubs,package = "ade4")
class(doubs)

# rownames_to_column(env,var="site") convert rownames_to_column
env <- doubs$env
env_tb <- as_tibble(rownames_to_column(env,var="site"))
env_tb

#subset select , rename columns , arrange in order
subset(env_tb,dfs>1000) %>%
  select(site, dfs, slo, flo, pH, nit, oxy) %>%
  rename(distsour = dfs, slope = slo, flowrate = flo, nitrogen = nit, oxygen = oxy) %>%
  arrange(slope) %>%
  arrange(desc(pH))
