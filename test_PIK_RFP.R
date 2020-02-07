rm(list = ls())

# After trials here, this has moved to '/check' folder under RFP-pik repository. Check out CheckLibrary.R mainly.

library(tidyverse)
library(rfp)

cd_links_datapath = "C:/Users/min/IIASA/VAN RUIJVEN Bas - Finance/UNEP FI/Data/CDLINKS_stocktaking_MESSAGE_native_regions_FULL.csv"
df_cdlinks = read.csv(cd_links_datapath)
names(df_cdlinks) = tolower(names(df_cdlinks)) # PIK's rfp library has them in lower case.

rmapping = data.frame(region_data='World', region_assumption='World')

df_cdlinks_l = df_cdlinks %>% gather(key="period", "value", -(model:unit)) %>% 
  mutate(period = as.numeric(gsub("x", "", period))) %>%
  filter(region=="World" & !grepl("NoPolicy", scenario)) %>%
  mutate(scenario = paste0("CD-LINKS_", gsub("_V4", "", scenario))) %>%
  mutate(scenario = ifelse(grepl("SSP1|SSP3", scenario), scenario, paste0(scenario, "_SSP2"))) %>%
  mutate(region = factor(region, levels = levels(assumptions$DEC_OilGasCoalShares$region))) %>% 
  mutate(baseline = case_when(grepl("SSP1", scenario) ~ "CD-LINKS_NPi_SSP1",
                              grepl("SSP3", scenario) ~ "CD-LINKS_NPi_SSP3",
                              TRUE ~ "CD-LINKS_NPi_SSP2")) %>%
  mutate(baseline = ifelse(scenario==baseline, NA, baseline)) 

# df_rfp_pik_l = compute_rfp(df_cdlinks_l, rmapping, VERSION = "pik")

# df_rfp_pik_l = rfp_direct_cost(df_cdlinks_l, VERSION = "pik")
# df_rfp_pik_l = rfp_indirect_cost(df_cdlinks_l, VERSION = "pik")
# df_rfp_pik_l = rfp_lcinv(df_cdlinks_l, rmapping, VERSION = "pik")
# df_rfp_pik_l = rfp_revenue(df_cdlinks_l, VERSION = "pik")

df_rfp_unepfi1_l = compute_rfp(df_cdlinks_l, i_mapping_region=rmapping, VERSION = "unepfi1", VERBOSE=FALSE)
# df_rfp_unepfi1_l = rfp_direct_cost(df_cdlinks_l, VERSION = "unepfi1", VERBOSE = TRUE)  # Trying to debug having all NA for indirect cost part
# df_rfp_unepfi1_l = rfp_indirect_cost(df_cdlinks_l, VERSION = "unepfi1", VERBOSE = TRUE)  # Trying to debug having all NA for indirect cost part
df_rfp_unepfi1_l = rfp_lcinv(df_cdlinks_l, rmapping, VERSION = "unepfi1", VERBOSE = TRUE)  # Trying to debug having all NA for indirect cost part
# df_rfp_unepfi1_l = rfp_revenue(df_cdlinks_l, VERSION = "unepfi1", VERBOSE = TRUE)  # Trying to debug having all NA for indirect cost part

df_rfp_unepfi1 = df_rfp_unepfi1_l %>% spread(period, value) 


df_all = df_rfp_pik %>% left_join(df_rfp_unepfi1 %>% rename('value_unep'='value')) %>% mutate(flag= value - value_unep)
