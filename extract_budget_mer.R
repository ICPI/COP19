

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)


# BUDGET ------------------------------------------------------------------

folderpath <- "~/ICPI/COP19 Funding Letters"

df_budget <- read_xlsx(file.path(folderpath, "a. Summary of COP19 Planning Levels.xlsx"), 
                       sheet = "COP19 Planning Levels_FMTD",
                       skip = 4)

df_budget <- df_budget %>% 
  select(ou = `Operating Unit (OU); note: Special Notification Countries are highlighted`,
         ends_with("(amount)"), `TOTAL PLANNING LEVEL..4`, starts_with("of which"), 
         `Total COP19 New Funding`, starts_with("Applied"), -ends_with("(TOTAL)")) %>% 
  gather(col, val, -ou) %>%
  mutate(col = str_remove(col, " \\(amount\\)"),
         col = str_replace(col, "Earmark for ", "earmark,"),
         col = str_replace(col, "of which, ", "total_new_funding,"),
         col = str_replace(col, "TOTAL PLANNING LEVEL..4", "planning_level"),
         col = str_replace(col, "Total COP19 New Funding", "total_new_funding,total"),
         col = str_replace(col, "Applied Pipeline: ", "applied_pipeline,"),
         col = str_replace(col, "All other", "all other"),
         col = str_replace(col, " ", "_")) %>% 
  separate(col, into = c("indicator", "disaggregate"), sep =",") %>% 
  mutate(class = "budget") %>% 
  select(ou, class, everything())


# MER ---------------------------------------------------------------------

folderpath_msd <- "~/ICPI/Data"

df_mer <- read_rds(file.path(folderpath_msd, "MER_Structured_Dataset_OU_IM_FY17-18_20181221_v2_1.rds"))

df_mer <- df_mer %>% 
  filter((indicator %in% c("TX_NEW", "TX_CURR", "TB_PREV", "VMMC_CIRC") &
         numeratordenom == "N" & agecoarse %in% c("<15", "15+")) |
         (indicator %in% c("TX_NEW", "TX_CURR", "TB_PREV", "VMMC_CIRC", 
                             "HTS_TST", "HTS_TST_POS", "KP_PREV", "PREP_NEW", 
                             "KP_MAT", "OVC_SERV", "TX_PVLS") & 
           standardizeddisaggregate == "Total Numerator")) %>% 
  unite(disaggregate , c(sex, agecoarse), sep = " ") %>% 
  group_by(operatingunit, indicator, disaggregate) %>% 
  summarize(val = sum(fy2018apr, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(disaggregate = str_replace(disaggregate, "NA NA", "total")) %>% 
  mutate(class = "mer") %>% 
  select(ou = operatingunit, class, everything())


# COMBINE OUTPUT ----------------------------------------------------------

bind_rows(df_budget, df_mer)  %>% 
write_csv(file.path(folderpath, "planning_source_data.csv"), na = "")
