### PROJECT:  Somalia JMMI
### PURPOSE:  Analyse JMMI data, this keeps all price data <------------------
### INPUT:    data.csv, data2.csv
### OUTPUT:   JMMI_[yymm]_analysed_[location]_[date].xlsx"
### AUTHOR:   Tie Franco Brotto
### LAST UPDATED: 04 December, 2020 by Tie Franco Brotto

#load required packages
library(tidyverse)
library(data.table)
library(openxlsx)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(plotly)
library(processx)

# Clear memory
rm(list = ls())

# Set REACH colors
cc_red = "#ee5859"
cc_grey = "#58595a"

# Set Directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load JMMI data
df_base <- read.csv("input/data_new_august.csv", stringsAsFactors = F, header = T, na.strings = c("", " ", "NA")) %>%
  mutate(across(starts_with("price_"), as.double)) %>%
  mutate(across(starts_with("spec_"), as.character))
# fix bug on data type
df_base$today <- as.integer(df_base$today)
df_base_old <- read.csv("input/data_old_may.csv", stringsAsFactors = F, header = T, na.strings = c("", " ", "NA")) %>%
  mutate(across(starts_with("price_"), as.double)) %>%
  mutate(across(starts_with("spec_"), as.character))
df_base_merge <- df_base %>%
  bind_rows (df_base_old)

# List of rounds
ll_round <- df_base_merge %>%
  distinct(round_id) %>%
  pull(round_id)

current_round <- sort(ll_round, TRUE)[1]
previous_round <- sort(ll_round, TRUE)[2]
analysis_rounds <- c(current_round, previous_round)

# List of select multiple questions
sm <- as_tibble(rownames(t(df_base_merge))) %>%
  mutate(value = gsub('[.].*',"",value)) %>%
  mutate(dup = duplicated(value)) %>%
  filter(dup == TRUE) %>%
  distinct() %>%
  pull(value)

# Define vars to be used in the output price loop
pr_ic <- ""
pr_ip <- ""

for (a in analysis_rounds) {
  df <- df_base_merge %>%
    filter(round_id == a) %>%
    group_by(call_location)
  
  # Number of interviews selected
  nn <- df %>%
    select(call_location) %>%
    summarise(nn = n())
  
  # Filter price data 
  ve_x <- df %>%
    # Adds all columns starting with price_usd_ and
    select(starts_with("price_usd_"), vendor_type) %>%
    # Excludes the _other prices (which should have been manually moved to price_usd_ during checks)
    select(!ends_with("_other"))
  
  # Vendor type table
  ve_t <- ve_x %>%
    # Pivot so that we can create an items column
    pivot_longer(starts_with("price_usd_"),
                 names_to = "item",
                 values_to = "value") %>%
    filter(!is.na(value)) %>%
    ungroup(call_location) %>%
    select(vendor_type, item) %>%
    distinct()
  
  
  # Price analysis and vendor number
  pr_i <- ve_x %>%
    select(!vendor_type) %>%
    # Get number of vendors reporting each item (nv), median, and quartiles
    summarise(across(everything(),  list(
      nv = ~ sum(!is.na(.)),
      median = ~ median(.x, na.rm = TRUE),
      q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
      q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
    ))) %>%
    # Pivot so that we can create an items column
    pivot_longer(!any_of("call_location"),
                 names_to = "key",
                 values_to = "value") %>%
    # obs. the regex "_(?!.*_)" means the last _ in the string
    separate(key, c("item", "info"), sep = "_(?!.*_)") %>%
    pivot_wider(names_from = "info", values_from = "value") %>%
    # Add a check column for all items with less than three prices (OR two prices for water suppliers)
    mutate(check = case_when(str_detect(item, "truck") ~ nv > 1,
                             str_detect(item, "communal") ~ nv > 1,
                             str_detect(item, "piped") ~ nv > 1,
                             TRUE ~ nv > 2)) %>%
    # Delete items with less than three prices
    # filter(check == TRUE) %>%
    # select(!check) %>%
    # Add vendor type
    left_join(ve_t, by="item") %>%
    # Rename items 
    mutate(item = str_to_title(gsub('_'," ",gsub('price_usd_',"",item)))) %>%
    # Order by vendor type and then item
    arrange(vendor_type,item)
  
  # GLOBAL Prices
  pr_iz <- pr_i %>%
    group_by(item) %>%
    summarise(
      call_location = "_global",
      nv = sum(nv),
      q1 = quantile(median, 0.25, na.rm = TRUE),
      q3 = quantile(median, 0.75, na.rm = TRUE),
      global_sd = sd(median, na.rm = TRUE),
      median = median(median, na.rm = TRUE),
      vendor_type = vendor_type,
      check = TRUE
    ) %>%
    distinct()
  pr_i <- pr_i %>%
    bind_rows(pr_iz) %>%
    # Unite
    unite("key", call_location, item, sep = "_1_")
  
  # Write to vars
  if (a == current_round) {
    pr_ic <- pr_i
  } else {
    pr_ip <- pr_i
  }
}

pr_ii <- pr_ic %>%
  full_join(pr_ip, by = "key") %>%
  separate(key, c("call_location", "item"), sep = "_1_") %>%
  mutate(valid = (check.x == TRUE & check.y == TRUE)) %>%
  mutate(price_change = (median.x / median.y) - 1) %>%
  select(!ends_with(".y"))

pr_ix <- pr_ii %>%
  filter(call_location == "_global")

pr_ii <- pr_ii %>%
  filter(call_location != "_global") %>%
  select(call_location, item, median.x, check.x, valid, price_change)

# List of locations
ll_locations <- pr_ii %>%
  distinct(call_location) %>%
  pull(call_location)

for (b in ll_locations) {
  temp <- pr_ii %>%
    filter(call_location == b)
  
  pr_ix <- pr_ix %>%
    full_join(temp, by = "item")
}




# #># Items availability table
# 
# av_i <- pr_i %>%
#   select(item, nv, vendor_type) 
# 
# av_ix <- df %>%
#   select(starts_with("avail_shop")) 
# #this needs to be continued


###--------------------WRITE--------------------

# Write analysed file
OUT <- createWorkbook()
# Add some sheets to the workbook
addWorksheet(OUT, paste0('price_comparison'))
# Write the data to the sheets
writeData(OUT, sheet = paste0('price_comparison'), x = pr_ix)

date <- gsub(' ',"_",gsub(":","",paste0(Sys.time())))
# Export the file
saveWorkbook(OUT, paste0('output/analysis/JMMI_priceComparison_', date , '.xlsx'), overwrite = TRUE)

