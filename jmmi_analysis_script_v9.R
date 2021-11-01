### PROJECT:  Somalia JMMI
### PURPOSE:  Analyse JMMI data, this keeps all price data <------------------
### INPUT:    data.csv, data2.csv
### OUTPUT:   JMMI_[yymm]_analysed_[location]_[date].xlsx"
### AUTHOR:   Tie Franco Brotto
### LAST UPDATED: 04 December, 2020 by Tie Franco Brotto

#load required packages
library(tidyverse)
#library(data.table)
library(openxlsx)
library(questionr)
#library(sf)
#library(processx)

# Clear memory
rm(list = ls())

# Set REACH colors
cc_red = "#ee5859"
cc_grey = "#58595a"

# Set Directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load JMMI data
df_base <- read.csv("input/data.csv", stringsAsFactors = FALSE, header = T, na.strings=c(""," ","NA"))
#df_base2 <- read.csv("C:/Users/Tie Franco Brotto/Documents/Tie Franco Brotto/3_NFI JMMI/06_Products/Shiny/data2.csv", stringsAsFactors = FALSE, header = T, na.strings=c(""," ","NA"))

# Make tibble copy
df <- as_tibble(df_base) %>%
  group_by(call_location)
# df2 <- as_tibble(df_base2) %>%
#   group_by(supplier_destination)

# Initialise location
ll <- "all"
# Filter location if not "all"
if (ll != "all") {
  df <- df %>%
    filter(call_location == ll)
}

# Initialise date
dd <- "2008"
# This needs to be changed in the future, so that we always load the latest data
if (dd != "2008") {
  df <- df %>%
    # obs. we need to manually add a column in the dataset with the date of the assessment as 'YYMM'
    filter(jmmi_date == dd)
}

# Number of interviews selected
nn <- df %>%
  select(call_location) %>%
  summarise(nn = n())

# List of select multiple questions
sm <- as_tibble(rownames(t(df))) %>%
  mutate(value = gsub('[.].*',"",value)) %>%
  mutate(dup = duplicated(value)) %>%
  filter(dup == TRUE) %>%
  distinct() %>%
  pull(value)

### Vendor type table
ve_x <- df %>%
  # Adds all columns starting with price_usd_ and
  # Excludes the _other prices (which should have been manually moved to price_usd_ during checks)
  select(starts_with("price_usd_"), vendor_type) %>%
  select(!ends_with("_other")) 

ve_t <- ve_x %>%
  # Pivot so that we can create an items column
  pivot_longer(starts_with("price_usd_"),
               names_to = "item",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  ungroup(call_location) %>%
  select(vendor_type, item) %>%
  distinct()
  

### Price analysis
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


#># Items availability table
av_i <- pr_i %>%
  select(item, nv, vendor_type) 

av_ix <- df %>%
  select(starts_with("avail_shop")) 
#this needs to be continued







### Stock analysis

st_i <- df %>%
  # Adds all columns related to stock
  select(starts_with(c("stock_len_", "stock_time_"))) %>%
  # Get median and quartiles
  summarise(across(everything(),  list(
    nv = ~ sum(!is.na(.)),
    median = ~ median(.x, na.rm = TRUE),
    q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
    q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
  ))) %>%
  pivot_longer(!any_of("call_location"),
               names_to = "key",
               values_to = "value") %>%
  # obs. the regex "_(?!.*_)" means the last _ in the string
  separate(key, c("item", "info"), sep = "_(?!.*_)") %>%
  pivot_wider(names_from = "info", values_from = "value") 

#># Stock length table

st_l <- st_i %>%
  rename(len_m = median, len_q1 = q1, len_q3 = q3) %>%
  filter(str_detect(item, "stock_len_")) %>%
  mutate(item = gsub('stock_len_',"",item)) %>%
  unite("name2", call_location, item, sep = "__")

#># Stock time table

st_t <- st_i %>%
  select(!nv) %>%
  rename(tim_m = median, tim_q1 = q1, tim_q3 = q3) %>%
  filter(str_detect(item, "stock_time_")) %>%
  mutate(item = gsub('stock_time_',"",item)) %>%
  unite("name2", call_location, item, sep = "__")
  
#># Stock difficult table

st_d <- df %>%
  # Adds all columns related to stock
  select(starts_with("stock_dif_")) %>%
  mutate(across(everything(), ~gsub("dk",'no',.))) %>%
  mutate(across(everything(), ~gsub("pnta",'no',.))) %>%
  pivot_longer(!any_of("call_location"), names_to = "name", values_to = "value") %>%
  unite("key", call_location, name, sep = "_1_") %>%
  unite("key", key, value, sep = "_2_") %>%
  count(key) %>%
  separate(key, c("key", "value"), sep = "_2_") %>%
  separate(key, c("call_location", "name"), sep = "_1_") %>%
  na_if("NA") %>%
  mutate(name1 = gsub('stock_dif_',"",name)) %>%
  full_join(nn, by = "call_location") %>%
  unite("name2", call_location, name1, sep = "__") %>%
  unite("key", name, value, sep = "__")

# This will calculate the number of responses, by excluding the NAs
st_dx <- st_d %>%
  filter(str_detect(key, "__NA")) %>%
  mutate(nn = nn - n) %>%
  select(name2,nn)

st_d <- st_d %>%
  filter(!str_detect(key, "__NA")) %>%
  left_join(st_dx, by="name2") %>%
  mutate(nn = case_when(is.na(nn.y) ~ nn.x,
                        !is.na(nn.y) ~ nn.y)) %>%
  mutate(pc = n/nn) %>%
  select(name2, item = key, dif = n, dif_pc = pc) 

# Create a temporary table to convert 100% no in 0% yes
st_d0 <- st_d %>%
  filter(str_detect(item, "__no")) %>%
  filter(dif_pc == 1) %>% # To avoid an error, eg NO 90% DK 10%, must convert all DK and PNTA to NO (see above)
  mutate(item = gsub("__no",'__yes',item)) %>%
  mutate(dif_pc = 0) %>%
  mutate(dif = 0)

# Merge back
st_d <- st_d %>%
  filter(str_detect(item, "__yes")) %>%
  bind_rows(st_d0) %>%
  mutate(item = gsub('stock_dif_',"",gsub('__yes',"",item)))

# Fix item names for the non-price indicators 
ve_t <- ve_t %>%
  # obs. the regex "_(?!.*_)" means the last _ in the string
  separate(item, c("item", "extra"), sep = "_(?!.*_)") %>%
  mutate(item = gsub('price_usd_',"",item)) %>%
  select(!extra) %>%
  distinct()

#># Finalize main stock table

st_i <- st_l %>%
  full_join(st_t, by = "name2") %>%
  full_join(st_d, by = "name2") %>%
  # obs. the regex "_{1}" means the first _ in the string
  separate(name2, c("call_location", "item"), sep = "__") %>%
  # Add vendor type
  left_join(ve_t, by="item") %>%
  # Clear NAs
  filter(!is.na(len_m)) %>%
  # Order by vendor type and then item
  arrange(vendor_type,item) %>%
  mutate(check = case_when(
    str_detect(item, "truck") ~ nv > 1,
    str_detect(item, "communal") ~ nv > 1,
    str_detect(item, "piped") ~ nv > 1,
    TRUE ~ nv > 2
  )) 


### Barriers

df[] <- lapply(df, as.character)

ba_x <- df %>%
  # Adds all columns related to stock
  select(starts_with("barrier_")) %>%
  # Remove the "other" columns
  select(!ends_with("_other")) %>%
  # Delete the multiple question columns (which are already represented in separate columns for each option)
  select(!any_of(sm)) %>%
  summarize(across(everything(), ~list(count(as_tibble_col(.), across(everything()))))) %>% 
  pivot_longer(!any_of("call_location"), names_to = "name", values_to = "value") %>% 
  unnest(value) %>%
  mutate(name2 = paste0(call_location,"_",name)) %>%
  full_join(nn, by = "call_location") %>%
  unite("key", name, value, sep = "__") 

# Get the number (ie "n") of empty lines, then subtracts from the total number of surveys in that location
ba_xx <- ba_x %>%
  filter(str_detect(key, "__NA")) %>%
  mutate(nn = nn - n) %>%
  select(name2,nn)

ba_x <- ba_x %>%
  filter(!str_detect(key, "__NA")) %>%
  left_join(ba_xx, by="name2") %>%
  mutate(nn = case_when(is.na(nn.y) ~ nn.x,
                        !is.na(nn.y) ~ nn.y)) %>%
  mutate(pc = n/nn) %>%
  select(call_location, item = key, n, pc) %>%
  filter(!str_detect(item, "__0")) %>%
  mutate(item = gsub('\\.',"__",gsub('__1',"",item)))


### Payment + barter

pa_x <- df %>%
  # Adds all columns related to payment
  select(starts_with(c("currency_","payment_","barter_"))) %>%
  # Remove the "other" columns
  select(!ends_with("_other")) %>%
  # Delete the multiple question columns (which are already represented in separate columns for each option)
  select(!any_of(sm)) %>%
  summarize(across(everything(), ~list(count(as_tibble_col(.), across(everything()))))) %>% 
  pivot_longer(!any_of("call_location"), names_to = "name", values_to = "value") %>% 
  unnest(value) %>%
  mutate(name2 = paste0(call_location,"_",name)) %>%
  full_join(nn, by = "call_location") %>%
  unite("key", name, value, sep = "__")  

pa_xx <- pa_x %>%
  filter(str_detect(key, "__NA")) %>%
  mutate(nn = nn - n) %>%
  select(name2,nn)

pa_x <- pa_x %>%
  filter(!str_detect(key, "__NA")) %>%
  left_join(pa_xx, by="name2") %>%
  mutate(nn = case_when(is.na(nn.y) ~ nn.x,
                        !is.na(nn.y) ~ nn.y)) %>%
  mutate(pc = n/nn) %>%
  select(call_location, item = key, n, pc) %>%
  filter(!str_detect(item, "__0")) %>%
  mutate(item = gsub('\\.',"__",gsub('__1',"",item)))


### Supplier

su_x <- df %>%
  # Adds all columns related to stock
  select(starts_with(c("supplier_","transportation_"))) %>%
  # Remove number of suppliers
  select(!contains("supplier_number")) %>%
  # Remove the "other" columns
  select(!ends_with("_other")) %>%
  # Delete the multiple question columns (which are already represented in separate columns for each option)
  select(!any_of(sm)) %>%
  summarize(across(everything(), ~list(count(as_tibble_col(.), across(everything()))))) %>% 
  pivot_longer(!any_of("call_location"), names_to = "name", values_to = "value") %>% 
  unnest(value) %>%
  mutate(name2 = paste0(call_location,"_",name)) %>%
  full_join(nn, by = "call_location") %>%
  unite("key", name, value, sep = "__")  

su_xx <- su_x %>%
  filter(str_detect(key, "__NA")) %>%
  mutate(nn = nn - n) %>%
  select(name2,nn)

su_x <- su_x %>%
  filter(!str_detect(key, "__NA")) %>%
  left_join(su_xx, by="name2") %>%
  mutate(nn = case_when(is.na(nn.y) ~ nn.x,
                        !is.na(nn.y) ~ nn.y)) %>%
  mutate(pc = n/nn) %>%
  select(call_location, item = key, n, pc) %>%
  filter(!str_detect(item, "__0")) %>%
  mutate(item = gsub('\\.',"__",gsub('__1',"",item)))


### Supplier numbers

sn_x <- df %>%
  # Adds all columns related to credit numbers
  select(contains(c("supplier_number","vendor_type"))) %>%
  group_by(call_location, vendor_type) %>%
  mutate(across(!starts_with("call"), as.numeric)) %>%
  # Get median and quartiles
  summarise(median = median(supplier_number, na.rm = TRUE),
    q1 = quantile(supplier_number, 0.25, na.rm = TRUE),
    q3 = quantile(supplier_number, 0.75, na.rm = TRUE)
  ) %>%
  filter(!str_detect(vendor_type, "water"))


### Credit and MBP (and gender)

cr_x <- df %>%
  # Adds all columns related to stock
  select(starts_with(c("credit_","cva_","customer_","gender_"))) %>%
  # Rename to keep column
  mutate(cva_effect_others = cva_effect_other) %>%
  # Remove the "other" columns
  select(!ends_with("_other")) %>%
  # Remove the numeric columns of credit
  select(!starts_with(c("credit_max","credit_total"))) %>%
  # Delete the multiple question columns (which are already represented in separate columns for each option)
  select(!any_of(sm)) %>%
  summarize(across(everything(), ~list(count(as_tibble_col(.), across(everything()))))) %>%
  pivot_longer(!any_of("call_location"), names_to = "name", values_to = "value") %>% 
  unnest(value) %>%
  mutate(name2 = paste0(call_location,"_",name)) %>%
  full_join(nn, by = "call_location") %>%
  unite("key", name, value, sep = "__")  

cr_xx <- cr_x %>%
  filter(str_detect(key, "__NA")) %>%
  mutate(nn = nn - n) %>%
  select(name2,nn)

cr_x <- cr_x %>%
  filter(!str_detect(key, "__NA")) %>%
  left_join(cr_xx, by="name2") %>%
  mutate(nn = case_when(is.na(nn.y) ~ nn.x,
                        !is.na(nn.y) ~ nn.y)) %>%
  mutate(pc = n/nn) %>%
  select(call_location, item = key, n, pc) %>%
  filter(!str_detect(item, "__0")) %>%
  mutate(item = gsub("\\.","__",gsub('__1',"",item)))


### Credit numbers

cn_x <- df %>%
  # Adds all columns related to credit numbers
  select(starts_with(c("credit_max","credit_total"))) %>%
  mutate(across(!starts_with("call"), as.numeric)) %>%
  # Get median and quartiles
  summarise(across(everything(),  list(
    median = ~ median(.x, na.rm = TRUE),
    q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
    q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
  )))
  # pivot_longer(!any_of("call_location"),
  #              names_to = "key",
  #              values_to = "value") %>%
  # # obs. the regex "_(?!.*_)" means the last _ in the string
  # separate(key, c("item", "info"), sep = "_(?!.*_)") %>%
  # pivot_wider(names_from = "info", values_from = "value") 


###--------------------GLOBAL--------------------

# Prices
pr_iz <- pr_i %>%
  # Delete items with less than three prices
  filter(check == TRUE) %>%
  group_by(item) %>%
  summarise(
    call_location = "_global",
    nv = sum(nv),
    q1 = median(q1, na.rm = TRUE),
    q3 = median(q3, na.rm = TRUE),
    median = median(median, na.rm = TRUE),
    vendor_type = vendor_type
    ) %>%
  distinct()
pr_i <- pr_i %>%
  bind_rows(pr_iz)

# Stock
st_iz <- st_i %>%
  # Delete items with less than three prices
  filter(check == TRUE) %>%
  group_by(item) %>%
  summarise(
    call_location = "_global",
    across(where(is.numeric), ~ median(.x, na.rm = TRUE)),
    vendor_type = vendor_type
    ) %>%
  distinct()
st_i <- st_i %>%
  bind_rows(st_iz)

# Barrier
ba_xz <- ba_x %>%
  select(!n) %>%
  # Horizontalize
  pivot_wider(names_from = "item", values_from = "pc") %>%
  replace(is.na(.),0)
# Create a line with medians of medians
ba_xz0 <- ba_xz %>%
  summarise(across(!starts_with("call_location"), ~ median(.x, na.rm = TRUE))) %>%
  mutate(call_location = "_global")
# Merge global values with locations
ba_x <- ba_xz %>%
  bind_rows(ba_xz0) %>%
  select(sort(names(.))) %>%
  relocate(call_location)


### Payment
pa_xz <- pa_x %>%
  select(!n) %>%
  # Horizontalize
  pivot_wider(names_from = "item", values_from = "pc") %>%
  replace(is.na(.),0)
# Create a line with medians of medians
pa_xz0 <- pa_xz %>%
  summarise(across(!starts_with("call_location"), ~ median(.x, na.rm = TRUE))) %>%
  mutate(call_location = "_global")
# Merge global values with locations
pa_x <- pa_xz %>%
  bind_rows(pa_xz0) %>%
  select(sort(names(.))) %>%
  relocate(call_location)


### Supplier
su_xz <- su_x %>%
  group_by(item) %>%
  summarise(
    call_location = "_global",
    n = sum(n),
    pc = median(pc, na.rm = TRUE)
  ) %>%
  distinct()
su_x <- su_x %>%
  bind_rows(su_xz)

### Supplier numbers
sn_xz <- sn_x %>%
  group_by(vendor_type) %>%
  summarise(
    call_location = "_global",
    across(where(is.numeric), ~ median(.x, na.rm = TRUE))
  ) %>%
  distinct()
sn_x <- sn_x %>%
  bind_rows(sn_xz)

### Credit
cr_xz <- cr_x %>%
  select(!n) %>%
  # Horizontalize
  pivot_wider(names_from = "item", values_from = "pc") %>%
  replace(is.na(.),0)
# Create a line with medians of medians
cr_xz0 <- cr_xz %>%
  summarise(across(!starts_with("call_location"), ~ median(.x, na.rm = TRUE))) %>%
  mutate(call_location = "_global")
# Merge global values with locations
cr_x <- cr_xz %>%
  bind_rows(cr_xz0) %>%
  select(sort(names(.))) %>%
  relocate(call_location)


### Credit numbers
cn_xz <- cn_x %>%
  summarise(
    call_location = "_global",
    across(where(is.numeric), ~ median(.x, na.rm = TRUE))
  ) %>%
  distinct()
cn_x <- cn_x %>%
  bind_rows(cn_xz)

## Currency exchange rate by location

jmmi_prop <- function(table){
  df <- prop_table(
    table,
    total = TRUE,
    percent = TRUE
  )
}

curr_ex <- table(df$call_location, df$currency_main)
pct <- prop_table(
  curr_ex,
  total = TRUE,
  percent = TRUE
)
pct

# aggregation of price exchange per location
descriptive_stats <- function(data, group_var, items){
  data %>% select(group_var, items) %>% 
    group_by(!!sym(group_var)) %>% 
    group_modify(~ {
      .x %>% 
        purrr::map_dfc(fivenum) %>% 
        mutate(c("min", "Q1", "median", "Q3", "max"))
    })
}

currency_exchange <- descriptive_stats(df, "call_location", items = "currency_exchange")

# accepted payments
accepted_payments <- descriptive_stats(df, "call_location", items = c("payment_type.cash",
                                                                      "payment_type.mobile",
                                                                      "payment_type.voucher",
                                                                      "payment_type.other",
                                                                      "payment_type.none"))
# Currency exchange rate
aggregate(currency_exchange ~ call_location, df, median)

### main suppliers
main_supplier <- table(df$call_location, df$supplier_country) %>% jmmi_prop()

# supply routes per location
supp_route <- table(df$supplier_country, df$supplier_route) %>% jmmi_prop()

# supplier challanges
supp_challanges <- df %>% select(starts_with("barrier_transportation"), call_location) %>% select(2:15) %>% 
  pivot_longer(starts_with("barrier_transportation"),
               names_to = "challanges",
               values_to = "values") %>% 
  filter(!is.na(values)) %>% filter(values != 0) %>% 
  group_by(call_location, challanges)

supp_ch <- table(supp_challanges$call_location, supp_challanges$challanges)

###--------------------SUPPLY_CHAIN--------------------


# ### Origins
# 
# or_x <- df2 %>%
#   select(!contains("items")) %>%
#   mutate(supplier_origin = case_when(is.na(supplier_district) ~ supplier_country,
#                                      TRUE ~ supplier_district)) %>%
#   select(supplier_destination, supplier_origin, supplier_route) %>%
#   unite("pair",supplier_destination, supplier_origin,sep = "__") %>%
#   group_by(pair) 
# 
# nn <- or_x %>%
#   select(pair) %>%
#   summarise(nn = n())
# 
# or_x <- or_x %>%
#   summarize(across(everything(), ~list(count(as_tibble_col(.), across(everything()))))) %>%
#   pivot_longer(!any_of("pair"), names_to = "name", values_to = "value") %>% 
#   unnest(value) %>%
#   mutate(name2 = paste0(pair,"_",name)) %>%
#   full_join(nn, by = "pair") %>%
#   unite("key", name, value, sep = "__") 
# 
# or_xx <- or_x %>%
#   filter(str_detect(key, "__NA")) %>%
#   mutate(nn = nn - n) %>%
#   select(name2,nn)
# 
# or_x <- or_x %>%
#   filter(!str_detect(key, "__NA")) %>%
#   left_join(or_xx, by="name2") %>%
#   mutate(nn = case_when(is.na(nn.y) ~ nn.x,
#                         !is.na(nn.y) ~ nn.y)) %>%
#   mutate(pc = n/nn) %>%
#   select(pair, item = key, n, pc)
#   
# ### Items
# 
# or_i <- df2 %>%
#   mutate(supplier_origin = case_when(is.na(supplier_district) ~ supplier_country,
#                                      TRUE ~ supplier_district)) %>%
#   group_by(supplier_origin) %>% 
#   select(contains("items.")) %>%
#   # Get median and quartiles
#   summarise(across(everything(),  sum)) %>%
#   pivot_longer(!any_of("supplier_origin"),
#                names_to = "item",
#                values_to = "value") %>%
#   filter(value != 0) %>%
#   mutate(item = gsub("supplier_items.","",item))

###--------------------WRITE--------------------

##### September 2021 Update #####
# Currency exchange rate per location
jmmi_prop <- function(table){ # function for generating proportions
  d <- prop_table(
    table,
    total = FALSE,
    percent = TRUE
  )
}

# main currency used
curr_main_pro <- table(df$call_location, df$currency_main) %>% jmmi_prop()

# currency exchange rete
curr_exch_pro <- aggregate(currency_exchange ~ call_location, df, median)

## Supplier Analysis
# main suppliers
main_supp <- table(df$call_location, df$supplier_country) %>% jmmi_prop()

# supply routes per location
supp_route <- table(df$supplier_country, df$supplier_route) %>% jmmi_prop()

## 
currency_main <- df %>% group_by(currency_main) %>% 
  summarize(Freq = n()) %>% mutate(Prop = Freq / sum(Freq)) %>% arrange(desc(Prop))

ggplot(currency_main, mapping = aes(x= currency_main, y=Prop))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limits = currency_main$currency_main) +
  theme_minimal()

currency_per_location <- svytable(~ df$call_location + currency_main, design = call_location)

## Median Prices
descriptive_stats <- function(data, group_var, items){
  data %>% select(group_var, items) %>% 
    group_by(!!sym(group_var)) %>% 
    group_modify(~ {
      .x %>% 
        purrr::map_dfc(fivenum) %>% 
        mutate(c("min", "Q1", "median", "Q3", "max"))
    })
}


#----------------------------
median_calculation <- function(df, price_list, aggregation){
  
  y <- df %>% select(aggregation, price_list) %>% group_by(!!sym(aggregation)) %>% summarise_all(funs(median(., na.rm = TRUE)))
  print(y)
  
}


ah <- median_calculation(df, price_list = c("price_usd_water_communal_1",
                                            "price_usd_water_piped_1",
                                            "price_usd_water_truck_1"),
                         aggregation = "call_location")

#---------------------------
# wash kit
wash_kit <- descriptive_stats(df, group_var = "call_location", items = c("price_usd_soap_1",
                                                                         "price_usd_washing_powder_1",
                                                                         "price_usd_mhm_1",
                                                                         "price_usd_bucket_1",
                                                                         "price_usd_jerry_can_1"))
# water kit
water_kit <- descriptive_stats(df, group_var = "call_location", items = c("price_usd_water_communal_1",
                                                                          "price_usd_water_piped_1",
                                                                          "price_usd_water_truck_1"))

# shelter kit
shelter_kit <- descriptive_stats(df, group_var = "call_location", items = c("price_usd_plastic_sheet_1",
                                                                            "price_usd_plastic_sheet_2",
                                                                            "price_usd_blanket_1",
                                                                            "price_usd_sleeping_mat_1",
                                                                            "price_usd_cooking_pot_1",
                                                                            "price_usd_cooking_pot_2",
                                                                            "price_usd_plate_1",
                                                                            "price_usd_bowl_1",
                                                                            "price_usd_cup_1",
                                                                            "price_usd_spoon_1",
                                                                            "price_usd_serving_spoon_1",
                                                                            "price_usd_knife_1",
                                                                            "price_usd_kettle_1",
                                                                            "price_usd_mosquito_net_1",
                                                                            "price_usd_solar_lamp_1",
                                                                            "price_usd_jerry_can_1"))

# education kit
education_kit <- descriptive_stats(df, group_var = "call_location", items = c("price_usd_exercise_book_1",
                                                                              "price_usd_pencil_1",
                                                                              "price_usd_sharpner_1",
                                                                              "price_usd_rubber_1",
                                                                              "price_usd_pens_1",
                                                                              "price_usd_ruler_1",
                                                                              "price_usd_math_set_1",
                                                                              "price_usd_crayons_1",
                                                                              "price_usd_bag_1"))

# supplier challenges
compute_pct <- function(var_name,df) {
  
  binaries <- df %>% select(starts_with(paste0(var_name,".")))
  names(binaries) <- gsub("^[^\\.]*\\.","",names(binaries))
  results <- tibble::rownames_to_column(round(colSums(binaries,na.rm = T) / nrow(binaries),digits = 2) %>% as.data.frame(), "option") 
  colnames(results)[2] <- "pct"
  results
}

barrier_transportation <- compute_pct("barrier_transportation", df)
barrier_security <- compute_pct("barrier_security", df)
barrier_nonsecurity <- compute_pct("barrier_nonsecurity", df)
barrier_season <- compute_pct("barrier_season", df)

# payment_type
payment_type <- compute_pct("payment_type", df)

# items_sold
items_sold <- compute_pct("items_sold", df)

# items_top
items_top <- compute_pct("items_top", df)

# supplier_currency
supplier_currency <- compute_pct("supplier_currency", df)

# credit_access
credit_access <- compute_pct("credit_access", df)

# barrier_financial
barrier_financial <- compute_pct("barrier_financial", df)

# gender_women
gender_women <- compute_pct("gender_women", df)

## Export
jmmi_analysis <- list("barrier_transportation" = barrier_transportation,
                      "barrier_security" = barrier_security,
                      "barrier_nonsecurity" = barrier_nonsecurity,
                      "barrier_season" = barrier_season,
                      "payment_type" = payment_type,
                      "items_sold" = items_sold,
                      "supplier_currency" = supplier_currency,
                      "credit_access" = credit_access,
                      "barrier_financial" = barrier_financial,
                      "gender_women" = gender_women)

write.xlsx(jmmi_analysis, paste0("output/analysis/Updated-Analysis_",today,".xlsx"))
## End of September update

# Create analysed file
OUT <- createWorkbook()

# Add sheets to the workbook
addWorksheet(OUT, paste0("items"))
addWorksheet(OUT, paste0("stock"))
addWorksheet(OUT, paste0("barrier"))
addWorksheet(OUT, paste0("payment"))
addWorksheet(OUT, paste0("supplier1"))
addWorksheet(OUT, paste0("supplier2"))
addWorksheet(OUT, paste0("credit1"))
addWorksheet(OUT, paste0("credit2"))
addWorksheet(OUT, paste0("currency_exchange")) 
addWorksheet(OUT, paste0("main_supplier"))
addWorksheet(OUT, paste0("supp_ch")) 
addWorksheet(OUT, paste0("supp_route"))
# addWorksheet(OUT, paste0("supply_chain1"))
# addWorksheet(OUT, paste0("supply_chain2"))

# Write data to the sheets
writeData(OUT, sheet = paste0("items"), x = pr_i)
writeData(OUT, sheet = paste0("stock"), x = st_i)
writeData(OUT, sheet = paste0("barrier"), x = ba_x)
writeData(OUT, sheet = paste0("payment"), x = pa_x)
writeData(OUT, sheet = paste0("supplier1"), x = su_x)
writeData(OUT, sheet = paste0("supplier2"), x = sn_x)
writeData(OUT, sheet = paste0("credit1"), x = cr_x)
writeData(OUT, sheet = paste0("credit2"), x = cn_x)
writeData(OUT, sheet = paste0("main_supplier"), x = main_supplier)
writeData(OUT, sheet = paste0("currency_exchange"), x = pct)
writeData(OUT, sheet = paste0("supp_ch"), x = supp_ch)
writeData(OUT, sheet = paste0("supp_route"), x = supp_route)
# writeData(OUT, sheet = paste0("supply_chain1"), x = or_x)
# writeData(OUT, sheet = paste0("supply_chain2"), x = or_i)

# Export the file
saveWorkbook(OUT, paste0("output/analysis//JMMI_2021FEB_analysed_", ll,"_",Sys.Date() , ".xlsx"), overwrite=TRUE)
