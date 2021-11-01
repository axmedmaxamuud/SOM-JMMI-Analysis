## JMMI Analysis Script - Update
## 22/09/2021
rm(list = ls())
today <- Sys.Date()

# load required packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(reshape2)
library(srvyr)
library(questionr)

# source functions
source("functions/reach_excel_format.R")

# load clean data
df <- rio::import("output/clean_data/SOM2003_JMMI_Clean_Data_0821 v2.csv", na=c(""))
names(df) <- gsub("\\/",".",names(df))

## Analysis of Select Multiple Questions
compute_pct <- function(var_name,df) {
  
  binaries <- df %>% select(starts_with(paste0(var_name,"."))) %>% na.omit(FALSE)
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

## Item Kits Median Calculation
median_calculation <- function(df, price_list, aggregation){
  
  y <- df %>% select(aggregation, price_list) %>% group_by(!!sym(aggregation)) %>% summarise_all(funs(median(., na.rm = TRUE)))
  print(y)
  
}

# wash kit
wash_kit <- median_calculation(df, price_list = c("price_usd_soap_1",
                                                  "price_usd_washing_powder_1",
                                                  "price_usd_mhm_1",
                                                  "price_usd_bucket_1",
                                                  "price_usd_jerry_can_3"),
                               aggregation = "call_location") %>% 
  mutate(meb_soap = price_usd_soap_1 * 6.67,
         meb_washing_powder = price_usd_washing_powder_1 * 3,
         meb_mhm = price_usd_mhm_1 * 2,
         meb_bucket = price_usd_bucket_1 * 1,
         meb_jerry_cann = price_usd_jerry_can_3 * 1,
         meb_total = meb_soap + meb_washing_powder + meb_mhm + meb_bucket + meb_jerry_cann) %>% na.omit(TRUE) %>% 
  select(1, starts_with("meb_"))

# water kit
water_kit <- median_calculation(df, price_list = c("price_usd_water_communal_1",
                                                   "price_usd_water_piped_1",
                                                   "price_usd_water_truck_1"),
                                aggregation = "call_location") %>% 
  mutate(meb_water_communal = price_usd_water_communal_1 * 139.5,
         meb_water_piped = price_usd_water_piped_1 * 2.79,
         meb_water_truck = price_usd_water_truck_1 * 2.79,
         meb_total = meb_water_communal + meb_water_piped + meb_water_truck) %>% na.omit(TRUE) %>% 
  select(1, starts_with("meb_"))

# shelter kit
shelter_kit <- median_calculation(df, price_list = c("price_usd_plastic_sheet_1",
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
                                                     "price_usd_jerry_can_1"),
                                  aggregation = "call_location") %>% 
  mutate(meb_plastic_sheet = price_usd_plastic_sheet_1 * 1,
         meb_blanket = price_usd_blanket_1 * 3,
         meb_sleeping_mat = price_usd_sleeping_mat_1 * 2,
         meb_cooking_pot_1 = price_usd_cooking_pot_1 * 1,
         meb_cooking_pot_2 = price_usd_cooking_pot_2 * 1,
         meb_plate = price_usd_plate_1 * 5,
         meb_bowl = price_usd_bowl_1 * 5,
         meb_cup = price_usd_cup_1 * 5,
         meb_table_spoon = price_usd_spoon_1 * 5,
         meb_serving_spoon = price_usd_serving_spoon_1 * 1,
         meb_knife = price_usd_knife_1 * 1,
         meb_kettle = price_usd_kettle_1 * 1,
         meb_mosquito_net = price_usd_mosquito_net_1 * 1,
         meb_solar_lamp = price_usd_solar_lamp_1 * 1,
         meb_jerrycan = price_usd_jerry_can_1 * 2,
         meb_total = meb_plastic_sheet+meb_blanket+meb_sleeping_mat+meb_cooking_pot_1+
           meb_cooking_pot_2+meb_plate+meb_bowl+meb_cup+meb_table_spoon+meb_serving_spoon+
           meb_knife+meb_kettle+meb_mosquito_net+meb_solar_lamp+meb_jerrycan) %>% na.omit(TRUE) %>% 
  select(1, starts_with("meb_"))

# education kit
education_kit <- median_calculation(df, price_list = c("price_usd_exercise_book_1",
                                                       "price_usd_pencil_1",
                                                       "price_usd_sharpner_1",
                                                       "price_usd_rubber_1",
                                                       "price_usd_pens_1",
                                                       "price_usd_ruler_1",
                                                       "price_usd_math_set_1",
                                                       "price_usd_crayons_1",
                                                       "price_usd_bag_1"),
                                    aggregation = "call_location") %>% 
  mutate(meb_exercise_books = price_usd_exercise_book_1 * 6,
         meb_pencils = price_usd_pencil_1 * 4,
         meb_pencil_sharpener = price_usd_sharpner_1 * 1,
         meb_rubbers = price_usd_rubber_1 * 4,
         meb_pens = price_usd_pens_1 * 2,
         meb_ruller = price_usd_ruler_1 * 1,
         meb_mathset = price_usd_math_set_1 * 1,
         meb_crayons = price_usd_crayons_1 * 1,
         meb_bag = price_usd_bag_1 * 1) %>% na.omit(TRUE) %>% 
  select(1, starts_with("meb_"))

## Univariate Analysis
# Payment type and exchange rate analyses
payments <- df %>% group_by(currency_main) %>% 
  summarise(Freq = n()) %>% mutate(Prop = Freq / sum(Freq), paste0(round(Prop * 100), "%"))

# supplier route per destination 
#supp_dist_route <- table(df$supplier_destination, df$supplier_district,df$supplier_route) %>% 
#  ftable() %>% prop.table()
#supplier_route_distination <- data.frame(expand.grid(rev(attr(supp_dist_route, "row.vars"))), unclass(supp_dist_route))

supp_dist_route <- df 
# group banadir districts into one
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_boondheere"] <- "mogadishu"
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_daynile"] <- "mogadishu"
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_dharkenley"] <- "mogadishu"
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_hawl_wadaag"] <- "mogadishu"
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_hodan"] <- "mogadishu"
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_karaan"] <- "mogadishu"
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_shangaani"] <- "mogadishu"
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_waaberi"] <- "mogadishu"
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_wadajir"] <- "mogadishu"
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_wardhiigleey"] <- "mogadishu"
supp_dist_route$supplier_district[supp_dist_route$supplier_district == "mogadishu_xamar_jabjab"] <- "mogadishu"

supply_route <- table(supp_dist_route$supplier_destination, supp_dist_route$supplier_district, supp_dist_route$supplier_route) %>% 
  ftable() %>% prop.table()
supplier_route_distination <- data.frame(expand.grid(rev(attr(supply_route, "row.vars"))), unclass(supply_route))

## Ouput
jmmi_analysis <- list("barrier_transportation" = barrier_transportation,
                      "barrier_security" = barrier_security,
                      "barrier_nonsecurity" = barrier_nonsecurity,
                      "barrier_season" = barrier_season,
                      "payments" = payments,
                      "items_sold" = items_sold,
                      "supplier_currency" = supplier_currency,
                      "credit_access" = credit_access,
                      "barrier_financial" = barrier_financial,
                      "gender_women" = gender_women,
                      "supplier_route_distination" = supplier_route_distination,
                      "wash_kit" = wash_kit,
                      "water_kit" = water_kit,
                      "shelter_kit" = shelter_kit,
                      "education_kit" = education_kit)

write_excel_as_reach_format(jmmi_analysis, output_path = "output/analysis/extended-analysis.xlsx")
#write.xlsx(jmmi_analysis, paste0("output/analysis/Updated-Analysis_",today,".xlsx"))
