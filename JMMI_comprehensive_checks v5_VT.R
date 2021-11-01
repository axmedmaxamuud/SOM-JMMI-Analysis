### PROJECT:  JMMI
### PURPOSE:  Check comprehensive assessments
### INPUT:    data.csv
### OUTPUT:   xlsx file
### AUTHOR:   Vanessa Topp
### LAST UPDATED: 06th June, 2021


# Packages
library(tidyverse)
library(openxlsx)
library(lubridate) #for some reason sometimes this has to be called manually
library(readxl)

# Clear Workspace
rm(list = ls())

# Make the timestamp more precise
op <- options(digits.secs = 6)
Sys.time()
date <- gsub(' ',"_",gsub(":","",paste0(Sys.time())))

 
name_macro <- 'input/aug_2021/jmmi_last.xlsx' # Name of the latest version of the macro
name_checks <- 'input/aug_2021/checks/checks_up-to-2021-08-29_104832.622644.xlsx' # Name of the checks to be integrated

data_raw <- read_excel(name_macro, sheet = "data_raw")
data_checked <- read_excel(name_macro, sheet = "data_checked")
data_log <- read_excel(name_macro, sheet = "data_log")
hash_check <- read_excel(name_macro, sheet = "hash_checks")

# Prepare source raw data as a starting point for checks 
data_raw <- as_tibble(data_raw) %>%
  mutate(across(starts_with("cva_effect_"), as.character)) %>%
  mutate(across(starts_with("spec_"), as.character)) %>%
  mutate(across(starts_with("price_usd_"), as.double)) %>%
  mutate(across(starts_with("price_confirm_"), as.character)) %>%
  mutate(across(starts_with("supplier_number"), as.double)) %>%
  mutate(across(starts_with("today"), as.Date)) %>%
  mutate(across(starts_with("username"), as.character)) %>%
  mutate(across(starts_with("deviceid"), as.character)) %>%
  mutate(across(starts_with("enumerator"), as.character)) %>%
  mutate(across(starts_with("vendor_contact_other"), as.double)) %>%
  mutate(across(starts_with("preferred_day"), as.character)) %>%
  
  mutate(across(where(is.integer), as.numeric))
data_names <- gsub("^_","X_", names(data_raw))
names(data_raw) <- data_names

# Prepare source checked data as a starting point for checks 
data_checked <- as_tibble(data_checked) %>%
  mutate(across(starts_with("cva_effect_"), as.character)) %>%
  mutate(across(starts_with("spec_"), as.character)) %>%
  mutate(across(starts_with("price_usd_"), as.double)) %>%
  mutate(across(starts_with("price_confirm_"), as.character)) %>%
  mutate(across(starts_with("supplier_number"), as.double)) %>%
  mutate(across(starts_with("today"), as.Date)) %>%
  mutate(across(starts_with("username"), as.character)) %>%
  mutate(across(starts_with("deviceid"), as.character)) %>%
  mutate(across(starts_with("enumerator"), as.character)) %>%
  mutate(across(starts_with("method"), as.character)) %>%
  mutate(across(starts_with("vendor_type"), as.character)) %>%
  mutate(across(starts_with("call_location"), as.character)) %>%
  mutate(across(starts_with("call_market"), as.character)) %>%
  mutate(across(starts_with("vendor_code"), as.character)) %>%
  mutate(across(starts_with("vendor_name"), as.character)) %>%
  mutate(across(starts_with("vendor_contact"), as.double)) %>%
  mutate(across(starts_with("preferred_day"), as.character)) %>%
  mutate(across(starts_with("vendor_location"), as.character)) %>%
  mutate(across(starts_with("note_vendor_name"), as.character)) %>%
  mutate(across(starts_with("note_vendor_location"), as.character)) %>%
  mutate(across(starts_with("consent"), as.character)) %>%
  mutate(across(starts_with("vendor_callback_name"), as.character)) %>%
  mutate(across(starts_with("vendor_contact_other"), as.double)) %>%
  mutate(across(starts_with("preferred_day"), as.character)) %>%
  mutate(across(where(is.integer), as.numeric))
data_names <- gsub("^_","X_", names(data_checked))
names(data_checked) <- data_names

# Prepare source log data as a starting point for checks 
data_log[] <- lapply(data_log, as.character)

# Move new entries from data raw to data checked
new_data <- data_raw %>%
  bind_rows(data_checked) %>%
  group_by(X_uuid) %>%
  filter(n()<2)
data_checked <- data_checked %>%
  bind_rows(new_data) 

# Read updated checks sent by FOs
check_update <- read_excel(name_checks, sheet = 'checks')


# Read location to FO match
loc_fo <- read.csv("input/aug_2021/location-fo.csv")

###--------------------MERGE--------------------


if (nrow(check_update) > 0) {
  for (a in 1:nrow(check_update)) {
    
    check_single <- check_update %>%
      slice(a)
    
    if (!is.na(check_single$corrected_value)) {
      
      #print(check_single$corrected_value)
      
      # Collect value to be replaced
      old_value <- data_checked[which(data_checked$X_uuid == check_single$X_uuid),check_single$field]
      
      # Modify integer values
      if (check_single$issue == 'PRICE soft outlier' || check_single$issue == 'PRICE hard outlier' || check_single$issue == 'absent number of suppliers' || check_single$issue == 'PRICE "other"' || check_single$issue == 'atypical number of suppliers') {
        
        # If the price needs to be deleted, then write NA
        if (as.character(check_single$corrected_value) == 'DELETE') {
          
          # Modify clean data in the macro
          data_checked[which(data_checked$X_uuid == check_single$X_uuid),check_single$field] <- NA
          
          # Or else just write the value  
        } else {
          
          print(as.double(check_single$corrected_value))
          
          # Modify clean data in the macro
          data_checked[which(data_checked$X_uuid == check_single$X_uuid),check_single$field] <- as.double(check_single$corrected_value)
          
        }
        
        # If soft outlier is checked, we can stop checking
        if (check_single$issue == 'PRICE soft outlier') {
          
          # Add hash_check to list of checked, so they don't need to be re-checked
          hash_check <- hash_check %>%
            add_row(
              tibble_row(
                HASH_CHECK = paste0(check_single$X_uuid, check_single$field)
              )
            )
          
        }
        
        # Modify string values  
      } else {
        
        # Modify clean data in the macro
        data_checked[which(data_checked$X_uuid == check_single$X_uuid),check_single$field] <- as.character(check_single$corrected_value)
        
        # Add hash_check to list of checked, so they don't need to be re-checked
        hash_check <- hash_check %>%
          add_row(
            tibble_row(
              HASH_CHECK = paste0(check_single$X_uuid, check_single$field)
            )
          )
        
      }
      
      # Modify logs data in the macro
      data_log <- data_log %>%
        add_row(
          tibble_row(
            ID = as.character(a),
            Question = check_single$field,
            Enumerator = check_single$enumerator,
            uuid = check_single$X_uuid,
            Community = check_single$call_location,
            `Old Value` = as.character(old_value),
            `New Value` = as.character(check_single$corrected_value),
            Reason = check_single$issue,
            `Modified by?` = check_single$fo
          )
        )
      
    }
  }
}

# Write MACRO
OUT <- createWorkbook()
# Add some sheets to the workbook
addWorksheet(OUT, paste0('data_raw'))
addWorksheet(OUT, paste0('data_checked'))
addWorksheet(OUT, paste0('data_log'))
addWorksheet(OUT, paste0('hash_check'))
# Write the data to the sheets
writeData(OUT, sheet = paste0('data_raw'), x = data_raw)
writeData(OUT, sheet = paste0('data_checked'), x = data_checked)
writeData(OUT, sheet = paste0('data_log'), x = data_log)
writeData(OUT, sheet = paste0('hash_check'), x = hash_check)

# Export the file
saveWorkbook(OUT, paste0('input/aug_2021/data-PROCESSED-', date ,'.xlsx'), overwrite = TRUE)

###--------------------CHECKS--------------------

# Just a lazy shortcut to renaming
data <- data_checked

# ### Upload and prepare data
# data <- read.csv("days1-2.csv", stringsAsFactors = FALSE, header = T) %>%
#   mutate(across(starts_with("spec_"), as.character))
# data_base <- data
# # Make tibble
# data <- as_tibble(data)


# Create empty kobo df
checks <- data.frame(
  "X_uuid" = character(),
  #"enumerator" = character(),
  #"contact" = character(),
  #"partner" = character(),
  #"location" = character(),
  "issue" = character(),
  "field" = character(),
  "value" = character(),
  "corrected_value" = character(),
  "comments_Tie" = character(),
  "expected" = character()
)
checks <- as_tibble(checks)


# Remove duplicates
#data <- unique(data$vendor_contact)
#data <- data[!duplicated(data$jmmi), ]
# Remove non-related items
#data <- data %>%
#filter(!str_detect(items_sold, "none"))

## Number of items sold --
nis <- data %>%
  select(starts_with("items_sold."), X_uuid) %>%
  #adds all columns starting with items_sold.
  pivot_longer(
    # select columns
    cols = starts_with("items_sold."),
    # column names go to a new column called items
    names_to = "items",
    # deletes the prefix from new column
    names_prefix = "items_sold.",
    # values of these columns go to a new column called n_items_sold
    values_to = "n_items_sold",
    values_drop_na = TRUE
  ) %>%
  group_by(X_uuid) %>%
  summarise(CHECK_nis = sum(as.double(n_items_sold)))

alt <- nis %>%
  ungroup() %>%
  summarise(alt = sum(CHECK_nis))

## Check duration --
dur <- data %>%
  select(start, end, X_uuid) %>%
  # merge with number of items sold table
  left_join(nis, by = "X_uuid") %>%
  # format times
  mutate(start_new = ymd_hms(convertToDateTime(start)), .after = start) %>%
  mutate(end_new = ymd_hms(convertToDateTime(end)), .after = end) %>%
  mutate(duration_s = int_length(interval(start_new, end_new))) %>%
  mutate(duration = hms::as_hms(duration_s)) %>%
  # add check, based on a minimum time + a fixed time per number of items
  # if TRUE, then it is too short
  mutate(CHECK_duration = duration_s < 675 + 84 * CHECK_nis) %>%
  mutate(dur_med = 675 + 84 * CHECK_nis)

## Get median
median <- dur %>%
  select(duration_s) %>%
  summarise(median = median(duration_s))
median <- as.double(round(median[1, ]))
median <- hms::as_hms(median)
#as.double(duration_median_s)

dur <- dur %>%
  select(X_uuid, duration, duration_s, CHECK_duration, CHECK_nis, dur_med) %>%
  filter(CHECK_duration == TRUE)

## Add to checklist --
if (nrow(dur) > 0) {
  for (a in 1:nrow(dur)) {
    checks <- checks %>%
      add_row(
        tibble_row(
          X_uuid = as.character(dur[a, "X_uuid"]),
          issue = "duration",
          field = "start + end",
          value = as.character(hms::as_hms(as.double(dur[a, "duration_s"]))),
          expected = as.character(hms::as_hms(as.double(dur[a, "dur_med"])))
        )
      )
  }
}

## Duplicate vendor codes --
dup <- data %>%
  select(X_uuid, vendor_code) %>%
  mutate(CHECK_duplicate = duplicated(vendor_code)) %>%
  filter(CHECK_duplicate == TRUE)
## Add to checklist --
if (nrow(dup) > 0) {
  for (a in 1:nrow(dup)) {
    checks <- checks %>%
      add_row(
        tibble_row(
          X_uuid = as.character(dup[a, "X_uuid"]),
          issue = "duplicate vendor code",
          field = "vendor_code",
          value = as.character(dup[a, "vendor_code"])
        )
      )
  }
}

# ## Distinct locations --
# loc <- data %>%
#   select(X_uuid, call_location, vendor_location) %>%
#   mutate(CHECK_location = call_location==vendor_location) %>%
#   filter(CHECK_location == FALSE)
# ## Add to checklist --
# if (nrow(loc) > 0) {
#   for (a in 1:nrow(loc)) {
#     checks <- checks %>%
#       add_row(
#         tibble_row(
#           X_uuid = as.character(loc[a, "X_uuid"]),
#           issue = "distinct locations",
#           field = "call_location + vendor_location",
#           value = paste(as.character(loc[a, "call_location"]),as.character(loc[a, "vendor_location"]))
#         )
#       )
#   }
# }

# ## Wrong market --
# var <- data %>%
#   select(X_uuid, profile_market) %>%
#   filter(profile_market != "yes")
# ## Add to checklist --
# if (nrow(var) > 0) {
#   for (a in 1:nrow(var)) {
#     checks <- checks %>%
#       add_row(
#         tibble_row(
#           X_uuid = as.character(var[a, "X_uuid"]),
#           issue = "wrong market",
#           field = "call_market",
#           value = as.character(var[a, "profile_market"])
#         )
#       )
#   }
# }

## Not a retailer --
var <- data %>%
  select(X_uuid, profile_type) %>%
  filter(profile_type != "retail")
## Add to checklist --
if (nrow(var) > 0) {
  for (a in 1:nrow(var)) {
    checks <- checks %>%
      add_row(
        tibble_row(
          X_uuid = as.character(var[a, "X_uuid"]),
          issue = "vendor might not be a retailer",
          field = "profile_type",
          value = as.character(var[a, "profile_type"])
        )
      )
  }
}

## "Other" translations --
var <- data %>%
  #adds all columns ending with _other, then selects out price and currency
  select(ends_with("_other"), X_uuid) %>%
  select(!starts_with("price")) %>%
  select(!starts_with("currency")) %>%
  select(!starts_with("note_vendor_contact_")) %>%
  select(!starts_with("vendor_callback_")) %>%
  mutate_if(is.double, as.character) %>%
  pivot_longer(
    # select columns
    cols = ends_with("_other"),
    # column names go to a new column called questions
    names_to = "questions",
    # values of these columns go to a new column called value
    values_to = "value", #values_ptypes = list(value="character"),
    values_drop_na = TRUE
  ) %>%
  filter(value!="")
## Add to checklist --
if (nrow(var) > 0) {
  for (a in 1:nrow(var)) {
    checks <- checks %>%
      add_row(
        tibble_row(
          X_uuid = as.character(var[a, "X_uuid"]),
          issue = "check/translate OTHER",
          field = as.character(var[a, "questions"]),
          value = as.character(var[a, "value"])
        )
      )
  }
}

## Number of suppliers out of median --
var <- data %>%
  select(X_uuid, supplier_number)
# Get median
median <- var %>%
  select(supplier_number) %>%
  summarise(median = median(supplier_number, na.rm = TRUE))
median <- as.double(round(median[1, ]))
# Filter outliers
var <- var %>%
  filter(supplier_number > median*3 | supplier_number < median/3)
## Add to checklist --
if (nrow(var) > 0) {
  for (a in 1:nrow(var)) {
    checks <- checks %>%
      add_row(
        tibble_row(
          X_uuid = as.character(var[a, "X_uuid"]),
          issue = "atypical number of suppliers",
          field = "supplier_number",
          value = as.character(var[a, "supplier_number"])
        )
      )
  }
}

## Number of suppliers is absent --
var <- data %>%
  select(X_uuid, supplier_number) %>%
  filter(is.na(supplier_number))
## Add to checklist --
if (nrow(var) > 0) {
  for (a in 1:nrow(var)) {
    checks <- checks %>%
      add_row(
        tibble_row(
          X_uuid = as.character(var[a, "X_uuid"]),
          issue = "absent number of suppliers",
          field = "supplier_number",
          value = as.character(var[a, "supplier_number"])
        )
      )
  }
}

## PRICES BY DISTRIBUTION--
var <- data %>%
  #adds all columns starting with price_usd_
  select(starts_with("price_usd_"), X_uuid) %>%
  select(!ends_with("_other")) %>%
  pivot_longer(
    # select columns
    cols = starts_with("price_usd_"),
    # column names go to a new column called items
    names_to = "items",
    # values of these columns go to a new column called value
    values_to = "value", #values_ptypes = list(value="integer"),
    values_drop_na = TRUE
  ) %>%
  filter(value!="")
# Get median prices
median <- var %>%
  group_by(items) %>%
  summarise(median = median(value, na.rm = TRUE))
#median <- as.double(round(median[1, ]))
var <- var %>%
  left_join(median, by = "items") %>%
  # Filter outliers
  filter(value > median*3 | value < median/3)
## Add to checklist --
if (nrow(var) > 0) {
  for (a in 1:nrow(var)) {
    checks <- checks %>%
      add_row(
        tibble_row(
          X_uuid = as.character(var[a, "X_uuid"]),
          issue = "PRICE hard outlier",
          field = as.character(var[a, "items"]),
          value = as.character(var[a, "value"]),
          expected = as.character(var[a, "median"])
        )
      )
  }
}

## PRICES BY TOOL FLAGGING--
var <- data %>%
  #adds all columns starting with price_usd_
  select(starts_with("price_usd_"), X_uuid) %>%
  select(!ends_with("_other")) %>%
  pivot_longer(
    # select columns
    cols = starts_with("price_usd_"),
    # column names go to a new column called items
    names_to = "items",
    # values of these columns go to a new column called value
    values_to = "value" #values_ptypes = list(value="integer"),
  ) 

var0 <- data %>%
  #adds all columns starting with price_usd_
  select(starts_with("price_confirm_"), X_uuid) %>%
  select(!ends_with("_other")) %>%
  pivot_longer(
    # select columns
    cols = starts_with("price_confirm_"),
    # column names go to a new column called items
    names_to = "items",
    # values of these columns go to a new column called value
    values_to = "confirm" #values_ptypes = list(value="integer"),
  ) %>%
  select(confirm)

var <- var %>%
  bind_cols(var0) %>%
  filter(confirm == 'OK') %>%
  left_join(median, by = "items") 

## Add to checklist --
if (nrow(var) > 0) {
  for (a in 1:nrow(var)) {
    checks <- checks %>%
      add_row(
        tibble_row(
          X_uuid = as.character(var[a, "X_uuid"]),
          issue = "PRICE soft outlier",
          field = as.character(var[a, "items"]),
          value = as.character(var[a, "value"]),
          expected = as.character(var[a, "median"])
        )
      )
  }
}


## prices OTHER --
var <- data %>%
  #adds all columns starting with price_usd_
  select(starts_with("price_usd_"), X_uuid) %>%
  select(ends_with("_other"), X_uuid) %>%
  pivot_longer(
    # select columns
    cols = starts_with("price_usd_"),
    # column names go to a new column called items
    names_to = "items",
    # values of these columns go to a new column called value
    values_to = "value", #values_ptypes = list(value="integer"),
    values_drop_na = TRUE
  ) %>%
  filter(value!="")
## Add to checklist --
if (nrow(var) > 0) {
  for (a in 1:nrow(var)) {
    checks <- checks %>%
      add_row(
        tibble_row(
          X_uuid = as.character(var[a, "X_uuid"]),
          issue = "PRICE \"other\"",
          field = as.character(var[a, "items"]),
          value = as.character(var[a, "value"]),
        )
      )
  }
}

## Add info to checks (end) --
info <- data %>%
  select(X_uuid,
         enumerator,
         enumerator_phone,
         enumerator_org,
         call_location,
         vendor_type)
checks <- checks %>%
  # merge with number of items sold table
  left_join(info, by = "X_uuid") %>%
  mutate(HASH_CHECK = paste0(X_uuid,field), .after = field) %>%
  # order by location, org, enumerator
  arrange(call_location,enumerator_org,enumerator,X_uuid) %>%
  # Add FO
  left_join(loc_fo, by = "call_location")


###--------------------WRITE--------------------



# Write CHECKS
OUT <- createWorkbook()
# Add some sheets to the workbook
addWorksheet(OUT, paste0('checks'))
# Write the data to the sheets
writeData(OUT,
          sheet = paste0('checks'),
          x = checks)

# Export the file
saveWorkbook(OUT, paste0('input/aug_2021/checks/checks_up-to-', date , '.xlsx'), overwrite = TRUE)




