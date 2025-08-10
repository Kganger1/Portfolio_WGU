install.packages("stringr")
install.packages("readxl")  # For Excel files
install.packages("readr")   # For CSV files
install.packages("dplyr")   # For data manipulation
install.packages("tidyr")   # For data cleaning
library(stringr)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)

atd23 <- read_excel(
  "C:/Users/kiara/OneDrive/WGU MSDA/D214 Data Analytics Graduate Capstone/tariff database_202307.xlsx"
)
additional_tariffs <- read_excel(
  "C:/Users/kiara/OneDrive/WGU MSDA/D214 Data Analytics Graduate Capstone/china_plus_tariff.xlsx",
  sheet = 1
)


# View the first few rows of the data
head(additional_tariffs)
head(atd23)
# Check the column names
colnames(additional_tariffs)
# Rename the columns for easier reference
colnames(additional_tariffs) <- c("hts8", "chapter99_code")

# cleaning the atd tables
colnames(atd23)

# Clean and Calculate Equivalent Rates
atd23_temp <- atd23 %>%
  select(
    hts8,
    brief_description,
    mfn_text_rate,
    mfn_ad_val_rate,
    mfn_specific_rate,
    col1_special_text,
    mexico_ad_val_rate,
    mexico_specific_rate,
    nafta_mexico_ind,
    begin_effect_date,
    end_effective_date,
  )
View(atd23_temp)

# Remove periods from hts8 in additional_tariffs
additional_tariffs <- additional_tariffs %>%
  mutate(hts8 = gsub("\\.", "", hts8))
# Remove rows where hts8 starts with 98 or 99
atd23_temp <- atd23_temp %>%
  filter(!grepl("^98|^99", hts8))

# Merge additional tariffs with the main dataset
atd23_temp <- atd23_temp %>%
  left_join(additional_tariffs, by = "hts8")

# Apply additional tariffs based on chapter99_code to separate rates
atd23_temp <- atd23_temp %>%
  mutate(
    additional_tariff = case_when(
      chapter99_code == "9903.88.15" ~ 7.5,
      chapter99_code == "9903.88.03" ~ 25,
      TRUE ~ 0  # No additional tariff if not specified
    ),
    china_ad_val_rate = mfn_ad_val_rate * (1 + additional_tariff / 100),
    # Adjust ad valorem rates
    china_specific_rate = mfn_specific_rate * (1 + additional_tariff / 100)  # Adjust specific rates
  )

# Fill in missing Mexico rates based on col1_special_text
atd23_temp <- atd23_temp %>%
  mutate(
    # Handle empty or missing col1_special_text
    col1_special_text = ifelse(
      is.na(col1_special_text) | col1_special_text == "",
      "NA",
      col1_special_text
    ),
    # Fill mexico_ad_val_rate where "S" or "NA" is present in col1_special_text
    mexico_ad_val_rate = ifelse(
      is.na(mexico_ad_val_rate) &
        (grepl("\\bS\\b", col1_special_text, ignore.case = TRUE) | col1_special_text == "NA"),
      mfn_ad_val_rate,
      mexico_ad_val_rate
    ),
    # Fill mexico_specific_rate where "S" or "NA" is present in col1_special_text
    mexico_specific_rate = ifelse(
      is.na(mexico_specific_rate) &
        (grepl("\\bS\\b", col1_special_text, ignore.case = TRUE) | col1_special_text == "NA"),
      mfn_specific_rate,
      mexico_specific_rate
    )
  )
# Re-check the table for remaining NAs in Mexico rates
nas_in_mexico_rate <- atd23_temp %>%
  filter(is.na(mexico_ad_val_rate) | is.na(mexico_specific_rate))



sections <- list(
  "Section I: Live Animals; Animal Products" = c(1, 2, 3, 4, 5),
  "Section II: Vegetable Products" = c(6, 7, 8, 9, 10, 11, 12, 13, 14),
  "Section III: Animal or Vegetable Fats and Oils" = c(15),
  "Section IV: Foodstuffs, Beverages & Tobacco" = c(16, 17, 18, 19, 20, 21, 22, 23, 24),
  "Section V: Mineral Products" = c(25, 26, 27),
  "Section VI: Chemical & Allied Products" = c(28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38),
  "Section VII: Plastics & Rubber Goods" = c(39, 40),
  "Section VIII: Leather, Furs, and Handbags" = c(41, 42, 43),
  "Section IX: Wood, Cork, and Handmade Goods" = c(44, 45, 46),
  "Section X: Paper, Pulp, and Recyclables" = c(47, 48, 49),
  "Section XI: Textile and Textile Goods" = c(50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63),
  "Section XII: Footwear, Headgear, and Accessories" = c(64, 65, 66, 67),
  "Section XIII: Stone, Ceramic, and Glass Goods" = c(68, 69, 70),
  "Section XIV: Pearls, Precious Metals, and Jewelry" = c(71),
  "Section XV: Base Metals Goods" = c(72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83),
  "Section XVI: Machinery, Electronics, and Media Devices" = c(84, 85),
  "Section XVII: Vehicles, Aircraft, and Vessels" = c(86, 87, 88, 89),
  "Section XVIII: Instruments: Optical, Medical, Clocks, and Musical" = c(90, 91, 92),
  "Section XIX: Arms and Ammunition" = c(93),
  "Section XX: Home Goods, Toys, and Miscellaneous Articles" = c(94, 95, 96),
  "Section XXI: Art Work" = c(97)
)
map_section <- function(hts8) {
  hts_chapter <- as.numeric(substr(hts8, 1, 2))
  for (section in names(sections)) {
    if (hts_chapter %in% sections[[section]]) {
      return(section)
    }
  }
  return(NA) # Default for unmatched codes
}
atd23_clean <- atd23_temp %>%
  mutate(section = sapply(hts8, map_section))

section_summary <- atd23_clean %>%
  group_by(section) %>%
  summarize(
    avg_mexico_ad_val_rate = mean(mexico_ad_val_rate, na.rm = TRUE),
    avg_mexico_specific_rate = mean(mexico_specific_rate, na.rm = TRUE),
    avg_china_ad_val_rate = mean(china_ad_val_rate, na.rm = TRUE),
    avg_china_specific_rate = mean(china_specific_rate, na.rm = TRUE),
    .groups = 'drop'
  )
View(section_summary)

# Add Scenario Columns
atd23_clean <- atd23_clean %>%
  mutate(
    china_ad_val_rate_10 = china_ad_val_rate * 1.10,
    china_ad_val_rate_20 = china_ad_val_rate * 1.20,
    china_ad_val_rate_30 = china_ad_val_rate * 1.30,
    mexico_ad_val_rate_10 = mexico_ad_val_rate * 1.10,
    mexico_ad_val_rate_20 = mexico_ad_val_rate * 1.20,
    mexico_ad_val_rate_30 = mexico_ad_val_rate * 1.30,
    china_specific_rate_10 = china_specific_rate * 1.10,
    china_specific_rate_20 = china_specific_rate * 1.20,
    china_specific_rate_30 = china_specific_rate * 1.30,
    mexico_specific_rate_10 = mexico_specific_rate * 1.10,
    mexico_specific_rate_20 = mexico_specific_rate * 1.20,
    mexico_specific_rate_30 = mexico_specific_rate * 1.30
  )

# Update the Section Summary
section_summary <- atd23_clean %>%
  group_by(section) %>%
  summarize(
    avg_mexico_ad_val_rate = mean(mexico_ad_val_rate, na.rm = TRUE),
    avg_mexico_ad_val_rate_10 = mean(mexico_ad_val_rate_10, na.rm = TRUE),
    avg_mexico_ad_val_rate_20 = mean(mexico_ad_val_rate_20, na.rm = TRUE),
    avg_mexico_ad_val_rate_30 = mean(mexico_ad_val_rate_30, na.rm = TRUE),
    avg_china_ad_val_rate = mean(china_ad_val_rate, na.rm = TRUE),
    avg_china_ad_val_rate_10 = mean(china_ad_val_rate_10, na.rm = TRUE),
    avg_china_ad_val_rate_20 = mean(china_ad_val_rate_20, na.rm = TRUE),
    avg_china_ad_val_rate_30 = mean(china_ad_val_rate_30, na.rm = TRUE),
    avg_mexico_specific_rate = mean(mexico_specific_rate, na.rm = TRUE),
    avg_mexico_specific_rate_10 = mean(mexico_specific_rate_10, na.rm = TRUE),
    avg_mexico_specific_rate_20 = mean(mexico_specific_rate_20, na.rm = TRUE),
    avg_mexico_specific_rate_30 = mean(mexico_specific_rate_30, na.rm = TRUE),
    avg_china_specific_rate = mean(china_specific_rate, na.rm = TRUE),
    avg_china_specific_rate_10 = mean(china_specific_rate_10, na.rm = TRUE),
    avg_china_specific_rate_20 = mean(china_specific_rate_20, na.rm = TRUE),
    avg_china_specific_rate_30 = mean(china_specific_rate_30, na.rm = TRUE),
    .groups = 'drop'
  )

write.csv(atd23_clean, "C:/Users/kiara/OneDrive/WGU MSDA/D214 Data Analytics Graduate Capstone/atd23_clean_with_scenarios.csv", row.names = FALSE)

write.csv(section_summary, "C:/Users/kiara/OneDrive/WGU MSDA/D214 Data Analytics Graduate Capstone/section_summary_with_scenarios.csv", row.names = FALSE)

