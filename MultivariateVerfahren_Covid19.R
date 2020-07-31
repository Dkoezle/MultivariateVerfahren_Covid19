library(readr)
library(plyr)
library(dplyr)
library(janitor)
library(data.table)
library(ggplot2)
library(maps)
library(zoo)
library(lubridate)


get_data <- function() {
  #to do: source all relevant online sources for newest available data.
  
  ## ---- Case number datasets ---- ####
  # Our World In Data: case numbers and demographics by country (timeseries)
  OWID_covid_data <<- read_csv("data/case_numbers/OWID_covid_data.csv",
                              col_types = cols(date = col_date(format = "%Y-%m-%d"), extreme_poverty = col_double(), 
                                               new_tests = col_double(), new_tests_per_thousand = col_double(), 
                                               new_tests_smoothed = col_double(), new_tests_smoothed_per_thousand = col_double(), 
                                               tests_units = col_factor(), total_tests = col_double(), 
                                               total_tests_per_thousand = col_double(), location=col_factor())) %>% clean_names()
  
  ## ---- Mobility datasets ---- ####
  
  # Apple mobility: use of transportation (relative to baseline)
  mobility_transportation <<- read_csv("data/mobility_data/apple_mobility_trends/applemobilitytrends-2020-07-19.csv", 
                                      col_types = cols(country = col_character(), `sub-region` = col_character()))
  
  # Google mobility: time spent at public places (relative to baseline)
  mobility_public <<- read_csv("data/mobility_data/google_mobility/Global_Mobility_Report.csv", 
                              col_types = cols(census_fips_code = col_double(), date = col_date(format = "%Y-%m-%d"), 
                                               iso_3166_2_code = col_character(), sub_region_1 = col_character(),
                                               sub_region_2 = col_character())) %>% clean_names()
  
  
  ## ---- Demographic datasets ---- ####
  # Worldbank
  health_systems <<- read_csv("data/demographics/Health_systems.csv", na = c("", "NA", "NaN")) %>%
    clean_names() %>% as.data.table()
  
  
  ## ---- government measurements ---- ####
  # ACAPS: categorized government measures by country (timeseries)
  gov_policies <<- read_delim("data/gov_measurements/acaps_covid19_government_measures_dataset.csv", ";", escape_double = FALSE,
                             col_types = cols(DATE_IMPLEMENTED = col_date(format = "%d.%m.%Y"), PCODE = col_double(),
                                              LOG_TYPE = col_factor(), CATEGORY = col_factor()), trim_ws = TRUE) %>% 
    select(-starts_with("X")) %>% 
    clean_names()
  
  # # Humanitarian Data Exchange: school closures (bereits in ACAPS dataset enthalten)
  # school_closures <- read_csv("data/gov_measurements/covid_impact_education.csv",
  #                             col_types = cols(Date = col_date(format = "%d/%m/%Y"))) %>% clean_names()
  # 
  
}


merge_data <- function(){
  
  ## manipulate mobility_transportation -------------
  
    #to do: identify common, misspelled and missing countries in owid and transportation dataset. then filter.
    
    # exclude data for states/counties/cities (only include country data)
    mob_transport_reduced <- as.data.table(mobility_transportation)[geo_type=="country/region",]
    
    # identify countries in mob_transp dataset that are in owid dataset
    mob_transport_reduced$region[!mob_transport_reduced$region %in% OWID_covid_data$location] %>% unique()
    
    # identify countries in owid dataset that are in mob_transp dataset
    OWID_covid_data$location[!OWID_covid_data$location %in% mob_transport_reduced$region] %>% unique()
    
    # recode county names in cases of different country spelling
    mob_transport_reduced <- mob_transport_reduced[region=="Republic of Korea", region := "South Korea"][order(region)]
    
    # melt date columns into single column
    mob_transport_melted <- melt(mob_transport_reduced, id.vars = c("region", "transportation_type"),measure.vars = patterns("\\d{4}-\\d{2}-\\d{2}"),
                                 variable.name="date", variable.factor = F, value.name = "relative_change_to_baseline")[, date := ymd(date)]
    
    # reshape transportation column into multiple columns
    mob_transport_reshaped <- dcast(mob_transport_melted, formula = region+date ~ transportation_type,
                                    value.var = "relative_change_to_baseline")
    
    # rename column names
    old_names <- names(mob_transport_reshaped)[-c(1:2)]
    mob_transport_reshaped <- setnames(mob_transport_reshaped, old_names, paste0(old_names, '_base_change'))
    
    
    # add columns with 4d change
    mob_transport_final <- mob_transport_reshaped
    mob_transport_final[, ':=' (driving_4d_change = rollmean(driving_base_change, 4, fill = NA, align = "right"),
                                transit_4d_change = rollmean(transit_base_change, 4, fill = NA, align = "right"),
                                walking_4d_change = rollmean(walking_base_change, 4, fill = NA, align = "right"))]
    
    # subtract 100 from values such that 0 is base value
    for (col in names(mob_transport_final)[-c(1:2)]){
      mob_transport_final[, (col) := get(col)-100]
    }
    
    
  ## manipulate public places data ---------------------- 
    
    mob_public_reduced <- mobility_public %>%
      filter(is.na(sub_region_1)) %>%
      select(-c(1,3,4,5,6))
      
  ## manipulate health data ----------------------------
    
    health_systems$country_region[!(health_systems$country_region %in% OWID_covid_data$location)] %>% unique()
    OWID_covid_data$location[!(OWID_covid_data$location %in% health_systems$country_region)] %>% unique()
    
    
    health_reduced <- health_systems[, country_region := as.factor(recode(country_region, "Czechia" = "Czech Republic","US" = "United States",
                                              "Korea, South" = "South Korea", "North Macedonia" = "Macedonia"))
                   ][(!is.na(country_region)) & is.na(province_state),][,c(1,4,5,6,10)]
    
    
    
  ## manipulate gov policy dataset -------------------
    
    policies_reshaped <- gov_policies %>% 
      select(iso, log_type, category, measure, comments, date_implemented) %>%
      filter(is.Date(date_implemented), !is.na(date_implemented), grepl("^[A-Z]{3}$", iso)) %>%
      as.data.table() %>%
      .[log_type=="Introduction / extension of measures", log_type := "introduction"] %>% 
      .[log_type=="Phase-out measure", log_type := "phase_out"] %>% 
      dcast(iso + date_implemented ~ log_type + category, fun.aggregate=length) %>% 
      clean_names()
    
      
    
    policies_counted <- policies_reshaped %>% .[,c(c(1,2),(ncol(.)-5):ncol(.))]
    
    
    full_corona_dataset <<- OWID_covid_data %>% 
      filter(grepl("^[A-Z]{3}$", iso_code)) %>%
      rename(country = location) %>%
      select(-continent) %>% as.data.table() %>% 
      merge(mob_transport_final, by.x=c("country", "date"), by.y=c("region", "date"), all.x = T) %>% 
      merge(mob_public_reduced, by.x = c("country", "date"), by.y = c("country_region", "date"), all.x = T) %>% 
      merge(health_reduced, by.x="country", by.y="country_region", all.x = T) %>% 
      merge(policies_reshaped, by.x=c("iso_code", "date"), by.y = c("iso", "date_implemented"), all.x = T) %>% 
      group_by(iso_code) %>%
      mutate(pubhealth_measures_count = cumsum(introduction_public_health_measures) - phase_out_public_health_measures,
             socioeconomic_measures_count = cumsum(introduction_governance_and_socio_economic_measures) - phase_out_governance_and_socio_economic_measures,
             socdistancing_measures_count = cumsum(introduction_social_distancing) - phase_out_social_distancing,
             movement_restriction_count = cumsum(introduction_movement_restrictions) - phase_out_movement_restrictions,
             lockdown_measure_count = cumsum(introduction_lockdown) - phase_out_lockdown,
             humanitarian_measure_count = cumsum(introduction_humanitarian_exemption) - phase_out_humanitarian_exemption) %>%
      ungroup() %>% 
      select(-c(introduction_public_health_measures, phase_out_public_health_measures, introduction_governance_and_socio_economic_measures,
                phase_out_governance_and_socio_economic_measures, introduction_social_distancing, phase_out_social_distancing,
                introduction_movement_restrictions, phase_out_movement_restrictions, introduction_lockdown, phase_out_lockdown,
                introduction_humanitarian_exemption, phase_out_humanitarian_exemption))

    
    
  
    
    
}


check_data <- function(){
  
  # Check OWID data -------------------
  
  print("Checking OWID dataset...")
  
  # filter countries: first case at least 120 days ago and minimum 100 total cases
  owid_analysis <- OWID_covid_data %>%
    group_by(location) %>% 
    mutate(number_of_days = n(),last_total = max(total_cases)) %>%
    ungroup() %>% 
    filter(total_cases > 0, iso_code !="OWID_WRL", number_of_days > 120, last_total > 100) %>% 
    group_by(location) %>% 
    mutate(first_case_date = min(date),last_date = max(date)) %>% 
    ungroup()
  
  # aggregate data (averages) by country
  owid_averages <- owid_analysis %>% select(-c("iso_code", "continent", "date", "tests_units")) %>% 
    group_by(location) %>% 
    summarize_all(mean, na.rm=TRUE) %>% 
    arrange(first_case_date)
  
  n_country_firstfilter <- owid_averages %>% nrow()
  earliest_date_firstfilter <- min(owid_averages$first_case_date)
  latest_date_firstfilter <- max(owid_averages$last_date)
  
  print(paste0("Frühester Zeitstempel in OWID-covid19: ", earliest_date_firstfilter))
  print(paste0("Letzter Zeitstempel in OWID-covid19: ", latest_date_firstfilter))
  print(paste0("Anzahl Länder mit >100 Fällen und erster Fall vor >120 Tagen: ",
               n_country_firstfilter))
  
  
  # identify countries with (without) reported tests
  owid_reduced_country_vector <- owid_averages %>% filter(!is.na(total_tests), ) %>% .[['location']] %>% 
    droplevels() %>% unique()
  
  # identify countries with clear (unclear) tests units
  owid_reduced_country_vector_testunits <- owid_analysis %>%
    filter(location %in% owid_reduced_country_vector,tests_units %in% c("tests performed", "people tested")) %>%
    .[['location']] %>% droplevels() %>% unique()
  
  # filter: exclude countries without reported tests or unclear tests_units
  owid_reduced <<- owid_analysis %>% filter(location %in% (owid_reduced_country_vector_testunits))
 
   
  print(paste0("Anzahl Länder mit zusätzlich Testeinheit in [Anzahl Test] oder [Personen getestet]: ",
               length(owid_reduced_country_vector_testunits)))
  
  # plot reduced_country vector on world map
  WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify %>% 
    mutate(region = recode(region, USA = "United States", UK = "United Kingdom"))
  
  ggplot() +
    geom_map(data = WorldData, map = WorldData, aes(x = long, y = lat, group = group, map_id=region),
             fill = "white", colour = "#7f7f7f", size = 0.5) +
    geom_map(data=data.frame("country"=owid_reduced_country_vector_testunits,"included"=1), map=WorldData, aes(fill=included, map_id=country),
             color ="#7f7f7f") +
    scale_y_continuous(breaks=c()) +
    scale_x_continuous(breaks=c()) +
    labs(title=paste0(length(owid_reduced_country_vector_testunits)," included countries from owid dataset after filters"),
         fill="") +
    theme(legend.position = "none")
  
  print("Länder, die wegen unklaren Testeinheiten rausfallen: ")
  print(setdiff(owid_reduced_country_vector, owid_reduced_country_vector_testunits))
  print("-----------------------------------------------------")
  
  # Data availability country level:
  print("Datenvferfügbarkeit auf Länderebene:")
  owid_reduced %>%
    select(-c("iso_code", "continent", "date", "tests_units")) %>% 
    group_by(location) %>% 
    summarize_all(mean, na.rm=TRUE) %>% 
    ungroup() %>% 
    is.na() %>% 
    colSums() %>% 
    as.data.frame() %>% 
    setnames(old=".", new="Missing_for_x_countries") %>% print()
  print("--------------------------------------------------")
  
  # Data availability daily level:
  print("Datenverfügbarkeit auf Tagesebene:")
  print(paste0("Anzahl Beobachtungen im gefilterten Datensatz: ", nrow(owid_reduced)))
  
  owid_reduced %>%
    select(-c("iso_code", "continent", "date", "tests_units")) %>% 
    is.na() %>% 
    colSums() %>% 
    as.data.frame() %>% 
    setnames(old=".", new="Missing_for_x_observations") %>% print()
  print("--------------------------------------------------")
  
  # identify date of first reported tests
  test_date_vector <- owid_reduced %>% group_by(location) %>% 
    filter(total_tests>0) %>%
    mutate(first_test_date = min(date)) %>% 
    summarise_all(mean) %>% .[['first_test_date']]
  
  # vector of first_case_date
  case_date_vector <- owid_reduced %>% group_by(location) %>% 
    summarise_all(mean) %>% .[['first_case_date']]
  
  # count missing new_tests_smooth for each country
  owid_missing_tests <- owid_reduced %>% group_by(location) %>%
    count(is.na(new_tests_smoothed)) %>%
    filter(`is.na(new_tests_smoothed)` == TRUE) %>% as.data.table() %>%
    .[,c("first_case_date","first_test_date") := list(case_date_vector, test_date_vector)] %>%
    arrange(desc(n))
  
  head(owid_missing_tests, 10) %>% print()
  print("--------------------------------------------------")
  owid_missing_tests %>% arrange(desc(first_test_date)) %>% head() %>% print()
  
  
  print("OWID data check done.")
  print("--------------------------------------------------")
  print("Checking Google mobility dataset...")
}




