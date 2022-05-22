
### Reading all the pages and property pages
numberPages <- 206
noHousePerPage <- 42

# temp <- 0
# list_pages <- vector(mode = "list", length = numberPages)
# list_house <- vector(mode = "list", length = numberPages*noHousePerPage)

# last_page <- 206
# start_page <- 1

# for (page_result in seq(last_page,start_page)) {
#   page = paste0("https://www.realtor.com/realestateandhomes-search/Los-Angeles_CA/show-recently-sold/pg-", 
#                 page_result)
#   html_page = read_html(page)
#   list_pages[[page_result]] <- html_page
#   print(paste("Page:", page_result)) 
#   House_links <- list_pages[[page_result]] %>% 
#     html_nodes(".photo-wrap a") %>%
#     html_attr("href") %>% 
#     paste0("https://www.realtor.com/", .)
#   for (link_number in 1:noHousePerPage) {
#     html_link = read_html(House_links[link_number])
#     index <- numberPages * noHousePerPage + 1 - (temp + link_number)
#     list_house[[index]] <- html_link
#     print(index)
#     print(paste("Page:", page_result, "Link:", link_number))  
#   }
#   temp <- temp + noHousePerPage
# }

### Saving new pages and property pages
# Do not run unless add another file
# for(i in 207:220){
#   saveRDS(as.character(list_pages[[i]]),sprintf("page%d.RDS",i))
# }

# Do not run unless add another file
# for(i in 8653:9240){
#   saveRDS(as.character(list_house[[i]]),sprintf("house%d.RDS",i))
# }

### Loading new pages and property pages
read_page <- vector(mode = "list", length = 206)
for(i in 1:206){
  read_page[[i]] <- read_html(readRDS(sprintf("page%d.RDS",i)))
}
# list_pages <- read_page

read_house <- vector(mode = "list", length = 206*42)
for(i in 1:8652){
  read_house[[i]] <- read_html(readRDS(sprintf("house%d.RDS",i)))
}
# list_house <- read_house

### Creating dataframe from 206 pages
houses.pages <- tibble()
for (page_number in 1:numberPages) {
  print(paste("Page:", page_number)) 
  Price <- list_pages[[page_number]] %>% 
    html_nodes(".jsx-641996641.ldp-redesign-price.srp-page-price") %>% 
    html_text()
  
  Address <- list_pages[[page_number]] %>% 
    html_nodes(".srp-address-redesign") %>% 
    html_text()
  
  Sold_date <- list_pages[[page_number]] %>% 
    html_nodes(".statusText") %>% 
    html_text() %>% 
    str_remove_all("Sold - ") %>% 
    mdy() %>% 
    na.omit()
  
  Otherinfo <- list_pages[[page_number]] %>% 
    html_nodes(".property-meta-srpPage") %>% 
    html_text()
  # One address does not have Otherinfo
  if (page_number == 147) {
    Price <- Price[-9]
    Address <- Address[-9]
    Sold_date <- Sold_date[-9]
  }
  houses.pages <- rbind(houses.pages, tibble(Price, Address, Sold_date, Otherinfo))
}

### Creating # of beds, baths, lot size, and building size from Otherinfo
houses.pages$Bath <- houses.pages$Otherinfo %>% 
  str_extract("(([:punct:]|\\d|\\+)+)(bath)") %>% 
  str_extract("(([:punct:]|\\d)+)") %>% 
  as.numeric()

houses.pages$bulding_size <- houses.pages$Otherinfo %>% 
  str_remove("sqft lot") %>% 
  str_extract("(([:punct:]|\\d|\\s)+)(sqft)") %>% 
  str_extract("(([:punct:]|\\d)+)(sqft)") %>% 
  str_extract("(([:punct:]|\\d)+)") %>% 
  str_remove_all(",") %>% 
  as.numeric()

get_lot_size <- function(otherinfo){
  if (str_detect(otherinfo,"acre lot")) {
    lotsize <- otherinfo %>% 
      str_extract("(([:punct:]|\\d)+)(acre lot)") %>% 
      str_extract("(([:punct:]|\\d)+)")
    
    lotsize <- gsub(",", ".", lotsize) %>% 
      as.numeric()
    
    lotsize <- lotsize*43560
    
  } else if (str_detect(otherinfo,"sqft lot")) {
    lotsize <- otherinfo %>% 
      str_extract("(([:punct:]|\\d)+)(sqft lot)") %>% 
      str_extract("(([:punct:]|\\d)+)") %>% 
      str_remove_all(",") %>% 
      as.numeric()
  }
  else {
    lotsize <- NA
  }
  return(lotsize)
}

houses.pages$lot_size = sapply(houses.pages$Otherinfo, FUN = get_lot_size, USE.NAMES = FALSE)

get_bed <- function(otherinfo){
  bed <- otherinfo %>% 
    str_extract("(([:punct:]|\\d)+)(bed)") %>% 
    str_extract("(([:punct:]|\\d)+)") %>% 
    as.numeric
  if (str_detect(otherinfo,"tudio")) {
    bed <- 0
  }
  return(bed)
}

houses.pages$Bed = sapply(houses.pages$Otherinfo, FUN = get_bed, USE.NAMES = FALSE)


### testing what data should be exctacted from each house page

### Note: list_house[[6166]] should be deleted
check_na <- function(feature){
  if (length(feature) == 0){
    feature <- NA
  }
  return(feature)
}

check_include <- function(dataframe,i){
  dataframe %>% 
    filter(c1 == i) %>% 
    select(c2) %>% 
    count() %>% 
    as.character()
}

property_check <- function(dataframe,i){
  if (check_include(public,i) == 1) {
    
    property <- public %>% 
      filter(c1 == i) %>% 
      select(c2) %>% 
      as.character()
  } else {
    property <- NA
  }
  return(property)
}

main_house <- tibble()
for (i in 5042:8652) {
  print(paste("House:", i)) 
  house_page <- list_house[[i]]
  
  
  house_addr <- house_page %>% 
    html_nodes("#ldp-address h1") %>% 
    html_text() %>% 
    str_squish()
  house_addr <- check_na(house_addr)
  
  yearBuilt <- house_page %>% 
    html_nodes(".ra-year-built~ .ellipsis") %>% 
    html_text() 
  yearBuilt <- check_na(yearBuilt)
  
  propertyType <- house_page %>% 
    html_nodes(".ra-property-type~ .ellipsis") %>% 
    html_text() 
  propertyType <- check_na(propertyType)
  
  more_features <- house_page %>% 
    html_nodes("#ldp-detail-features") %>% 
    html_text() %>% 
    str_squish()
  more_features <- check_na(more_features)
  
  Water_Source <- case_when(
    more_features %>% 
      str_extract("Water Source: (.){12}") %>% 
      str_detect("Pub") ~ "Public",
    more_features %>% 
      str_extract("Water Source: (.){12}") %>% 
      str_detect("Pri") ~ "Private",
    TRUE ~ NA_character_)
  
  Sewer <- case_when(
    more_features %>% 
      str_extract("Sewer: (.){12}") %>% 
      str_detect("Pub") ~ "Public",
    more_features %>% 
      str_extract("Sewer: (.){12}") %>% 
      str_detect("Pri") ~ "Private",
    TRUE ~ NA_character_)
  
  No_Garage <- more_features %>% 
    str_extract("Garage Spaces: (.){5}") %>% 
    str_extract("((\\d)+)") %>% 
    as.numeric()
  
  Have_garage <- case_when(
    more_features %>% 
      str_detect("Garage") ~ "Yes",
    TRUE ~ "No")
  
  length_school <- house_page %>% 
    html_nodes("#ldp-school") %>% 
    html_text() %>% 
    str_squish() %>% str_length()
  
  if(length_school < 300){
    Distance_to_School <- NA
  } else {
    Distance_to_School <- house_page %>% 
      html_nodes("#ldp-school") %>% 
      html_table() %>% .[[1]] %>% 
      select(Distance) %>% .[[1]] %>% 
      str_extract("(\\d|[:punct:])+") %>% 
      as.numeric() %>% mean()
  }
  
  public_record <- house_page %>% 
    html_nodes("#ldp-detail-public-records") %>% 
    html_text() 
  public_record <- check_na(public_record)
  
  if (!is.na(public_record)){
    r1 <- public_record %>% 
      str_extract_all("\\n(.)+") %>% .[[1]] %>% 
      str_squish() %>% 
      str_extract("(.)+:") %>% 
      str_extract("([:alnum:]|\\s)+") %>% 
      tibble()
    
    
    r2 <- public_record %>% 
      str_extract_all("\\n(.)+") %>% .[[1]] %>% 
      str_squish() %>% 
      str_extract(": ([^ ]+)") %>% 
      str_replace(": ([^ ]+)","\\1") %>% 
      tibble()
    
    public <- cbind(r1, r2)
    colnames(public) <- (c("c1", "c2"))
    public <- tibble(public)
    
    Heating <- property_check(public,"Heating")
    Cooling <- property_check(public,"Cooling")
    Year_renovated <- property_check(public,"Year renovated")
    Property_type <- property_check(public,"Property type")
    Rooms <- property_check(public,"Rooms")
    Stories <- property_check(public,"Stories")
    Units <- property_check(public,"Units")
  } else {
    Heating <- NA
    Cooling <- NA
    Year_renovated <- NA
    Property_type <- NA
    Rooms <- NA
    Stories <- NA
    Units <- NA
  }
  
  
  house_df <- tibble(house_addr,
                     Rooms,
                     Stories,
                     yearBuilt, 
                     Year_renovated,
                     propertyType,
                     Property_type,
                     Heating,
                     Cooling,
                     Units,
                     Water_Source,
                     Sewer,
                     No_Garage,
                     Have_garage,
                     Distance_to_School,
                     length_school
  )
  
  history_price_length <- house_page %>% 
    html_nodes("#ldp-history-price") %>% 
    html_text() %>% 
    str_squish() %>% 
    str_length()
  
  if(history_price_length < 65){
    
    house <- houses.pages %>% 
      filter(Address == house_addr)
    
    date <- house %>% 
      select(Sold_date) %>% .[[1]]
    
    price <- house %>% 
      select(Price) %>% 
      as.character()
    
    temp_hist_price_df <- tibble(Date = date, 
                                 Event = "Sold", 
                                 Price = price, 
                                 Price_Sq_Ft = NA)
    
    house.df <- temp_hist_price_df %>% 
      left_join(house, by = "Price") %>% 
      select(-Sold_date, -Otherinfo)
      
  } else {
    history_price <- house_page %>% 
      html_nodes("#ldp-history-price") %>% 
      html_table() %>% .[[1]]
    
    colnames(history_price) <- c("Date", "Event", "Price", "Price_Sq_Ft", "Source")
    
    history_price$Date <- history_price$Date %>%
      mdy()
    
    house <- houses.pages %>% 
      filter(Address == house_addr)
    
    price <- houses.pages %>% 
      filter(Address == house_addr) %>% 
      select(Price) %>% 
      as.character()
    
    if (!price %in% history_price$Price){
      date <- data.frame(Date = NA, Event = "Sold", Price = price, Price_Sq_Ft = NA, Source = NA)
      
      date[1] <- houses.pages %>%
        filter(Address == house_addr) %>%
        select(Sold_date)
      
      date <- tibble(date)
      
      history_price <- rbind(date,history_price)
    }
    
    house.df <- history_price %>% 
      select(-Source) %>% 
      left_join(house, by = "Price") %>% 
      select(-Sold_date, -Otherinfo)
  }
  
  temp_house <- house.df %>% 
    left_join(house_df, by = c("Address" = "house_addr")) %>% 
    fill(c(5:12,14,15,18)) %>% 
    arrange(Date) %>% 
    fill(c(5:12,14,15,18)) %>% 
    arrange(desc(Date))
  
  main_house <- rbind(main_house, temp_house)
}

# cleaning/processing columns

main_house <- main_house %>% select(-Price_Sq_Ft)

## clean price column
main_house$Price <-  main_house$Price %>% 
  str_remove("\\$") %>% 
  str_remove_all(",") %>% 
  as.numeric()

## define price/sqft column
main_house <- main_house %>% 
  mutate(Price_per_Sqft = case_when(
    is.na(bulding_size) ~ Price/lot_size,
    TRUE ~ Price/bulding_size
  ))

# order columns
main_house <- main_house %>% select(Date, 
                                    Address, 
                                    Price, 
                                    Price_per_Sqft, everything())

# Back up 1
backup_main_house <- main_house


# make some columns as integer
int_cols <- c(10:13,18,21)
main_house[, int_cols] <- sapply(main_house[, int_cols], as.integer)

# delete useless columns
main_house <- main_house %>% select(-Property_type, -length_school, -Distance_to_School)

# categorizing values in several columns
main_house <- main_house %>% mutate(propertyType = case_when(
  propertyType == "Other" ~ NA_character_,
  TRUE ~ propertyType
))

main_house <- main_house %>% mutate(Heating = case_when(
  Heating %in% c("Type", "Unknown") ~ NA_character_,
  TRUE ~ Heating
))

main_house <- main_house %>% mutate(Cooling = case_when(
  Cooling %in% c("Unknown", "None", "Type") ~ NA_character_,
  TRUE ~ Cooling
))

# make some columns as factor
fac_cols <- c(5, 14:16, 18:19, 21)
main_house[, fac_cols] <- lapply(main_house[, fac_cols], factor)

# create year and month columns from Date
main_house <- main_house %>% 
  mutate(Year = year(Date), Month = month(Date)) %>% 
  select(Date, Year, Month, everything())

## reading CPI data of the USA from csv file
CPI <- read_csv("CPILFENS.csv")

CPI <- CPI %>% 
  mutate(year = year(ymd(DATE)), month = month(ymd(DATE)))

CPI <- CPI %>% 
  mutate(adj_factor = CPILFENS/266.004)

# adding adj_factor to main_house df
main_house <- main_house %>% 
  left_join(CPI, by = c("Year" = "year", "Month" = "month")) %>% 
  select(Date:Price, adj_factor, everything()) %>% 
  select(-CPILFENS, -DATE)

# adjusting price and price/sqft using adj_factor
main_house <- main_house %>% 
  mutate(adj_price = Price/adj_factor, 
         adj_price_sqft = Price_per_Sqft/adj_factor) %>% 
  select(Date:adj_factor, adj_price, 
         Price_per_Sqft, adj_price_sqft, 
         everything())

# getting geocode from google sheet (latitude and longitude)
write_csv(gg,file = "house_addresses.csv")
library(readxl)
address <- read_excel("address.xlsx")
address <- address %>% 
  filter(!is.na(Address)) %>% distinct()

# adding latitude and longitude
main_house <- main_house %>%
  left_join(address, by = "Address") %>%
  select(Date, Address, Latitude, Longitude, everything()) 

# Back up 2
backup2_main_house <- main_house

# categorizing values in several columns
main_house <- main_house %>% mutate(yearBuilt = case_when(
  Year < yearBuilt ~ NA_integer_,
  TRUE ~ yearBuilt
))

## year_renovated values
addresswithyearrenov <- main_house %>% 
  filter(!is.na(Year_renovated)) %>% 
  select(Address, Year_renovated) %>% 
  distinct()

main_house <- main_house %>% 
  select(-Year_renovated) %>% 
  left_join(addresswithyearrenov, by = "Address") %>% 
  select(Date:yearBuilt, Year_renovated, everything())

main_house <- main_house %>% 
  mutate(Year_renovated = case_when(
    is.na(Year_renovated) & !is.na(yearBuilt) ~ yearBuilt,
    is.na(yearBuilt) ~ NA_integer_,
    TRUE ~ Year_renovated
  ))


main_house <- main_house %>% mutate(Year_renovated = case_when(
  Year_renovated < yearBuilt ~ NA_integer_,
  Year_renovated > Year ~ yearBuilt,
  TRUE ~ Year_renovated
))

# Deleting the same observations
main_house <- main_house %>% 
  group_by(Address, Date, Price) %>% 
  filter(n() == 1)

main_house <- main_house %>% 
  ungroup()

# deleting observations with null addresses or price
main_house <- main_house %>% 
  filter(!is.na(Address))

main_house <- main_house %>% 
  filter(!is.na(Price))

# dealing with NAs


## heating values
addressheating <- main_house %>% 
  filter(!is.na(Heating)) %>% 
  select(Address, Heating) %>% 
  distinct()

main_house <- main_house %>% 
  select(-Heating) %>% 
  left_join(addressheating, by = "Address") %>% 
  select(Date:propertyType, Heating, everything())

## cooling values
addresscooling <- main_house %>% 
  filter(!is.na(Cooling)) %>% 
  select(Address, Cooling) %>% 
  distinct()

main_house <- main_house %>% 
  select(-Cooling) %>% 
  left_join(addresscooling, by = "Address") %>% 
  select(Date:Heating, Cooling, everything())

## water_source values
addresswater <- main_house %>% 
  filter(!is.na(Water_Source)) %>% 
  select(Address, Water_Source) %>% 
  distinct()

main_house <- main_house %>% 
  select(-Water_Source) %>% 
  left_join(addresswater, by = "Address") %>% 
  select(Date:Units, Water_Source, everything())

## sewer values
addressSewer <- main_house %>% 
  filter(!is.na(Sewer)) %>% 
  select(Address, Sewer) %>% 
  distinct()

main_house <- main_house %>% 
  select(-Sewer) %>% 
  left_join(addressSewer, by = "Address") %>% 
  select(Date:Water_Source, Sewer, everything())

## have_garage values
addressGarage <- main_house %>% 
  filter(!is.na(Have_garage)) %>% 
  select(Address, Have_garage) %>% 
  distinct()

main_house <- main_house %>% 
  select(-Have_garage) %>% 
  left_join(addressGarage, by = "Address") %>% 
  select(Date:No_Garage, Have_garage, everything())

## no_garage values
addressNoOfGarage <- main_house %>% 
  filter(!is.na(No_Garage)) %>% 
  select(Address, No_Garage) %>% 
  distinct()

main_house <- main_house %>% 
  select(-No_Garage) %>% 
  left_join(addressNoOfGarage, by = "Address") %>% 
  select(Date:Sewer, No_Garage, everything())

main_house <- main_house %>% 
  mutate(No_Garage = case_when(
    Have_garage == "No" ~ as.integer(0),
    TRUE ~ No_Garage
  ))

# delete useless columns
main_house <- main_house %>% 
  select(-Rooms, -Stories, -Units, -Sewer, -Water_Source)

# Back up 3
backup3_main_house <- main_house

# #trim data
# trim <- 0.001
# l <- round(nrow(main_house)*trim)
# u <- round(nrow(main_house)*(1-trim))
# main_house <- main_house %>%
#   arrange(Price) %>%
#   slice(l:u) 
# 
# # filter Date
# main_house <- main_house %>% filter(Date > "1963-01-01")

main_house1 <- main_house %>% 
  mutate(age = Year - yearBuilt,
         renovated_age = Year - Year_renovated) %>% 
  select(-yearBuilt, -Year_renovated)


main_house %>% filter(is.na(Year)) %>% nrow()
0
main_house %>% filter(is.na(Month)) %>% nrow()
0
main_house %>% filter(is.na(Longitude)) %>% nrow()
0
main_house %>% filter(is.na(Latitude)) %>% nrow()
0
main_house %>% filter(is.na(adj_price)) %>% nrow()
0
main_house %>% filter(is.na(Event)) %>% nrow()
0
main_house %>% filter(is.na(Bath)) %>% nrow()
 3834
main_house %>% filter(is.na(bulding_size)) %>% nrow()
 1239
main_house %>% filter(is.na(lot_size)) %>% nrow()
 835
main_house %>% filter(is.na(Bed)) %>% nrow()
 3498
main_house %>% filter(is.na(yearBuilt)) %>% nrow()
 2312
main_house %>% filter(is.na(Year_renovated)) %>% nrow()
 2312
main_house %>% filter(is.na(propertyType)) %>% nrow()
 784
main_house %>% filter(is.na(Heating)) %>% nrow()
 20836
main_house %>% filter(is.na(Cooling)) %>% nrow()
 25972
main_house %>% filter(is.na(No_Garage)) %>% nrow()
 13778
main_house %>% filter(is.na(Have_garage)) %>% nrow()
 362

 write_xlsx(as.data.frame.matrix(summary(main_house)), "summary_main_house.xlsx")
 
# creating long_data
long_data <- main_house %>% 
  select(-Heating, 
         -Cooling, 
         -No_Garage) %>% 
  filter(!is.na(Bath),
         !is.na(bulding_size),
         !is.na(lot_size), 
         !is.na(Bed), 
         !is.na(yearBuilt), 
         !is.na(propertyType),
         !is.na(Have_garage))


long_data <- long_data %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

long_data <- long_data %>% 
  select(-Address, -Price, -adj_factor,
         -Price_per_Sqft, -adj_price_sqft, 
         -Event) %>% 
  select(id, adj_price, Year, Month, everything()) %>% 
  rename(building_size = bulding_size)

long_data <- long_data %>% 
  mutate(age = Year - yearBuilt,
         renovated_age = Year - Year_renovated) %>% 
  select(-yearBuilt, -Year_renovated)



long_data %>% count(propertyType)

long_data <- long_data %>% 
  filter(propertyType != "Mfd/Mobile Home")

long_data$propertyType <- droplevels(long_data$propertyType)

summary(long_data) 

write_xlsx(as.data.frame.matrix(summary(long_data)), "table_longdata.xlsx")

numeric_columns <- colnames(long_data %>% select_if(is.numeric))

density <- function(dataframe, feature){
  feature <- as.name(feature)
  density_plot <- dataframe %>% 
    ggplot() + 
    aes(x = {{feature}}) + 
    geom_density() + 
    labs(title = "Before") +
    xlab("Price")
  return(density_plot)
}

density1 <- function(dataframe, feature){
  feature <- as.name(feature)
  density_plot <- dataframe %>% 
    ggplot() + 
    aes(x = {{feature}}) + 
    geom_density() + 
    labs(title = "After") +
    xlab("Price")
  return(density_plot)
}

plot_before1 <- long_data %>% 
  ggplot() + 
  aes(x = adj_price) + 
  geom_density() + 
  #labs(title = "Before") +
  xlab("Price")
plot_after1 <- neat_data %>% 
  ggplot() + 
  aes(x = adj_price) + 
  geom_density() + 
 # labs(title = "After") +
  xlab("Price")

plot_before2 <- long_data %>% 
  ggplot() + 
  aes(x = Bath) + 
  geom_bar() + 
 # labs(title = "Before") +
  xlab("# of bath")
plot_after2 <- neat_data %>% 
  ggplot() + 
  aes(x = Bath) + 
  geom_bar() + 
#  labs(title = "After") +
  xlab("# of bath")

long_data %>% 
  ggplot() + 
  aes(x = building_size) + 
  geom_density() +
  xlab("Building size")

neat_data %>% 
  ggplot() + 
  aes(x = building_size) + 
  geom_density() + 
  xlab("Building size")

long_data %>% 
  ggplot() + 
  aes(x = lot_size) + 
  geom_density() +
  xlab("Lot size")

neat_data %>% 
  ggplot() + 
  aes(x = lot_size) + 
  geom_density() + 
  xlab("Lot size")

long_data %>% 
  ggplot() + 
  aes(x = Bed) + 
  geom_bar() + 
  xlab("# of bed")

neat_data %>% 
  ggplot() + 
  aes(x = Bed) + 
  geom_bar() + 
  xlab("# of bed")

multiplot(plotlist = c(plot_before1, plot_after1, plot_before2, plot_after2), cols=2)

plots <- list()
count <- 1
for (i in numeric_columns[c(-1,-4)]) {
  plots[[count]] <- density(long_data, i)
  count <- count + 1
}
multiplot(plotlist = plots[1:4], cols=2)
multiplot(plotlist = plots[5:8], cols=2)
multiplot(plotlist = plots[9:10], cols=2)

multiplot(plotlist = plots[1], cols=2)
# removing outliers

no_outlier <- function(dataframe, feature){
  `%!in%` <- Negate(`%in%`)
  feature <- as.name(feature)
  clean_data <- dataframe %>% filter({{feature}} %!in% (dataframe %>% 
                              identify_outliers({{feature}})%>% 
                              filter(is.extreme == T) %>% 
                              select({{feature}}) %>% .[[1]]
                          )
           )
  return(clean_data)
  }

temp_long_data <- long_data

for (i in numeric_columns) {
  neat_data <- no_outlier(temp_long_data, i)
  temp_long_data <- neat_data
}

summary(neat_data)

write_xlsx(as.data.frame.matrix(summary(neat_data)), "table_neatdata.xlsx")

plots1 <- list()
count1 <- 1
for (i in numeric_columns[c(-1,-4)]) {
  plots1[[count1]] <- density1(neat_data, i)
  count1 <- count1 + 1
}
multiplot(plotlist = plots1[1:4], cols=2)
multiplot(plotlist = plots1[5:8], cols=2)
multiplot(plotlist = plots1[9:10], cols=2)



long_data %>% filter(is.na(Bath)) %>% nrow()
0
long_data %>% filter(is.na(bulding_size)) %>% nrow()
0
long_data %>% filter(is.na(lot_size)) %>% nrow()
0
long_data %>% filter(is.na(Bed)) %>% nrow()
0
long_data %>% filter(is.na(yearBuilt)) %>% nrow()
0
long_data %>% filter(is.na(Year_renovated)) %>% nrow()
0
long_data %>% filter(is.na(propertyType)) %>% nrow()
0
long_data %>% filter(is.na(Have_garage)) %>% nrow()
0
long_data %>% nrow

# creating wide_data
wide_data <- main_house %>% 
  filter(!is.na(Cooling), 
         !is.na(Heating),
         !is.na(No_Garage),
         !is.na(Year_renovated),
         !is.na(lot_size),
         !is.na(Bath),
         !is.na(Bed),
         !is.na(propertyType),
         !is.na(bulding_size))

wide_data %>% filter(is.na(bulding_size)) %>% nrow()
0
wide_data %>% filter(is.na(lot_size)) %>% nrow()
0
wide_data %>% filter(is.na(Bed)) %>% nrow()
0
wide_data %>% filter(is.na(yearBuilt)) %>% nrow()
0
wide_data %>% filter(is.na(Year_renovated)) %>% nrow()
0
wide_data %>% filter(is.na(propertyType)) %>% nrow()
0
wide_data %>% filter(is.na(Heating)) %>% nrow()
0
wide_data %>% filter(is.na(Cooling)) %>% nrow()
0
wide_data %>% filter(is.na(No_Garage)) %>% nrow()
0
wide_data %>% filter(is.na(Have_garage)) %>% nrow()
0
wide_data %>% nrow()

# export for Tableau

# library(xlsx)
# write.xlsx(long_data, file = "long_data_house.xlsx")
# write.xlsx(wide_data, file = "wide_data_house.xlsx")
write.csv(wide_data, file = "wide_data_house.csv")
write.csv(long_data, file = "long_data_house.csv")

main_house %>% count(Event) %>% 
  ggplot(aes(fct_reorder(Event, n) , n)) +
  geom_bar(aes(fill = Event), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Event") +
  theme(legend.position = "none")


main_house %>% mutate(propertyType = case_when(
  propertyType == "Condo/Townhome/Row Home/Co-Op" ~ "Condo/Townhome",
  TRUE ~ as.character(propertyType)
)) %>% count(propertyType) %>%
  ggplot(aes(fct_reorder(propertyType, n), n)) +
  geom_bar(aes(fill = propertyType), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Type of Property") +
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust = 1))

main_house %>% count(Heating) %>% 
  ggplot(aes(fct_reorder(Heating, n) , n)) +
  geom_bar(aes(fill = Heating), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Heating System") +
  theme(legend.position = "none")

main_house %>% count(Cooling) %>% 
  ggplot(aes(fct_reorder(Cooling, n) , n)) +
  geom_bar(aes(fill = Cooling), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Cooling System") +
  theme(legend.position = "none")

main_house %>% count(Have_garage) %>% 
  ggplot(aes(Have_garage, n)) +
  geom_bar(aes(fill = Have_garage), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Having Garage") +
  theme(legend.position = "none")


wide_data %>% mutate(propertyType = case_when(
  propertyType == "Condo/Townhome/Row Home/Co-Op" ~ "Condo/Townhome",
  TRUE ~ as.character(propertyType)
)) %>% count(propertyType) %>%
  ggplot(aes(fct_reorder(propertyType, n), n)) +
  geom_bar(aes(fill = propertyType), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Type of Property") +
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust = 1))

wide_data %>% count(Heating) %>% 
  ggplot(aes(fct_reorder(Heating, n) , n)) +
  geom_bar(aes(fill = Heating), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Heating System") +
  theme(legend.position = "none")

wide_data %>% count(Cooling) %>% 
  ggplot(aes(fct_reorder(Cooling, n) , n)) +
  geom_bar(aes(fill = Cooling), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Cooling System") +
  theme(legend.position = "none")


wide_data %>% count(Have_garage) %>% 
  ggplot(aes(Have_garage, n)) +
  geom_bar(aes(fill = Have_garage), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Having Garage") +
  theme(legend.position = "none")

long_data %>% mutate(propertyType = case_when(
  propertyType == "Condo/Townhome/Row Home/Co-Op" ~ "Condo/Townhome",
  TRUE ~ as.character(propertyType)
)) %>% count(propertyType) %>%
  ggplot(aes(fct_reorder(propertyType, n), n)) +
  geom_bar(aes(fill = propertyType), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Type of Property") +
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust = 1))



long_data %>% count(Have_garage) %>% 
  ggplot(aes(Have_garage, n)) +
  geom_bar(aes(fill = Have_garage), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Having Garage") +
  theme(legend.position = "none")


neat_data %>% mutate(propertyType = case_when(
  propertyType == "Condo/Townhome/Row Home/Co-Op" ~ "Condo/Townhome",
  TRUE ~ as.character(propertyType)
)) %>% count(propertyType) %>%
  ggplot(aes(fct_reorder(propertyType, n), n)) +
  geom_bar(aes(fill = propertyType), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Type of Property") +
  theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust = 1))


neat_data %>% count(Have_garage) %>% 
  ggplot(aes(Have_garage, n)) +
  geom_bar(aes(fill = Have_garage), stat = "identity") +
  geom_text(aes(label = n), vjust=-0.3, size=3.5) +
  ylab("Count") +
  xlab("Having Garage") +
  theme(legend.position = "none")


neat_data %>%
  ggplot(aes(adj_price)) +
  geom_histogram(bins = 70, fill = "blue") +
  ylab("Count") +
  xlab("Price") 


long_data  %>%
  ggplot(aes(log(adj_price))) +
  geom_histogram(bins = 70, fill = "blue") +
  ylab("Count") +
  xlab("Log(Price)") +
  coord_cartesian(xlim = c(10,17))




