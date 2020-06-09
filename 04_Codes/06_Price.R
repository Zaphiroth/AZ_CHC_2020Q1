# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC 2020Q1
# Purpose:      Price
# programmer:   Zhe Liu
# Date:         2020-06-02
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Origin Price ----
price.origin <- total.imp %>% 
  filter(units > 0) %>% 
  group_by(packid, year, quarter, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by year ----
price.year <- total.imp %>% 
  filter(units > 0) %>% 
  group_by(packid, year, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_year = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by city ----
price.city <- total.imp %>% 
  filter(units > 0) %>% 
  group_by(packid, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_city = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by pack ID ----
price.pack <- total.imp %>% 
  filter(units > 0) %>% 
  group_by(packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_pack = sales / units) %>% 
  select(-sales, -units)


##---- Add new price ----
total.price <- total.proj %>% 
  left_join(price.origin, by = c("province", "city", "year", "quarter", "packid")) %>% 
  left_join(price.year, by = c("province", "city", "year", "packid")) %>% 
  left_join(price.city, by = c("province", "city", "packid")) %>% 
  left_join(price.pack, by = c("packid")) %>% 
  mutate(price = ifelse(is.na(price), price_year, price),
         price = ifelse(is.na(price), price_city, price),
         price = ifelse(is.na(price), price_pack, price)) %>% 
  mutate(units_update = sales / price,
         units_update = ifelse(units_update <= 0, 0, units_update)) %>% 
  filter(!is.na(price), !is.na(sales)) %>% 
  select(year, quarter, province, city, pchc, TA, atc4, 
         molecule_desc, packid, price, units = units_update, sales)

write_feather(total.price, "03_Outputs/06_AZ_CHC_Price.feather")





