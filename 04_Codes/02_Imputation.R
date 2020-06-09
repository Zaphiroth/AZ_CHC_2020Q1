# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC 2020Q1
# Purpose:      Imputation
# programmer:   Zhe Liu
# Date:         2020-06-02
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Imputing inside existing provinces ----
# quarterly date continuity
date.continuity <- ta.raw %>% 
  distinct(province, city, pchc, TA, year, date) %>% 
  count(province, city, pchc, TA, year) %>% 
  setDT() %>% 
  dcast(province + city + pchc + TA ~ year, value.var = "n", fill = 0) %>% 
  mutate(cnt_min = pmin(`2019`, `2020`),
         cnt_max = pmax(`2019`, `2020`))

# city molecule yearly growth
city.growth <- date.continuity %>% 
  filter(cnt_min >= 2) %>% 
  inner_join(ta.raw, by = c("province", "city", "pchc", "TA")) %>% 
  group_by(province, city, year, TA, atc4, molecule_desc) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "year", "TA", "atc4", "molecule_desc"), 
       measure.vars = c("units", "sales"), variable.name = "cate", value.name = "value") %>% 
  unite("type", cate, year) %>% 
  setDT() %>% 
  dcast(province + city + TA + atc4 + molecule_desc ~ type, value.var = "value", fill = 0) %>% 
  mutate(sales_growth = sales_2020 / sales_2019,
         units_growth = units_2020 / units_2019,
         sales_growth = ifelse(is.na(sales_growth) | sales_growth < 0.1 | sales_growth > 10, 1, sales_growth),
         units_growth = ifelse(is.na(units_growth) | units_growth < 0.1 | units_growth > 10, 1, units_growth)) %>% 
  select(province, city, TA, atc4, molecule_desc, sales_growth, units_growth)

# imputing
imputing.data <- date.continuity %>% 
  filter(cnt_max >= 2) %>% 
  select(province, city, pchc, TA) %>% 
  left_join(ta.raw, by = c("province", "city", "pchc", "TA")) %>% 
  mutate(month = stri_sub(date, 5, 6)) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc", "year", "month", "TA", "atc4", "molecule_desc", "packid"),
       measure.vars = c("units", "sales"), variable.name = "cate", value.name = "value") %>% 
  unite("type", cate, year) %>% 
  dcast(province + city + pchc + TA + atc4 + molecule_desc + packid + month ~ type,
        value.var = "value", fill = -1) %>%
  left_join(city.growth, by = c("province", "city", "TA", "atc4", "molecule_desc")) %>% 
  mutate(sales_growth = ifelse(is.na(sales_growth), 1, sales_growth),
         units_growth = ifelse(is.na(units_growth), 1, units_growth),
         flag_2019 = ifelse(sales_2019 == -1, 1, 0),
         flag_2020 = ifelse(sales_2020 == -1, 1, 0),
         sales_2019 = ifelse(flag_2019 == 1, sales_2020 / sales_growth, sales_2019),
         sales_2020 = ifelse(flag_2020 == 1, sales_2019 * sales_growth, sales_2020),
         units_2019 = ifelse(flag_2019 == 1, units_2020 / units_growth, units_2019),
         units_2020 = ifelse(flag_2020 == 1, units_2019 * units_growth, units_2020)) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc", "TA", "atc4", "molecule_desc", "packid", "month"), 
       measure.vars = c("flag_2019", "flag_2020", "sales_2019", "sales_2020", "units_2019", "units_2020"), 
       variable.name = "cate", value.name = "value") %>% 
  separate(cate, c("type", "year"), sep = "_") %>% 
  dcast(province + city + pchc + year + month + TA + atc4 + molecule_desc + packid ~ type, 
        value.var = "value") %>% 
  rename("units_imp" = "units",
         "sales_imp" = "sales") %>% 
  mutate(date = stri_paste(year, month),
         quarter = stri_paste(year, "Q1")) %>% 
  select(year, date, quarter, province, city, pchc, TA, atc4, molecule_desc, 
         packid, units_imp, sales_imp, flag)

# result
total.in.imp <- ta.raw %>% 
  full_join(imputing.data, by = c("year", "date", "quarter", "province", "city", "pchc", 
                                  "TA", "atc4", "molecule_desc", "packid")) %>% 
  mutate(units = if_else(is.na(units), units_imp, units),
         sales = if_else(is.na(sales), sales_imp, sales),
         flag = if_else(is.na(flag), 0, flag)) %>% 
  select(year, date, quarter, province, city, pchc, TA, atc4, molecule_desc, 
         packid, units, sales, flag)


##---- Result ----
total.imp <- total.in.imp

write_feather(total.imp, "03_Outputs/02_AZ_CHC_Imp.feather")






