# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC
# Purpose:      Imputation of inexistent quarters
# programmer:   Zhe Liu
# Date:         25-02-2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- KNN model ----
total.inside.imp <- read_feather("03_Outputs/02_AZ_CHC_Inside_Imp.feather")

# quarter count
quar.count <- total.inside.imp %>% 
  filter(year %in% c("2017", "2018")) %>% 
  setDT() %>% 
  dcast(province ~ quarter, fun.aggregate = length) %>% 
  filter(`2017Q1` > 0, `2017Q2` > 0, `2017Q3` > 0, `2017Q4` > 0,
         `2018Q1` > 0, `2018Q2` > 0, `2018Q3` > 0, `2018Q4` > 0)

# model set
model.data <- total.inside.imp %>% 
  filter(province != "上海") %>% 
  mutate(flag_17 = ifelse(province %in% c("山东", "浙江"), 1, 0),
         flag_19 = ifelse(province %in% c("福建", "浙江", "山东"), 1, 0))

model.set <- model.data %>% 
  filter(year %in% c("2018")) %>% 
  group_by(province, city, pchc, date, flag_17, flag_19) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  setDT() %>% 
  dcast(province + city + pchc + flag_17 + flag_19 ~ date, value.var = "sales", fill = 0)

# model 2017
train.set.17 <- model.set[model.set$flag_17 == 0, ]
test.set.17 <- model.set[model.set$flag_17 == 1, ]

model.train.17 <- train.set.17[, -c("province", "city", "pchc")]
model.test.17 <- test.set.17[, -c("province", "city", "pchc")]

knn.model.17 <- kknn(flag_17 ~ ., train = model.train.17, test = model.test.17, k = 3, scale = TRUE)

# model 2019
train.set.19 <- model.set[model.set$flag_19 == 0, ]
test.set.19 <- model.set[model.set$flag_19 == 1, ]

model.train.19 <- train.set.19[, -c("province", "city", "pchc")]
model.test.19 <- test.set.19[, -c("province", "city", "pchc")]

knn.model.19 <- kknn(flag_19 ~ ., train = model.train.19, test = model.test.19, k = 3, scale = TRUE)

# model weightage 2017
model.indice.17 <- as.data.frame(knn.model.17$C) %>%
  lapply(function(x) {
    train.set.17$pchc[x]
  }) %>%
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>%
  bind_cols(test.set.17[, c("province", "city", "pchc")]) %>%
  setDT() %>%
  melt(id.vars = c("province", "city", "pchc"), variable.name = "knn_level", value.name = "knn_pchc")

model.weight.17 <- as.data.frame(knn.model.17$D) %>%
  lapply(function(x) {
    1 / (x+1)
  }) %>%
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>%
  mutate(weight_sum = pchc_1 + pchc_2 + pchc_3,
         pchc_1 = pchc_1 / weight_sum,
         pchc_2 = pchc_2 / weight_sum,
         pchc_3 = pchc_3 / weight_sum) %>%
  bind_cols(test.set.17[, c("province", "city", "pchc")]) %>%
  select(-weight_sum) %>%
  setDT() %>%
  melt(id.vars = c("province", "city", "pchc"), variable.name = "knn_level", value.name = "knn_weight")

# model weightage 2019
model.indice.19 <- as.data.frame(knn.model.19$C) %>% 
  lapply(function(x) {
    train.set.19$pchc[x]
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  bind_cols(test.set.19[, c("province", "city", "pchc")]) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc"), variable.name = "knn_level", value.name = "knn_pchc")

model.weight.19 <- as.data.frame(knn.model.19$D) %>% 
  lapply(function(x) {
    1 / (x+1)
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  mutate(weight_sum = pchc_1 + pchc_2 + pchc_3,
         pchc_1 = pchc_1 / weight_sum,
         pchc_2 = pchc_2 / weight_sum,
         pchc_3 = pchc_3 / weight_sum) %>% 
  bind_cols(test.set.19[, c("province", "city", "pchc")]) %>% 
  select(-weight_sum) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc"), variable.name = "knn_level", value.name = "knn_weight")


##---- Imputation ----
# molecule growth 2017
model.growth.17 <- model.data %>% 
  filter(flag_17 == 0, year %in% c("2017", "2018")) %>% 
  group_by(knn_pchc = pchc, molecule_desc, year, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(model.indice.17, by = "knn_pchc") %>% 
  left_join(model.weight.17, by = c("province", "city", "pchc", "knn_level")) %>% 
  group_by(pchc, molecule_desc, year, quarter) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  setDT() %>% 
  dcast(pchc + molecule_desc ~ quarter, value.var = "sales", fill = 0) %>% 
  mutate(growth_1718q1 = `2018Q1` / `2017Q1`,
         growth_1718q2 = `2018Q2` / `2017Q2`,
         growth_1718q3 = `2018Q3` / `2017Q3`,
         growth_1718q4 = `2018Q4` / `2017Q4`,
         growth_1718q1 = ifelse(is.na(growth_1718q1) | growth_1718q1 < 0.1 | growth_1718q1 > 10, 1, growth_1718q1),
         growth_1718q2 = ifelse(is.na(growth_1718q2) | growth_1718q2 < 0.1 | growth_1718q2 > 10, 1, growth_1718q2),
         growth_1718q3 = ifelse(is.na(growth_1718q3) | growth_1718q3 < 0.1 | growth_1718q3 > 10, 1, growth_1718q3),
         growth_1718q4 = ifelse(is.na(growth_1718q4) | growth_1718q4 < 0.1 | growth_1718q4 > 10, 1, growth_1718q4)) %>% 
  select(pchc, molecule_desc, `2018Q1` = growth_1718q1, `2018Q2` = growth_1718q2,
         `2018Q3` = growth_1718q3, `2018Q4` = growth_1718q4) %>% 
  setDT() %>% 
  melt(id.vars = c("pchc", "molecule_desc"),
       measuer.vars = c("2018Q1", "2018Q2", "2018Q3", "2018Q4"),
       variable.name = "quarter",
       value.name = "growth",
       variable.factor = FALSE)

# molecule growth 2019
model.growth.19 <- model.data %>% 
  filter(flag_19 == 0, year %in% c("2018", "2019")) %>% 
  group_by(knn_pchc = pchc, molecule_desc, year, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(model.indice.19, by = "knn_pchc") %>% 
  left_join(model.weight.19, by = c("province", "city", "pchc", "knn_level")) %>% 
  group_by(pchc, molecule_desc, year, quarter) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  setDT() %>% 
  dcast(pchc + molecule_desc ~ quarter, value.var = "sales", fill = 0) %>% 
  mutate(growth_1819q1 = `2019Q1` / `2018Q1`,
         growth_1819q2 = `2019Q2` / `2018Q2`,
         growth_1819q3 = `2019Q3` / `2018Q3`,
         growth_1819q4 = `2019Q4` / `2018Q4`,
         growth_1819q1 = ifelse(is.na(growth_1819q1) | growth_1819q1 < 0.1 | growth_1819q1 > 10, 1, growth_1819q1),
         growth_1819q2 = ifelse(is.na(growth_1819q2) | growth_1819q2 < 0.1 | growth_1819q2 > 10, 1, growth_1819q2),
         growth_1819q3 = ifelse(is.na(growth_1819q3) | growth_1819q3 < 0.1 | growth_1819q3 > 10, 1, growth_1819q3),
         growth_1819q4 = ifelse(is.na(growth_1819q4) | growth_1819q4 < 0.1 | growth_1819q4 > 10, 1, growth_1819q4)) %>% 
  select(pchc, molecule_desc, `2018Q1` = growth_1819q1, `2018Q2` = growth_1819q2, 
         `2018Q3` = growth_1819q3, `2018Q4` = growth_1819q4) %>% 
  setDT() %>% 
  melt(id.vars = c("pchc", "molecule_desc"), 
       measuer.vars = c("2018Q1", "2018Q2", "2018Q3", "2018Q4"), 
       variable.name = "quarter", 
       value.name = "growth", 
       variable.factor = FALSE)

# imputation sales 2017
# model.sales.17 <- model.data %>%
#   filter(year %in% c("2018"), flag_17 == 1) %>%
#   left_join(model.growth.17, by = c("pchc", "molecule_desc", "quarter")) %>%
#   mutate(growth = if_else(is.na(growth), 1, growth),
#          growth = if_else(growth > 3, 3, growth),
#          growth = if_else(growth > quantile(growth, 0.9),
#                           mean(growth[growth >= quantile(growth, 0.25) & growth <= quantile(growth, 0.75)]),
#                           growth)) %>%
#   mutate(units_17 = units / growth,
#          sales_17 = sales / growth,
#          date = gsub("2018", "2017", date),
#          quarter = gsub("2018", "2017", quarter),
#          year = "2017",
#          flag = 1) %>%
#   select(year, date, quarter, province, city, pchc, TA, atc4, molecule_desc,
#          packid, units = units_17, sales = sales_17, flag) %>%
#   filter((TA == "GI" & province == "山东" & quarter == "2017Q1") | 
#            (TA == "DM" & province == "山东" & quarter %in% c("2017Q1", "2017Q2", "2017Q3")) | 
#            (TA == "DM" & province == "浙江"))

model.sales.17 <- model.data %>% 
  filter(year %in% c("2018"), flag_17 == 1) %>% 
  left_join(model.growth.17, by = c("pchc", "molecule_desc", "quarter")) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_17 = units / growth,
         sales_17 = sales / growth,
         date = gsub("2018", "2017", date),
         quarter = gsub("2018", "2017", quarter),
         year = "2017") %>% 
  select(year, date, quarter, province, city, pchc, TA, atc4, molecule_desc,
         packid, units_17, sales_17)

# imputation sales 2019
# model.sales.19 <- model.data %>% 
#   filter(year %in% c("2018"), flag_19 == 1) %>% 
#   left_join(model.growth.19, by = c("pchc", "molecule_desc", "quarter")) %>% 
#   mutate(growth = if_else(is.na(growth), 1, growth),
#          growth = if_else(growth > 3, 3, growth),
#          growth = if_else(growth > quantile(growth, 0.9),
#                           mean(growth[growth >= quantile(growth, 0.25) & growth <= quantile(growth, 0.75)]),
#                           growth)) %>% 
#   mutate(units_19 = units * growth,
#          sales_19 = sales * growth,
#          date = gsub("2018", "2019", date),
#          quarter = gsub("2018", "2019", quarter),
#          year = "2019",
#          flag = 1) %>% 
#   select(year, date, quarter, province, city, pchc, TA, atc4, molecule_desc, 
#          packid, units = units_19, sales = sales_19, flag) %>% 
#   filter((TA == "GI" & province == "福建") | (TA == "GI" & province == "山东" & quarter == "2019Q4") | 
#            (TA == "GI" & province == "浙江" & quarter %in% c("2019Q3", "2019Q4")) | 
#            (TA == "RE" & province == "福建") | (TA == "RE" & province == "山东" & quarter == "2019Q4") | 
#            (TA == "RE" & province == "浙江" & quarter %in% c("2019Q3", "2019Q4")) | 
#            (TA == "CV" & province == "福建") | (TA == "CV" & province == "山东" & quarter == "2019Q4") | 
#            (TA == "DM" & province == "福建") | (TA == "DM" & province == "山东" & quarter == "2019Q4"))

model.sales.19 <- model.data %>% 
  filter(year %in% c("2018"), flag_19 == 1) %>% 
  left_join(model.growth.19, by = c("pchc", "molecule_desc", "quarter")) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_19 = units * growth,
         sales_19 = sales * growth,
         date = gsub("2018", "2019", date),
         quarter = gsub("2018", "2019", quarter),
         year = "2019") %>% 
  select(year, date, quarter, province, city, pchc, TA, atc4, molecule_desc, 
         packid, units_19, sales_19)


##---- Result ----
total.outside.imp <- total.inside.imp %>% 
  full_join(model.sales.17, by = c("year", "date", "quarter", "province", "city", "pchc", 
                                   "TA", "atc4", "molecule_desc", "packid")) %>% 
  full_join(model.sales.19, by = c("year", "date", "quarter", "province", "city", "pchc", 
                                   "TA", "atc4", "molecule_desc", "packid")) %>% 
  mutate(units = if_else(is.na(units) & year == "2017", units_17, units),
         units = if_else(is.na(units) & year == "2019", units_19, units),
         sales = if_else(is.na(sales) & year == "2017", sales_17, sales),
         sales = if_else(is.na(sales) & year == "2019", sales_19, sales),
         flag = if_else(is.na(flag), 1, 0)) %>% 
  filter(units > 0, sales > 0) %>% 
  select(-units_17, -sales_17, -units_19, -sales_19)

write_feather(total.outside.imp, "03_Outputs/03_AZ_CHC_Outside_Imp.feather")

# 17，18分别建模，计算加权增长率，用18年数据和增长率补数



