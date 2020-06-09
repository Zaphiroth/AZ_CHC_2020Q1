# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC 2020Q1
# Purpose:      Shanghai
# programmer:   Zhe Liu
# Date:         2020-06-02
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Sample ----
# total.inside.imp <- read_feather("03_Outputs/02_AZ_CHC_Inside_Imp.feather")
# total.raw <- read_feather("03_Outputs/01_AZ_CHC_Total_Raw.feather")
# total.servier.raw <- read_feather("03_Outputs/01_Servier_CHC_Total_Raw.feather")
# market.cndrug <- read.xlsx("02_Inputs/Market_Def_2020_CHC_0317.xlsx", sheet = "XZK-其他降脂中药")

# HTN, NIAD pack
pack.sub <- total.raw %>% 
  mutate(flag_ta = ifelse(stri_sub(ATC4_Code, 1, 4) %in% c("C01D") | 
                            stri_sub(ATC4_Code, 1, 3) %in% c("C03", "C07", "C08", "C09") | 
                            Prd_desc == "CADUET             PFZ", "CV", 
                          ifelse(stri_sub(ATC4_Code, 1, 4) %in% c("A10L", "A10H", "A10M", "A10J", 
                                                                  "A10K", "A10S", "A10N"), "DM", 
                                 NA))) %>% 
  filter(!is.na(flag_ta)) %>% 
  distinct(packid, flag_ta)

# Servier sample
servier.sample.raw <- read.xlsx("02_Inputs/07_AZ_CHC_Result.xlsx")

servier.sample <- servier.sample.raw %>% 
  filter(`城市` %in% c("上海"), 
         `年季` %in% c("2019Q1")) %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0)) %>% 
  left_join(pack.sub, by = c("Pack_ID" = "packid")) %>% 
  filter(!is.na(flag_ta)) %>% 
  mutate(MKT = flag_ta) %>% 
  group_by(year = stri_sub(`年季`, 1, 4), quarter = `年季`, province = `省份`, city = `城市`, 
           TA, atc4 = ATC.Code.IV, molecule_desc = Mole_Ename, packid = Pack_ID) %>% 
  summarise(units = sum(`数量（盒）`, na.rm = TRUE),
            sales = sum(`金额（元）`, na.rm = TRUE)) %>% 
  ungroup()

# total substitude
sh.sub <- total.imp %>% 
  filter(city %in% c("上海"),
         !(packid %in% pack.sub$packid)) %>% 
  bind_rows(servier.sample) %>% 
  group_by(year, quarter, province, city, TA, atc4, molecule_desc, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# Beijing sample
bj.sample <- total.imp %>% 
  filter(city == "北京", 
         quarter %in% c("2019Q1", "2020Q1"))

# Shanghai sample
sh.sample <- total.imp %>% 
  filter(city %in% c("北京")) %>% 
  bind_rows(sh.sub, bj.sample) %>% 
  mutate(province = "上海",
         city = "上海") %>% 
  group_by(quarter, city, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# Shanghai growth
sh.growth <- sh.sample %>% 
  spread(quarter, sales, fill = 0) %>% 
  mutate(growth_1920q1 = `2020Q1` / `2019Q1`,
         growth_1920q1 = if_else(growth_1920q1 > 3, 1, growth_1920q1),
         quarter_pmin = pmin(`2019Q1`, `2020Q1`)) %>% 
  filter(quarter_pmin > 0) %>% 
  select(city, packid, growth_1920q1)


##---- KNN model ----
# ims sales
ims.raw <- fread("02_Inputs/cn_IMS_Sales_Fdata_201912_1.txt", stringsAsFactors = FALSE)

ims.sales <- ims.raw %>% 
  mutate(date = gsub("M", "", Period_Code),
         packid = stri_pad_left(Pack_ID, 7, 0),
         sample_flag = if_else(packid %in% sh.growth$packid, 1, 0)) %>% 
  filter(Geography_id == "CHT", date >= "201701", packid %in% sh.sample$packid) %>% 
  group_by(date, packid) %>% 
  summarise(sales = sum(LC, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(date, sales, fill = 0) %>% 
  mutate(train_flag = if_else(packid %in% sh.growth$packid, 1, 0))

# set
train.sh <- ims.sales[ims.sales$train_flag == 1, ]
test.sh <- ims.sales[ims.sales$train_flag == 0, ]

train.sh.tmp <- select(train.sh, -packid, -train_flag)
test.sh.tmp <- select(test.sh, -packid, -train_flag)

# model
sh.model <- kknn(`201701` ~ ., train = train.sh.tmp, test = test.sh.tmp, k = 3, scale = TRUE)

sh.indice <- as.data.frame(sh.model$C) %>% 
  lapply(function(x) {
    train.sh$packid[x]
  }) %>% 
  as.data.frame(col.names = c("pack_1", "pack_2", "pack_3")) %>% 
  bind_cols(test.sh[, c("packid")]) %>% 
  setDT() %>% 
  melt(id.vars = "packid", variable.name = "knn_level", value.name = "knn_pack")

sh.weight <- as.data.frame(sh.model$D) %>% 
  lapply(function(x) {
    1 / (x+1)
  }) %>% 
  as.data.frame(col.names = c("pack_1", "pack_2", "pack_3")) %>% 
  mutate(weight_sum = pack_1 + pack_2 + pack_3,
         pack_1 = pack_1 / weight_sum,
         pack_2 = pack_2 / weight_sum,
         pack_3 = pack_3 / weight_sum) %>% 
  bind_cols(test.sh[, c("packid")]) %>% 
  select(-weight_sum) %>% 
  setDT() %>% 
  melt(id.vars = "packid", variable.name = "knn_level", value.name = "knn_weight")

# growth
weight.growth <- sh.indice %>% 
  left_join(sh.weight, by = c("packid", "knn_level")) %>% 
  left_join(sh.growth, by = c("knn_pack" = "packid")) %>% 
  group_by(city, packid) %>% 
  summarise(growth_1920q1 = sum(growth_1920q1 * knn_weight, na.rm = TRUE)) %>% 
  select(city, packid, growth_1920q1)

# diff
surplus <- setdiff(sh.sample$packid[!(sh.sample$packid %in% sh.growth$packid)], ims.sales$packid)

# surplus.growth <- data.frame(city = "上海",
#                              packid = surplus) %>% 
#   mutate(growth_1920q1 = 1)

sh.growth.add <- bind_rows(merge(sh.growth, 0),
                           merge(weight.growth, 1))
  # setDT() %>% 
  # melt(id.vars = c("city", "packid"), 
  #      measure.vars = c("growth_1718q2", "growth_1718q3", "growth_1718q4", 
  #                       "growth_1819q1", "growth_1819q2", "growth_1819q3", 
  #                       "growth_1819q4"), 
  #      variable.name = "quarter", 
  #      value.name = "growth", 
  #      variable.factor = FALSE) %>% 
  # mutate(quarter = case_when(
  #   quarter == "growth_1718q2" ~ "2017Q2",
  #   quarter == "growth_1718q3" ~ "2017Q3",
  #   quarter == "growth_1718q4" ~ "2017Q4",
  #   quarter == "growth_1819q1" ~ "2018Q1",
  #   quarter == "growth_1819q2" ~ "2018Q2",
  #   quarter == "growth_1819q3" ~ "2018Q3",
  #   quarter == "growth_1819q4" ~ "2018Q4",
  #   TRUE ~ quarter
  # ),
  # year = stri_sub(quarter, 1, 4))


##---- Projection ----
sh.imp <- sh.sub %>% 
  group_by(year, quarter, province, city, TA, atc4, molecule_desc, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(sh.growth.add, by = c("city", "packid")) %>% 
  mutate(units = units * growth_1920q1,
         sales = sales * growth_1920q1) %>% 
  filter(units > 0, sales > 0) %>% 
  mutate(year = "2020",
         quarter = gsub("2019", "2020", quarter)) %>% 
  group_by(year, quarter, province, city, TA, atc4, molecule_desc, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units) %>% 
  select(year, quarter, province, city, TA, atc4, molecule_desc, 
         packid, sales, units)

write_feather(sh.imp, "03_Outputs/07_AZ_CHC_Shanghai.feather")







