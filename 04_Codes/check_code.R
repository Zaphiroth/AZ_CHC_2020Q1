chk <- bind_rows(total.servier.raw, total.pfizer.raw, total.raw) %>% 
  filter(is.na(pchc)) %>% 
  filter(grepl("服务中心", 医院名称)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", 医院名称)) %>% 
  distinct(province, city, district, 医院名称) %>% 
  arrange(province, city)

write.xlsx(chk, "05_Internal_Review/Missing_PCHC_1.xlsx")


ta.raw <- read_feather("03_Outputs/01_AZ_CHC_Raw_with_TA.feather")
total.outside.imp <- read_feather("03_Outputs/03_AZ_CHC_Outside_Imp.feather")
total.proj <- read_feather("03_Outputs/05_AZ_CHC_Projection.feather")
total.price <- read_feather("03_Outputs/06_AZ_CHC_Price.feather")
sh.imp <- read_feather("03_Outputs/04_AZ_CHC_Shanghai_Imp.feather")
az.chc <- read.xlsx("03_Outputs/07_AZ_CHC_Result3.xlsx")

chk <- ta.raw %>% 
  filter(city %in% target.city) %>% 
  distinct(TA, quarter, city, pchc) %>% 
  count(TA, quarter, city, name = "n1") %>% 
  arrange(TA, city, quarter)

chk1 <- total.proj %>% 
  filter(city %in% target.city) %>% 
  distinct(TA, quarter, city, pchc) %>% 
  count(TA, quarter, city, name = "n2") %>% 
  arrange(TA, city, quarter)

chk2 <- chk %>% 
  full_join(chk1) %>% 
  mutate(n2 = ifelse(city == "上海", 413, n2),
         `universe / sample` = n2 / n1)

write.xlsx(chk2, "05_Internal_Review/Hospital_Rate.xlsx")


chk3 <- ta.raw %>% 
  filter(city %in% target.city) %>% 
  group_by(TA, city, quarter) %>% 
  summarise(raw = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

chk4 <- total.outside.imp %>% 
  filter(city %in% target.city, pchc %in% unique(total.price$pchc)) %>% 
  group_by(TA, city, quarter) %>% 
  summarise(sales2 = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

chk5 <- az.chc %>% 
  filter(`城市` %in% target.city) %>% 
  group_by(TA, city = `城市`, quarter = `年季`) %>% 
  summarise(final = sum(`金额（元）`, na.rm = TRUE)) %>% 
  ungroup()

chk6 <- chk3 %>% 
  # full_join(chk4) %>% 
  full_join(chk5) %>% 
  mutate(projection_rate = final / raw)

chk7 <- filter(chk6, is.na(sales1) | is.na(sales2) | is.na(sales3))

write.xlsx(chk6, "05_Internal_Review/Projection_Rate.xlsx")






chk <- bj.2017 %>% filter(is.na(packid)) %>% distinct(通用名, 剂型, 规格, 价格转换比, 生产企业)


# ##-- read in packid info
new_packid <- read.xlsx("03_Outputs/all_raw_data_m_v2_20191213-匹配结果.xlsx", sheet = "Sheet1") %>% 
  select(-c(商品名,索引,转换比距离,七位产品编码)) %>%
  mutate(packcode = str_pad(packcode, 7, side = "left", pad = "0")) %>%
  distinct()


new_packid_new <- new_packid%>%
  distinct() %>%
  group_by(匹配名, 剂型, 规格, 包装, 生产企业) %>%
  filter(row_number() == 1)#当这5列都相同，有多个packid的时候，取第一个packid。






chk <- read.csv("02_Inputs/2019/gs.csv")
chk1 <- read.xlsx("02_Inputs/2019/gs19xbase.xlsx")

chk2 <- chk1 %>% 
  filter(PERIOD %in% c(201901, 201902, 201903, 201904, 201905, 201906)) %>% 
  mutate(PERIOD = as.character(PERIOD)) %>% 
  select(IDCODE, PERIOD, PFC, UNIT = NOP, VALUE = TP)

chk3 <- chk %>% 
  select("IDCODE" = "锘縄D1", "PERIOD", "PFC", "UNIT", "VALUE") %>% 
  mutate(PERIOD = stri_replace_all_fixed(PERIOD, "-", "0"),
         PFC = as.character(PFC))

chk4 <- bind_rows(chk2, chk3)

write.xlsx(chk4, "02_Inputs/AZ广东2019Q1Q2Q3.xlsx")





chk1 <- bj.2017 %>% 
  filter(!is.na(pchc))

chk2 <- sd.servier.2017 %>% 
  filter(is.na(pchc)) %>% 
  mutate(cs = stri_sub(医院名称, -1, -1)) %>% 
  filter(grepl("服务中心", 医院名称)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", 医院名称))

chk1 <- distinct(chk2, province, city, district = 区县, 医院名称)




chk1 <- bj.2019q4 %>% 
  filter(!is.na(pchc))

chk2 <- bj.2019q4 %>% 
  filter(is.na(pchc1), is.na(pchc2)) %>% 
  mutate(cs = stri_sub(医院名称, -1, -1)) %>% 
  filter(grepl("服务中心", 医院名称)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", 医院名称))

chk3 <- distinct(chk2, province, city, district = 区县, 医院名称)





chk1 <- ah.2019q4 %>% 
  filter(!is.na(pchc))

chk2 <- ah.2019q4 %>% 
  filter(is.na(pchc1), is.na(pchc2)) %>% 
  mutate(cs = stri_sub(医院名称, -1, -1)) %>% 
  filter(grepl("服务中心", 医院名称)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", 医院名称))

chk4 <- distinct(chk2, province, city, district = 区县, 医院名称)



chk1 <- js.2019q4 %>% 
  filter(!is.na(pchc))

chk2 <- js.2019q4 %>% 
  filter(is.na(pchc1), is.na(pchc2)) %>% 
  mutate(cs = stri_sub(医院名称, -1, -1)) %>% 
  filter(grepl("服务中心", 医院名称)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", 医院名称))

chk5 <- distinct(chk2, province, city, district = 区县, 医院名称)






chk <- gd.2019 %>% 
  filter(is.na(hosp_name)) %>% 
  distinct(IDCODE)

write.csv(chk, "05_Internal_Review/GD_2019_IDCODE_not_matched.csv", row.names = FALSE)

chk1 <- gd.2019 %>% 
  filter(is.na(pchc)) %>% 
  mutate(cs = stri_sub(hosp_name, -1, -1)) %>% 
  filter(grepl("服务中心", hosp_name)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", hosp_name))

chk2 <- distinct(chk1, province, city, district, hosp_name)

write.csv(chkgd, "05_Internal_Review/GD_2019_PCHC_not_matched.csv", row.names = FALSE)



chk1 <- sd.2019q1 %>% 
  filter(!is.na(pchc))

chk2 <- sd.2019q1 %>% 
  filter(is.na(pchc1), is.na(pchc2)) %>% 
  mutate(cs = stri_sub(医院名称, -1, -1)) %>% 
  filter(grepl("服务中心", 医院名称)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", 医院名称))

chksd1 <- distinct(chk2, province, 医院名称)



chk1 <- sd.2019q2 %>% 
  filter(!is.na(pchc))

chk2 <- sd.2019q2 %>% 
  filter(is.na(pchc1), is.na(pchc2)) %>% 
  mutate(cs = stri_sub(医院名称, -1, -1)) %>% 
  filter(grepl("服务中心", 医院名称)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", 医院名称))

chksd2 <- distinct(chk2, province, 医院名称)


chk1 <- sd.2019q3 %>% 
  filter(!is.na(pchc))

chk2 <- sd.2019q3 %>% 
  filter(is.na(pchc1), is.na(pchc2)) %>% 
  mutate(cs = stri_sub(医院名称, -1, -1)) %>% 
  filter(grepl("服务中心", 医院名称)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", 医院名称))

chksd3 <- distinct(chk2, province, city, 医院名称)


chk <- bind_rows(chksd1, chksd2, chksd3) %>% 
  distinct()

write.csv(chk, "05_Internal_Review/SD_2019_PCHC_not_matched.csv", row.names = FALSE)



chk1 <- zj.2019q1 %>% 
  filter(!is.na(pchc))

chk2 <- zj.2019q1 %>% 
  filter(is.na(pchc1), is.na(pchc2)) %>% 
  mutate(cs = stri_sub(医院名称, -1, -1)) %>% 
  filter(grepl("服务中心", 医院名称)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", 医院名称))

chkzj1 <- distinct(chk2, province, city, 医院名称)


chk1 <- zj.2019q2 %>% 
  filter(!is.na(pchc))

chk2 <- zj.2019q2 %>% 
  filter(is.na(pchc1), is.na(pchc2)) %>% 
  mutate(cs = stri_sub(医院名称, -1, -1)) %>% 
  filter(grepl("服务中心", 医院名称)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊部|医务室|卫生所|中心分部|分院|分中心|计划生育|计生", 医院名称))

chkzj2 <- distinct(chk2, province, city, 医院名称)


chk <- bind_rows(chkzj1, chkzj2) %>% 
  distinct()

write.csv(chk, "05_Internal_Review/ZJ_2019_PCHC_not_matched.csv", row.names = FALSE)


chk1 <- pchc.universe %>% 
  select(-省, -单位名称, -ZS_Servier.name) %>% 
  distinct()

chk <- read.xlsx("05_Internal_Review/JS_PCHC_update.xlsx", sheet = 2) %>% 
  # select(-district) %>%
  filter(!is.na(PCHC_Code)) %>% 
  distinct() %>% 
  left_join(chk1, by = c("PCHC_Code")) %>% 
  distinct()

write.csv(chk, "js_update.csv", na = "", row.names = FALSE)


chk2 <- chk %>% 
  group_by(医院名称) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1)


chk <- ah.2019q4 %>% distinct(医院名称, pchc) %>% group_by(医院名称) %>% mutate(n = n()) %>% ungroup() %>% filter(n > 1)


bj.2019q1 <- file.2019[[1]] %>% 
  filter(`省份` == "北京市") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "北京",
         city = "北京",
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

bj.2019q2 <- file.2019[[2]] %>% 
  filter(`省份` == "北京市") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "北京",
         city = "北京",
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

bj.2019q3 <- file.2019[[7]] %>% 
  filter(`省份` == "北京市") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "北京",
         city = "北京",
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量之合计`, sales = `金额之合计`)

ah.2019q1 <- file.2019[[12]] %>% 
  filter(`省份` == "安徽省") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "安徽",
         city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

ah.2019q2 <- file.2019[[13]] %>% 
  filter(`省份` == "安徽省") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "安徽",
         city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

ah.2019q3 <- file.2019[[7]] %>% 
  filter(`省份` == "安徽省") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "安徽",
         # city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1),
         city = ifelse(is.na(city.x), city.y, city.x)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量之合计`, sales = `金额之合计`)

js.2019q1 <- file.2019[[3]] %>% 
  filter(`省份` == "江苏省") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "江苏",
         city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

js.2019q2 <- file.2019[[4]] %>% 
  filter(`省份` == "江苏省") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "江苏",
         city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

js.2019q3 <- file.2019[[8]] %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "江苏",
         # city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`),
         `价格转换比` = as.integer(`价格转换比`)) %>% 
  distinct() %>% 
  filter(!(`医院名称` %in% c("城中社区卫生服务中心", "兴隆社区卫生服务中心"))) %>% 
  left_join(pchc.mapping1, by = c("province", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1),
         city = ifelse(is.na(city.x), city.y, city.x)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

# 2019 Shandong
sd.2019q1 <- file.2019[[6]] %>% 
  filter(`季度` == "2019Q1") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "山东",
         # city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  filter(!(`医院名称` %in% c("城阳区城阳街道社区卫生服务中心"))) %>% 
  left_join(pchc.mapping1, by = c("province", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1),
         city = ifelse(is.na(city.x), city.y, city.x)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

sd.2019q2 <- file.2019[[6]] %>% 
  filter(`季度` == "2019Q2") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "山东",
         # city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  filter(!(`医院名称` %in% c("城阳区城阳街道社区卫生服务中心"))) %>% 
  left_join(pchc.mapping1, by = c("province", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1),
         city = ifelse(is.na(city.x), city.y, city.x)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

sd.2019q3 <- file.2019[[14]] %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "山东",
         city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  filter(!(`医院名称` %in% c("城阳区城阳街道社区卫生服务中心"))) %>% 
  left_join(pchc.mapping1, by = c("province", "city", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

# 2019 Zhejiang
zj.2019q1 <- file.2019[[5]] %>% 
  filter(`季度` == "2019Q1") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "浙江",
         city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)

zj.2019q2 <- file.2019[[5]] %>% 
  filter(`季度` == "2019Q2") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         province = "浙江",
         city = gsub("市", "", `城市`),
         `规格` = tolower(`规格`)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "医院名称" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "医院名称" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pack.mapping.19, by = c("匹配名", "剂型", "规格", "价格转换比" = "包装", "生产企业")) %>% 
  select(year, date, quarter = `季度`, province, city, pchc, `通用名`, `商品名`, 
         units = `数量`, sales = `金额`)


chk2 <- gi.raw %>% 
  filter(province == "北京")




