# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC 2020Q1
# Purpose:      Projection
# programmer:   Zhe Liu
# Date:         2020-06-02
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
# target city
target.city <- c("北京", "上海", "杭州", "广州", "南京", "苏州", "宁波", 
                 "福州", "无锡", "温州", "济南", "青岛", "金华", "常州", 
                 "徐州", "台州", "厦门")

# city segment
segment <- read_xlsx("02_Inputs/seg_45cities.xlsx") %>% 
  mutate(seg_city = if_else(city == "上海", paste0(city, district), city)) %>% 
  select(seg_city, seg = seg_up)

# hospital universe
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20200507.xlsx", sheet = "PCHC")

hospital.universe <- pchc.universe %>% 
  filter(`地级市` %in% target.city) %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            district = first(na.omit(`区[县/县级市】`)),
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup() %>% 
  filter(!is.na(est)) %>% 
  mutate(seg_city = if_else(city == "上海", paste0(city, district), city)) %>% 
  left_join(segment, by = "seg_city") %>% 
  mutate(seg = if_else(is.na(seg), 1, seg))


##---- Projection ----
# projection data
proj.raw <- total.imp %>% 
  filter(city %in% target.city, quarter == "2020Q1")

# quarter sales
proj.quarter <- proj.raw %>% 
  group_by(quarter, province, city, pchc, TA, atc4, molecule_desc, packid) %>% 
  summarise(panel_sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# price
proj.price <- proj.raw %>% 
  filter(units > 0) %>% 
  group_by(quarter, city, packid) %>% 
  summarise(price = sum(sales, na.rm = TRUE) / sum(units, na.rm = TRUE)) %>% 
  ungroup()

# universe set
universe.set <- merge(distinct(hospital.universe, pchc, seg, est), 
                      distinct(proj.raw, quarter)) %>% 
  merge(distinct(proj.raw, province, city, TA, atc4, molecule_desc, packid)) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  mutate(seg = ifelse(is.na(seg), 1, seg),
         panel = ifelse(pchc %in% unique(ta.raw$pchc), 1, 0),
         panel = ifelse(is.na(est), 0, panel))

filtered.set <- universe.set %>% 
  filter(panel == 1) %>% 
  left_join(proj.quarter, by = c("province", "city", "pchc", "quarter", 
                                 "TA", "atc4", "molecule_desc", "packid")) %>% 
  mutate(panel_sales = ifelse(is.na(panel_sales), 0, panel_sales))

# projection parameter
proj.parm <- data.table(filtered.set)[, {
  ux <- mean(est)
  uy <- mean(panel_sales)
  slope <- uy / ux
  intercept <- 0
  predict_sales <- slope * est
  spearman_cor <- cor(panel_sales, predict_sales, method = "spearman")
  list(slope = slope, intercept = intercept, spearman_cor = spearman_cor)
}, by = list(city, quarter, packid, TA, seg)]

# QC: TRUE is no multiple match
sum(universe.set$panel_sales, na.rm = TRUE) <= sum(proj.quarter$panel_sales, na.rm = TRUE)


##---- Result ----
proj.result <- universe.set %>% 
  left_join(proj.quarter, by = c("province", "city", "pchc", "quarter", 
                                 "TA", "molecule_desc", "atc4", "packid")) %>% 
  mutate(panel_sales = ifelse(is.na(panel_sales), 0, panel_sales)) %>% 
  left_join(proj.parm, by = c("quarter", "city", "TA", "packid", "seg")) %>% 
  mutate(predict_sales = est * slope + intercept,
         final_sales = ifelse(panel == 0, predict_sales, panel_sales)) %>% 
  filter(final_sales > 0) %>% 
  mutate(year = stri_sub(quarter, 1, 4)) %>% 
  group_by(year, quarter, province, city, pchc, TA, atc4, molecule_desc, packid) %>% 
  summarise(sales = sum(final_sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(city %in% c("北京"))

total.proj <- total.imp %>% 
  filter(city %in% c("广州"), quarter %in% c("2020Q1")) %>% 
  bind_rows(proj.result) %>% 
  group_by(year, quarter, province, city, pchc, TA, atc4, molecule_desc, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write_feather(total.proj, "03_Outputs/05_AZ_CHC_Projection.feather")



