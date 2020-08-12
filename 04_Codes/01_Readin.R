# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC 2020Q1
# Purpose:      Readin raw data
# programmer:   Zhe Liu
# Date:         2020-06-02
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
# PCHC code
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20200507.xlsx", sheet = "PCHC")

pchc.mapping1 <- pchc.universe %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc1 = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.universe %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc2 = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- pchc.mapping1 %>% 
  group_by(pchc = pchc1) %>% 
  summarise(district = first(na.omit(district))) %>% 
  ungroup()

# molecule
ims_prod_ref <- fread("02_Inputs/cn_prod_ref_201903_1.txt") %>% 
  setDF() %>% 
  mutate(Pack_Id = str_pad(Pack_Id, 7, "left", pad = "0")) %>% 
  select(Pack_Id, NFC123_Code)

ims.mol.raw <- read.xlsx("02_Inputs/ims_chpa_to20Q1.xlsx", startRow = 3, cols = 1:21)

ims.mol1 <- ims.mol.raw[, 1:21] %>% 
  distinct() %>% 
  filter(!is.na(Pack_Id)) %>% 
  left_join(ims_prod_ref, by = "Pack_Id") %>% 
  select(packid = Pack_ID, Corp_ID, Corp_Desc, MNF_TYPE, MnfType_Desc, 
         Mnf_Desc, ATC4_Code, NFC123_Code, Prd_desc, Pck_Desc, 
         Molecule_Desc)

ims_prod_ref <- fread("02_Inputs/cn_prod_ref_201903_1.txt") %>% 
  setDF() %>% 
  mutate(Pack_Id = str_pad(Pack_Id, 7, "left", pad = "0"))

ims_mol_lkp_ref <- fread("02_Inputs/cn_mol_lkp_201903_1.txt") %>%
  setDF() %>%
  arrange(Pack_ID, Molecule_ID) %>%
  mutate(Pack_ID  = str_pad(Pack_ID , 7, "left", pad = "0"))

ims_mol_ref <- fread("02_Inputs/cn_mol_ref_201903_1.txt")

ims_corp_ref <- fread("02_Inputs/cn_corp_ref_201903_1.txt")

ims.mol2 <- ims_mol_lkp_ref %>%
  left_join(ims_mol_ref, by = c("Molecule_ID" = "Molecule_Id")) %>%
  arrange(Pack_ID, Molecule_Desc) %>%
  group_by(Pack_ID) %>%
  summarise(Molecule_Desc = paste(Molecule_Desc, collapse = "+")) %>%
  ungroup() %>%
  left_join(ims_prod_ref, by = c("Pack_ID" = "Pack_Id")) %>%
  left_join(ims_corp_ref, by = "Corp_ID") %>% 
  select(packid = Pack_ID, Corp_ID, Corp_Desc, ATC4_Code, NFC123_Code,
         Prd_desc, Pck_Desc, Molecule_Desc)

ims.mol <- ims.mol2 %>% 
  filter(!(packid %in% ims.mol1$packid)) %>% 
  mutate(Corp_ID = stri_pad_left(Corp_ID, 4, 0)) %>% 
  bind_rows(ims.mol1) %>% 
  filter(!(packid %in% c("4777502", "4777504")))

# target city
target.city <- c("北京", "上海", "杭州", "广州", "南京", "苏州", "宁波", 
                 "福州", "无锡", "温州", "济南", "青岛", "金华", "常州", 
                 "徐州", "台州", "厦门")


##---- Raw data ----
# Guangdong
gd.2020q1.raw <- read.csv("02_Inputs/Servier/gzs 20q1.csv")

gd.2020q1 <- gd.2020q1.raw %>% 
  mutate(year = stri_sub(period, 1, 4),
         date = stri_replace_all_fixed(period, "-", "0"),
         quarter = stri_paste(year, "Q1"),
         province = "广东",
         city = "广州",
         packid = stri_pad_left(pfc, 7, 0)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "name" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "name" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pchc.mapping3, by = "pchc") %>% 
  select(year, date, quarter, province, city, district, pchc, packid, 
         units = unit, sales = value)

# Beijing
bj.2020q1.raw <- read.xlsx("02_Inputs/Servier/beijing2020Q1_packid_moleinfo.xlsx")

bj.2020q1 <- bj.2020q1.raw %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         quarter = as.character(`季度`),
         province = "北京",
         city = "北京",
         hospital = `医院名称`,
         packid = stri_pad_left(packcode, 7, 0)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(pchc.mapping3, by = "pchc") %>% 
  select(year, date, quarter, province, city, district, pchc, packid, 
         units = `数量`, sales = `金额`)

# Shanghai
sh.2020q1.raw <- read.xlsx("02_Inputs/Servier/shanghai_201805_202004_packid_moleinfo_PCHC.xlsx")

sh.2020q1 <- sh.2020q1.raw %>% 
  mutate(year = as.character(Year),
         date = as.character(Month),
         quarter = ifelse(stri_sub(date, 5, 6) %in% c("01", "02", "03"), paste0(year, "Q1"), 
                          ifelse(stri_sub(date, 5, 6) %in% c("04", "05", "06"), paste0(year, "Q2"), 
                                 ifelse(stri_sub(date, 5, 6) %in% c("07", "08", "09"), paste0(year, "Q3"), 
                                        ifelse(stri_sub(date, 5, 6) %in% c("10", "11", "12"), paste0(year, "Q4"), 
                                               NA_character_)))),
         province = "上海",
         city = "上海",
         seg_city = paste0(`城市`, `区县`),
         pchc = PCHC_Code,
         packid = packcode) %>% 
  distinct() %>% 
  left_join(pchc.mapping3, by = "pchc") %>% 
  select(year, date, quarter, province, city, district, pchc, packid, 
         units = `数量`, sales = `金额`)

# history
history.az <- read_feather("02_Inputs/History/01_AZ_CHC_Total_Raw.feather")
history.pfizer <- read_feather("02_Inputs/History/01_Pfizer_CHC_Total_Raw.feather")
history.servier <- read_feather("02_Inputs/History/01_Servier_CHC_Total_Raw.feather")

# total
total.raw <- bind_rows(gd.2020q1, bj.2020q1, sh.2020q1,
                       history.az, history.pfizer, history.servier) %>% 
  filter(pchc != "#N/A", units > 0, sales > 0, quarter %in% c("2019Q1", "2020Q1")) %>% 
  filter(city %in% c("广州", "北京", "上海")) %>% 
  group_by(year, date, quarter, pchc, packid) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district)),
            units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(ims.mol, by = "packid") %>% 
  select(year, date, quarter, province, city, district, pchc, packid, 
         units, sales, Corp_ID, Corp_Desc, MNF_TYPE, MnfType_Desc, 
         Mnf_Desc, ATC4_Code, NFC123_Code, Prd_desc, Pck_Desc, Molecule_Desc)

write_feather(total.raw, "03_Outputs/01_AZ_CHC_Total_Raw.feather")


##---- TA ----
# market
market.mapping.raw <- read.xlsx("02_Inputs/Market_Def_2020_CHC_0317.xlsx", sheet = "MKT DEF")

market.mapping <- market.mapping.raw[, -1] %>% 
  select(`小市场`, `大市场`, `购买方式`, flag_mkt = flag) %>% 
  distinct() %>% 
  group_by(`小市场`) %>% 
  mutate(`购买方式` = paste(unique(`购买方式`), collapse = "+")) %>% 
  ungroup()

market.cndrug <- read.xlsx("02_Inputs/Market_Def_2020_CHC_0317.xlsx", sheet = "XZK-其他降脂中药")

# GI
gi.raw <- total.raw %>% 
  mutate(flag_mkt = case_when(
    Molecule_Desc %in% c("ESOMEPRAZOLE", "OMEPRAZOLE", "PANTOPRAZOLE", 
                         "LANSOPRAZOLE", "RABEPRAZOLE") ~ 1,
    
    Prd_desc %in% c("YI LI AN           LZB") ~ 2,
    
    stri_sub(ATC4_Code, 1, 4) == "A06A" & 
      stri_sub(NFC123_Code, 1, 1) %in% c("A", "B", "D") ~ 3,
    
    Prd_desc == "SHU TAI QING       S5J" & 
      Molecule_Desc == "MACROGOL(S)+POTASSIUM+SODIUM" & 
      Corp_Desc == "STAIDSON BEIJING" & 
      stri_sub(packid, 1, 5) == "32464" ~ 4,
    
    Prd_desc == "MOVICOL            NR-" & 
      Molecule_Desc == "MACROGOL(S)+POTASSIUM+SODIUM" & 
      Corp_Desc == "NORGINE LIM UK" & 
      stri_sub(packid, 1, 5) == "66157" ~ 4,
    
    Prd_desc == "FORLAX             IPS" & 
      Molecule_Desc == "MACROGOL(S)" & 
      Corp_Desc == "IPSEN" & 
      stri_sub(packid, 1, 5) == "14351" ~ 4,
    
    Prd_desc == "CHANG SONG         C&Q" & 
      Molecule_Desc == "MACROGOL(S)" & 
      Corp_Desc == "CQ.PHARSCIN PHARM" & 
      stri_sub(packid, 1, 5) == "32293" ~ 4,
    
    Prd_desc == "RUN KE LONG        H3U" & 
      Molecule_Desc == "MACROGOL(S)" & 
      Corp_Desc == "HN.WARRANT PHARM" & 
      stri_sub(packid, 1, 5) == "32652" ~ 4,
    
    Prd_desc == "MACROGOL 4000 POWD HMA" & 
      Molecule_Desc == "MACROGOL(S)" & 
      Corp_Desc == "HB.MAYINGLONG PH" & 
      stri_sub(packid, 1, 5) == "41490" ~ 4,
    
    Prd_desc == "YOU SAI LE         CQ&" & 
      Molecule_Desc == "MACROGOL(S)" & 
      Corp_Desc == "CQ.SINO BIOPHARM" & 
      stri_sub(packid, 1, 5) == "57040" ~ 4,
    
    TRUE ~ 0
  )) %>% 
  filter(flag_mkt != 0) %>% 
  mutate(TA = "GI") %>% 
  group_by(year, date, quarter, province, city, pchc, TA, 
           atc4 = ATC4_Code, molecule_desc = Molecule_Desc, packid, flag_mkt) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = "flag_mkt")

# RE
re.raw <- total.raw %>% 
  mutate(flag_mkt = case_when(
    Prd_desc %in% c("SYMBICORT TURBUHAL AZN", "SERETIDE           GSK", 
                    "FOSTER             C5I", "RELVAR") ~ 5,
    
    Prd_desc == "BRICANYL           AZM" & 
      Molecule_Desc == "TERBUTALINE" & 
      stri_sub(packid, 1, 5) == "14018" & 
      stri_sub(NFC123_Code, 1, 2) == "RG" & 
      stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 6,
    
    Prd_desc == "SU SHUN            SU9" & 
      Molecule_Desc == "TERBUTALINE" & 
      stri_sub(packid, 1, 5) == "16352" & 
      stri_sub(NFC123_Code, 1, 2) == "FM" & 
      stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 6,
    
    Prd_desc == "SU SHUN            SU9" & 
      Molecule_Desc == "TERBUTALINE" & 
      stri_sub(packid, 1, 5) == "16352" & 
      stri_sub(NFC123_Code, 1, 2) == "FQ" & 
      stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 6,
    
    Prd_desc == "SALBUTAMOL SULFATE SSZ" & 
      Molecule_Desc == "SALBUTAMOL" & 
      stri_sub(packid, 1, 5) == "56285" & 
      stri_sub(NFC123_Code, 1, 2) == "RG" & 
      stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 6,
    
    Prd_desc == "VENTOLIN           GSK" & 
      Molecule_Desc == "SALBUTAMOL" & 
      stri_sub(packid, 1, 5) == "02003" & 
      stri_sub(NFC123_Code, 1, 2) == "RG" & 
      stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 6,
    
    Prd_desc == "SALBUTAMOL         JO-" & 
      Molecule_Desc == "SALBUTAMOL" & 
      stri_sub(packid, 1, 5) == "01734" & 
      stri_sub(NFC123_Code, 1, 2) == "FM" & 
      stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 6,
    
    Prd_desc == "SALBUTAMOL  SULFAT SHT" & 
      Molecule_Desc == "SALBUTAMOL" & 
      stri_sub(packid, 1, 5) == "52133" & 
      stri_sub(NFC123_Code, 1, 2) == "FQ" & 
      stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 6,
    
    Prd_desc == "DA FEN KE CHUANG   SZA" & 
      Molecule_Desc == "SALBUTAMOL" & 
      stri_sub(packid, 1, 5) == "36434" & 
      stri_sub(NFC123_Code, 1, 2) == "RG" & 
      stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 6,
    
    Prd_desc == "SALBUTAMOL  SULFAT SFU" & 
      Molecule_Desc == "SALBUTAMOL" & 
      stri_sub(packid, 1, 5) == "55281" & 
      stri_sub(NFC123_Code, 1, 2) == "FQ" & 
      stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 6,
    
    Prd_desc == "ATROVENT           B.I" & 
      Molecule_Desc == "IPRATROPIUM BROMIDE" & 
      stri_sub(packid, 1, 5) == "04354" & 
      stri_sub(NFC123_Code, 1, 2) == "RG" & 
      stri_sub(ATC4_Code, 1, 4) == "R03K" ~ 6,
    
    Prd_desc == "COMBIVENT          B.I" & 
      Molecule_Desc == "IPRATROPIUM BROMIDE+SALBUTAMOL" & 
      stri_sub(packid, 1, 5) == "07319" & 
      stri_sub(NFC123_Code, 1, 2) == "RG" & 
      stri_sub(ATC4_Code, 1, 4) == "R03L" ~ 6,
    
    stri_sub(ATC4_Code, 1, 4) == "R05C" & NFC123_Code != "ABD" ~ 7,
    
    stri_sub(ATC4_Code, 1, 3) == "R03" ~ 8,
    
    # stri_sub(ATC4_Code, 1, 4) == "R05C" ~ 8,
    
    TRUE ~ 0
  )) %>% 
  filter(flag_mkt != 0) %>% 
  mutate(TA = "RE") %>% 
  group_by(year, date, quarter, province, city, pchc, TA, 
           atc4 = ATC4_Code, molecule_desc = Molecule_Desc, packid, flag_mkt) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = "flag_mkt")

# CV
cv.cndrug <- total.raw %>% 
  filter(stri_sub(packid, 1, 5) %in% market.cndrug$PROD_COD) %>% 
  group_by(year, date, quarter, province, city, pchc, ATC4_Code, 
           Molecule_Desc, packid, flag_mkt = 19) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

cv.raw1 <- total.raw %>% 
  mutate(flag_mkt = case_when(
    stri_sub(ATC4_Code, 1, 4) == "C07A" & 
      stri_sub(NFC123_Code, 1, 1) %in% c("A", "B") ~ 9,
    
    Molecule_Desc == "IVABRADINE" ~ 10,
    
    stri_sub(ATC4_Code, 1, 4) == "C08A" ~ 11,
    
    Prd_desc == "EXFORGE            NVR" ~ 12,
    
    stri_sub(ATC4_Code, 1, 4) %in% c("C01D") ~ 13,
    
    stri_sub(ATC4_Code, 1, 3) %in% c("C03", "C07", "C08", "C09") ~ 13,
    
    # Prd_desc == "CADUET             PFZ" ~ 13,
    
    TRUE ~ 0
  )) %>% 
  filter(flag_mkt != 0) %>% 
  mutate(TA = "CV") %>% 
  group_by(year, date, quarter, province, city, pchc, TA, 
           atc4 = ATC4_Code, molecule_desc = Molecule_Desc, packid, flag_mkt) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = "flag_mkt")

cv.raw2 <- total.raw %>% 
  mutate(flag_mkt = case_when(
    ATC4_Code == "B01C2" ~ 16,
    
    Prd_desc %in% c("CADUET             PFZ", "XUE ZHI KANG       BWX", 
                    "ZHI BI TAI         DJP", "JIANG ZHI TONG MAI YYK") ~ 18,
    
    TRUE ~ 0
  )) %>% 
  filter(flag_mkt != 0) %>% 
  bind_rows(cv.cndrug) %>% 
  mutate(TA = "CV") %>% 
  group_by(year, date, quarter, province, city, pchc, TA, 
           atc4 = ATC4_Code, molecule_desc = Molecule_Desc, packid, flag_mkt) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = "flag_mkt")

cv.raw3 <- total.raw %>% 
  mutate(flag_mkt = case_when(
    ATC4_Code == "C10A1" ~ 14,
    
    Prd_desc %in% c("CADUET             PFZ", "VYTORIN            MSD", 
                    "EZETROL            SG7") ~ 15,
    
    Molecule_Desc %in% c("ATORVASTATIN", "ROSUVASTATIN", "SIMVASTATIN", 
                         "PITAVASTATIN", "PRAVASTATIN", "FLUVASTATIN", 
                         "EZETIMIBE", "LOVASTATIN", "EZETIMIBE+SIMVASTATIN") ~ 17,
    
    TRUE ~ 0
  )) %>% 
  filter(flag_mkt != 0) %>% 
  mutate(TA = "CV") %>% 
  group_by(year, date, quarter, province, city, pchc, TA, 
           atc4 = ATC4_Code, molecule_desc = Molecule_Desc, packid, flag_mkt) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = "flag_mkt") %>% 
  filter(!(packid %in% unique(c(cv.raw1$packid, cv.raw2$packid))))

# cv.raw4 <- total.raw %>% 
#   mutate(flag_mkt = case_when(
#     ATC4_Code == "C10A1" ~ 14,
#     
#     Prd_desc %in% c("CADUET             PFZ", "VYTORIN            MSD", 
#                     "EZETROL            SG7") ~ 15,
#     
#     Molecule_Desc %in% c("ATORVASTATIN", "ROSUVASTATIN", "SIMVASTATIN", 
#                          "PITAVASTATIN", "PRAVASTATIN", "FLUVASTATIN", 
#                          "EZETIMIBE", "LOVASTATIN", "EZETIMIBE+SIMVASTATIN") ~ 17,
#     
#     TRUE ~ 0
#   )) %>% 
#   filter(flag_mkt != 0, year %in% c("2019")) %>% 
#   mutate(TA = "CV") %>% 
#   group_by(year, date, quarter, province, city, pchc, TA, 
#            atc4 = ATC4_Code, molecule_desc = Molecule_Desc, packid, flag_mkt) %>% 
#   summarise(units = sum(units, na.rm = TRUE),
#             sales = sum(sales, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   left_join(market.mapping, by = "flag_mkt") %>% 
#   filter(!(packid %in% unique(c(cv.raw1$packid, cv.raw2$packid))))

cv.raw <- bind_rows(cv.raw1, cv.raw2, cv.raw3)

# DM
dm.raw <- total.raw %>% 
  mutate(flag_mkt = case_when(
    Prd_desc %in% c("EUCREAS            NVR", "KOMBIGLYZE XR      AZN", 
                    "TRAJENTA DUO       B.I", "ONGLYZA            BMS", 
                    "JANUVIA            MSD", "GALVUS             NVR", 
                    "TRAJENTA           B.I", "NESINA             TAK", 
                    "JANUMET            MSD") ~ 21,
    
    Prd_desc %in% c("FORXIGA            AZN", "JARDIANCE          B.I", 
                    "INVOKANA           MCK") ~ 22,
    
    stri_sub(ATC4_Code, 1, 4) %in% c("A10L", "A10H", "A10M", "A10J", 
                                     "A10K", "A10S", "A10N") ~ 20,
    
    TRUE ~ 0
  )) %>% 
  filter(flag_mkt != 0) %>% 
  mutate(TA = "DM") %>% 
  group_by(year, date, quarter, province, city, pchc, TA, 
           atc4 = ATC4_Code, molecule_desc = Molecule_Desc, packid, flag_mkt) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = "flag_mkt")

# Renal
renal.raw <- total.raw %>% 
  mutate(flag_mkt = case_when(
    !(Molecule_Desc %in% c("FOLIC ACID", "CYANOCOBALAMIN+FOLIC ACID+NICOTINAMIDE")) & 
      stri_sub(ATC4_Code, 1, 3) == "B03" ~ 23,
    
    Molecule_Desc == "POLYSTYRENE SULFONATE" ~ 24,
    
    Prd_desc == "Lokelma" ~ 25,
    
    TRUE ~ 0
  )) %>% 
  filter(flag_mkt != 0) %>% 
  mutate(TA = "Renal") %>% 
  group_by(year, date, quarter, province, city, pchc, TA, 
           atc4 = ATC4_Code, molecule_desc = Molecule_Desc, packid, flag_mkt) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(market.mapping, by = "flag_mkt")

# bind
ta.raw <- bind_rows(gi.raw, re.raw, cv.raw, dm.raw, renal.raw)

write_feather(ta.raw, "03_Outputs/01_AZ_CHC_Raw_with_TA.feather")
write.xlsx(ta.raw, "03_Outputs/01_AZ_CHC_Raw_with_TA.xlsx")

