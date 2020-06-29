# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC
# Purpose:      Summary
# programmer:   Zhe Liu
# Date:         26-02-2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
# pack
product.info <- read.xlsx("02_Inputs/Product standardization master data-A-S-0313.xlsx") %>% 
  select(-MNF_TYPE, -MNF_ID) %>% 
  distinct()

# pack-market
# gi.market <- distinct(gi.raw, packid, `小市场`, `大市场`, `购买方式`)
# re.market <- distinct(re.raw, packid, `小市场`, `大市场`, `购买方式`)
# cv.market <- distinct(cv.raw, packid, `小市场`, `大市场`, `购买方式`)
# dm.market <- distinct(dm.raw, packid, `小市场`, `大市场`, `购买方式`)
# renal.market <- distinct(renal.raw, packid, `小市场`, `大市场`, `购买方式`)

# market
market.info <- ta.raw %>% 
  distinct(packid, `小市场`, `大市场`, `购买方式`) %>% 
  filter(!(packid == "1401802" & `小市场` == "NEB Market"))

# product
prod.info.raw <- read.xlsx("02_Inputs/packid_prod_20181112.xlsx")

prod.info <- prod.info.raw %>% 
  select(pack_id, atc4_code = ATC4_Code, gene_name, ims_product_cn, pck_desc = Pck_Desc, ims_corp, PckSize_Desc) %>% 
  mutate(dosage = str_trim(str_replace(pck_desc, "[0-9]{1,}.{1,}", ""), "right"),
         specification = sapply(pck_desc, function(x) {
           paste(str_extract_all(x, "[0-9]{1,}[a-zA-Z]{1,}", simplify = TRUE), collapse = " ")
         }))

# corp
corp.cn <- read_xls("02_Inputs/Corp E & C name.xls") %>% 
  distinct(Corporation, Corporation_C)

# VBP flag
vbp1 <- read.xlsx("02_Inputs/VBP匹配 for AZ CHC.xlsx", sheet = 2) %>% 
  mutate(city = gsub("市", "", `城市`),
         packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  select(city, packid, VBP_Excu1 = VBP_Excu, VBP1 = VBP) %>% 
  distinct()

vbp2 <- read.xlsx("02_Inputs/VBP匹配 for AZ CHC.xlsx", sheet = 3) %>% 
  mutate(province = gsub("省", "", `省份`),
         packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  select(province, packid, VBP_Excu2 = VBP_Excu, VBP2 = VBP) %>% 
  distinct()

vbp3 <- read.xlsx("02_Inputs/VBP匹配 for AZ CHC.xlsx", sheet = 4) %>% 
  mutate(packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  select(packid, VBP3 = VBP)


##---- Result ----
# history
az.history <- read.xlsx("02_Inputs/AZ_CHC_2017Q1_2019Q4_Delivery_Final_0610（HTN+Crestor Market）.xlsx")
colnames(az.history) <- gsub("[.]", " ", colnames(az.history))

az.history.result <- az.history %>% 
  group_by(year = `年`, quarter = `年季`, province = `省份`, city = `城市`, TA, 
           atc4 = `ATC Code IV`, packid = Pack_ID) %>% 
  summarise(units = sum(`数量（盒）`, na.rm = TRUE), 
            sales = sum(`金额（元）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(atc4 = if_else(packid == "Others", 
                        "Others", 
                        atc4))

# 2020Q1
az.cndrug <- total.price %>% 
  filter(stri_sub(packid, 1, 5) %in% market.cndrug$PROD_COD) %>% 
  group_by(year, quarter, province, city) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units,
         TA = "Others",
         atc4 = "Others",
         packid = "Others")

az.chc <- total.price %>% 
  filter(!(stri_sub(packid, 1, 5) %in% market.cndrug$PROD_COD)) %>% 
  group_by(year, quarter, province, city, TA, atc4, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(az.cndrug, sh.imp, az.history.result) %>% 
  mutate(price = sales / units) %>% 
  left_join(ims.mol, by = "packid") %>% 
  left_join(product.info, by = c("packid" = "PACK_ID")) %>% 
  left_join(prod.info, by = c("packid" = "pack_id")) %>% 
  left_join(market.info, by = "packid") %>% 
  left_join(vbp1, by = c("city", "packid")) %>% 
  left_join(vbp2, by = c("province", "packid")) %>% 
  left_join(vbp3, by = c("packid")) %>% 
  left_join(corp.cn, by = c("Corp_Desc" = "Corporation")) %>% 
  mutate(MOLE_NAME_CH = if_else(is.na(MOLE_NAME_CH), gene_name, MOLE_NAME_CH),
         PROD_NAME_CH = if_else(is.na(PROD_NAME_CH), ims_product_cn, PROD_NAME_CH),
         DOSAGE = if_else(is.na(DOSAGE), dosage, DOSAGE),
         SPEC = if_else(is.na(SPEC), specification, SPEC),
         PACK = if_else(is.na(PACK), PckSize_Desc, PACK),
         CORP_NAME_CH = if_else(is.na(CORP_NAME_CH), ims_corp, CORP_NAME_CH),
         VBP_Excu = ifelse(is.na(VBP_Excu1), VBP_Excu2, VBP_Excu1),
         VBP = ifelse(is.na(VBP1), VBP2, VBP1),
         VBP = ifelse(is.na(VBP), VBP3, VBP),
         `单位` = NA,
         `包装` = NA,
         `退货数量（盒）` = 0,
         `退货金额（元）` = 0,
         `单支单片` = PACK * units,
         `IMS 药品ID` = stri_paste(stri_sub(packid, 1, 5), "-", packid),
         `IMS 药品ID` = if_else(packid == "Others", "Others", `IMS 药品ID`),
         units = round(units),
         sales = round(sales, 2)) %>% 
  mutate(`PPI (Oral/IV) Market` = case_when(
           Molecule_Desc %in% c("ESOMEPRAZOLE", "OMEPRAZOLE", "PANTOPRAZOLE", 
                                "LANSOPRAZOLE", "RABEPRAZOLE") ~ 1,
           
           Prd_desc %in% c("YI LI AN           LZB") ~ 1,
           
           TRUE ~ 0
         ),
         `Linaclotide Market` = case_when(
           stri_sub(ATC4_Code, 1, 4) == "A06A" & 
             stri_sub(NFC123_Code, 1, 1) %in% c("A", "B", "D") ~ 1,
           
           Prd_desc == "SHU TAI QING       S5J" & 
             Molecule_Desc == "MACROGOL(S)+POTASSIUM+SODIUM" & 
             Corp_Desc == "STAIDSON BEIJING" & 
             stri_sub(packid, 1, 5) == "32464" ~ 1,
           
           Prd_desc == "MOVICOL            NR-" & 
             Molecule_Desc == "MACROGOL(S)+POTASSIUM+SODIUM" & 
             Corp_Desc == "NORGINE LIM UK" & 
             stri_sub(packid, 1, 5) == "66157" ~ 1,
           
           Prd_desc == "FORLAX             IPS" & 
             Molecule_Desc == "MACROGOL(S)" & 
             Corp_Desc == "IPSEN" & 
             stri_sub(packid, 1, 5) == "14351" ~ 1,
           
           Prd_desc == "CHANG SONG         C&Q" & 
             Molecule_Desc == "MACROGOL(S)" & 
             Corp_Desc == "CQ.PHARSCIN PHARM" & 
             stri_sub(packid, 1, 5) == "32293" ~ 1,
           
           Prd_desc == "RUN KE LONG        H3U" & 
             Molecule_Desc == "MACROGOL(S)" & 
             Corp_Desc == "HN.WARRANT PHARM" & 
             stri_sub(packid, 1, 5) == "32652" ~ 1,
           
           Prd_desc == "MACROGOL 4000 POWD HMA" & 
             Molecule_Desc == "MACROGOL(S)" & 
             Corp_Desc == "HB.MAYINGLONG PH" & 
             stri_sub(packid, 1, 5) == "41490" ~ 1,
           
           Prd_desc == "YOU SAI LE         CQ&" & 
             Molecule_Desc == "MACROGOL(S)" & 
             Corp_Desc == "CQ.SINO BIOPHARM" & 
             stri_sub(packid, 1, 5) == "57040" ~ 1,
           
           TRUE ~ 0
         ),
         `Symbicort Market` = case_when(
           Prd_desc %in% c("SYMBICORT TURBUHAL AZN", "SERETIDE           GSK", 
                           "FOSTER             C5I", "RELVAR") ~ 1,
           
           TRUE ~ 0
         ),
         `NEB Market` = case_when(
           Prd_desc == "BRICANYL           AZM" & 
             Molecule_Desc == "TERBUTALINE" & 
             stri_sub(packid, 1, 5) == "14018" & 
             stri_sub(NFC123_Code, 1, 2) == "RG" & 
             stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 1,
           
           Prd_desc == "SU SHUN            SU9" & 
             Molecule_Desc == "TERBUTALINE" & 
             stri_sub(packid, 1, 5) == "16352" & 
             stri_sub(NFC123_Code, 1, 2) == "FM" & 
             stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 1,
           
           Prd_desc == "SU SHUN            SU9" & 
             Molecule_Desc == "TERBUTALINE" & 
             stri_sub(packid, 1, 5) == "16352" & 
             stri_sub(NFC123_Code, 1, 2) == "FQ" & 
             stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 1,
           
           Prd_desc == "SALBUTAMOL SULFATE SSZ" & 
             Molecule_Desc == "SALBUTAMOL" & 
             stri_sub(packid, 1, 5) == "56285" & 
             stri_sub(NFC123_Code, 1, 2) == "RG" & 
             stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 1,
           
           Prd_desc == "VENTOLIN           GSK" & 
             Molecule_Desc == "SALBUTAMOL" & 
             stri_sub(packid, 1, 5) == "02003" & 
             stri_sub(NFC123_Code, 1, 2) == "RG" & 
             stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 1,
           
           Prd_desc == "SALBUTAMOL         JO-" & 
             Molecule_Desc == "SALBUTAMOL" & 
             stri_sub(packid, 1, 5) == "01734" & 
             stri_sub(NFC123_Code, 1, 2) == "FM" & 
             stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 1,
           
           Prd_desc == "SALBUTAMOL  SULFAT SHT" & 
             Molecule_Desc == "SALBUTAMOL" & 
             stri_sub(packid, 1, 5) == "52133" & 
             stri_sub(NFC123_Code, 1, 2) == "FQ" & 
             stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 1,
           
           Prd_desc == "DA FEN KE CHUANG   SZA" & 
             Molecule_Desc == "SALBUTAMOL" & 
             stri_sub(packid, 1, 5) == "36434" & 
             stri_sub(NFC123_Code, 1, 2) == "RG" & 
             stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 1,
           
           Prd_desc == "SALBUTAMOL  SULFAT SFU" & 
             Molecule_Desc == "SALBUTAMOL" & 
             stri_sub(packid, 1, 5) == "55281" & 
             stri_sub(NFC123_Code, 1, 2) == "FQ" & 
             stri_sub(ATC4_Code, 1, 4) == "R03A" ~ 1,
           
           Prd_desc == "ATROVENT           B.I" & 
             Molecule_Desc == "IPRATROPIUM BROMIDE" & 
             stri_sub(packid, 1, 5) == "04354" & 
             stri_sub(NFC123_Code, 1, 2) == "RG" & 
             stri_sub(ATC4_Code, 1, 4) == "R03K" ~ 1,
           
           Prd_desc == "COMBIVENT          B.I" & 
             Molecule_Desc == "IPRATROPIUM BROMIDE+SALBUTAMOL" & 
             stri_sub(packid, 1, 5) == "07319" & 
             stri_sub(NFC123_Code, 1, 2) == "RG" & 
             stri_sub(ATC4_Code, 1, 4) == "R03L" ~ 1,
           
           TRUE ~ 0
         ),
         `Non-Oral Expectorant Market` = case_when(
           stri_sub(ATC4_Code, 1, 4) == "R05C" & NFC123_Code != "ABD" ~ 1,
           
           TRUE ~ 0
         ),
         `Respules (Asthma&COPD) Market` = case_when(
           stri_sub(ATC4_Code, 1, 3) == "R03" ~ 1,
           
           TRUE ~ 0
         ),
         `Betaloc Oral Market` = case_when(
           stri_sub(ATC4_Code, 1, 4) == "C07A" & 
             stri_sub(NFC123_Code, 1, 1) %in% c("A", "B") ~ 1,
           
           Molecule_Desc == "IVABRADINE" ~ 1,
           
           TRUE ~ 0
         ),
         `Plendil Market` = case_when(
           stri_sub(ATC4_Code, 1, 4) == "C08A" ~ 1,
           
           Prd_desc == "EXFORGE            NVR" ~ 1,
           
           TRUE ~ 0
         ),
         `HTN Market` = case_when(
           stri_sub(ATC4_Code, 1, 4) %in% c("C01D") ~ 1,
           
           stri_sub(ATC4_Code, 1, 3) %in% c("C03", "C07", "C08", "C09") ~ 1,
           
           Prd_desc == "CADUET             PFZ" ~ 1,
           
           TRUE ~ 0
         ),
         `Crestor Market` = case_when(
           ATC4_Code == "C10A1" ~ 1,
           
           Prd_desc %in% c("CADUET             PFZ", "VYTORIN            MSD", 
                           "EZETROL            SG7") ~ 1,
           
           TRUE ~ 0
         ),
         `Brilinta Market` = case_when(
           ATC4_Code == "B01C2" ~ 1,
           
           TRUE ~ 0
         ),
         `XZK Market` = case_when(
           Molecule_Desc %in% c("ATORVASTATIN", "ROSUVASTATIN", "SIMVASTATIN", 
                                "PITAVASTATIN", "PRAVASTATIN", "FLUVASTATIN", 
                                "EZETIMIBE", "LOVASTATIN", "EZETIMIBE+SIMVASTATIN") ~ 1,
           
           Prd_desc %in% c("CADUET             PFZ", "XUE ZHI KANG       BWX", 
                           "ZHI BI TAI         DJP", "JIANG ZHI TONG MAI YYK") ~ 1,
           
           packid == "Others" ~ 1,
           
           TRUE ~ 0
         ),
         `NIAD Market` = case_when(
           stri_sub(ATC4_Code, 1, 4) %in% c("A10L", "A10H", "A10M", "A10J", 
                                            "A10K", "A10S", "A10N") ~ 1,
           
           TRUE ~ 0
         ),
         `Onglyza Market` = case_when(
           Prd_desc %in% c("EUCREAS            NVR", "KOMBIGLYZE XR      AZN", 
                           "TRAJENTA DUO       B.I", "ONGLYZA            BMS", 
                           "JANUVIA            MSD", "GALVUS             NVR", 
                           "TRAJENTA           B.I", "NESINA             TAK", 
                           "JANUMET            MSD") ~ 1,
           
           TRUE ~ 0
         ),
         `Forxiga(SGLT2) Market` = case_when(
           Prd_desc %in% c("FORXIGA            AZN", "JARDIANCE          B.I", 
                           "INVOKANA           MCK") ~ 1,
           
           TRUE ~ 0
         ),
         `IOAD Market` = if_else(`Onglyza Market` + `Forxiga(SGLT2) Market` > 0, 1, 0),
         `Antianemic Market` = case_when(
           !(Molecule_Desc %in% c("FOLIC ACID", "CYANOCOBALAMIN+FOLIC ACID+NICOTINAMIDE")) & 
             stri_sub(ATC4_Code, 1, 3) == "B03" ~ 1,
           
           TRUE ~ 0
         ),
         `Lokelma Market` = case_when(
           Molecule_Desc == "POLYSTYRENE SULFONATE" ~ 1,
           
           Prd_desc == "Lokelma" ~ 1,
           
           TRUE ~ 0
         )) %>% 
  select(`年` = year, `年季` = quarter, `省份` = province, `城市` = city, 
         `标通` = MOLE_NAME_CH, PROD_DES_C = PROD_NAME_CH, `剂型` = DOSAGE, 
         `规格` = SPEC, `转换比` = PACK, `单位`, `包装`, CORP_DES_C = Corporation_C, 
         `购买方式`, `数量（盒）` = units, `金额（元）` = sales, `单支单片`, 
         `价格` = price, Pack_ID = packid, `IMS 药品ID`, Mole_Ename = Molecule_Desc, 
         Prod_Ename = Prd_desc, Corp_EName = Corp_Desc, Corp_TYPE = MNF_TYPE, 
         `ATC Code IV` = ATC4_CODE, TA, `PPI (Oral/IV) Market`, 
         `Linaclotide Market`, `Symbicort Market`, `Respules (Asthma&COPD) Market`, 
         `NEB Market`, `Non-Oral Expectorant Market`, `Betaloc Oral Market`, 
         `Plendil Market`, `HTN Market`, `Crestor Market`, `XZK Market`, 
         `Brilinta Market`, `Onglyza Market`, `NIAD Market`, `Forxiga(SGLT2) Market`, 
         `IOAD Market`, `Antianemic Market`, `Lokelma Market`, VBP_Excu, VBP)

write_feather(az.chc, "03_Outputs/08_AZ_CHC_2017Q1_2020Q1.feather")
write.xlsx(az.chc, "03_Outputs/08_AZ_CHC_2017Q1_2020Q1.xlsx")


# history
az.history <- read.xlsx("02_Inputs/AZ_CHC_2017Q1_2019Q4_Delivery_Final_0610（HTN+Crestor Market）.xlsx")
colnames(az.history) <- gsub("[.]", " ", colnames(az.history))

az.total <- bind_rows(az.history, az.chc) %>% 
  mutate(`Non-Oral Expectorant Market` = ifelse(`剂型` %in% c("粉针剂", "雾化溶液", "吸入剂", "注射液"), 
                                                `Non-Oral Expectorant Market`, 
                                                0)) %>% 
  filter(!(`PPI (Oral/IV) Market` & `Linaclotide Market` & `Symbicort Market` & 
             `Respules (Asthma&COPD) Market` & `NEB Market` & `Non-Oral Expectorant Market` & 
             `Betaloc Oral Market` & `Plendil Market` & `HTN Market` & `Crestor Market` & 
             `XZK Market` & `Brilinta Market` & `Onglyza Market` & `NIAD Market` & 
             `Forxiga(SGLT2) Market` & `IOAD Market` & `Antianemic Market` & `Lokelma Market`)) %>% 
  select(`年`, `年季`, `省份`, `城市`, 
         `规格`, `转换比`, `单位`, `包装`, CORP_DES_C, 
         `购买方式`, `数量（盒）`, `金额（元）`, `单支单片`, 
         `价格`, Pack_ID, `IMS 药品ID`, `PPI (Oral/IV) Market`, 
         `Linaclotide Market`, `Symbicort Market`, `Respules (Asthma&COPD) Market`, 
         `NEB Market`, `Non-Oral Expectorant Market`, `Betaloc Oral Market`, 
         `Plendil Market`, `HTN Market`, `Crestor Market`, `XZK Market`, 
         `Brilinta Market`, `Onglyza Market`, `NIAD Market`, `Forxiga(SGLT2) Market`, 
         `IOAD Market`, `Antianemic Market`, `Lokelma Market`, VBP_Excu, VBP)
  left_join(ims.mol, by = c("Pack_ID" = "packid")) %>% 
  left_join(product.info, by = c("packid" = "PACK_ID")) %>% 
  left_join(prod.info, by = c("packid" = "pack_id")) %>% 
  left_join(market.info, by = "packid") %>% 
  mutate(ATC4_CODE = if_else(is.na(ATC4_CODE), atc4_code, ATC4_CODE),
         MOLE_NAME_CH = if_else(is.na(MOLE_NAME_CH), gene_name, MOLE_NAME_CH),
         PROD_NAME_CH = if_else(is.na(PROD_NAME_CH), ims_product_cn, PROD_NAME_CH),
         DOSAGE = if_else(is.na(DOSAGE), dosage, DOSAGE),
         SPEC = if_else(is.na(SPEC), specification, SPEC),
         PACK = if_else(is.na(PACK), PckSize_Desc, PACK),
         CORP_NAME_CH = if_else(is.na(CORP_NAME_CH), ims_corp, CORP_NAME_CH))
  select(`年`, `年季`, `省份`, `城市`, `标通`, PROD_DES_C, `剂型`, 
         `规格`, `转换比`, `单位`, `包装`, CORP_DES_C, 
         `购买方式`, `数量（盒）`, `金额（元）`, `单支单片`, 
         `价格`, Pack_ID, `IMS 药品ID`, 
         Mole_Ename = Molecule_Desc, Prod_Ename = Prd_desc, 
         Corp_EName = Corp_Desc, Corp_TYPE = MNF_TYPE, `ATC Code IV` = ATC4_Code, TA, 
         `PPI (Oral/IV) Market`, 
         `Linaclotide Market`, `Symbicort Market`, `Respules (Asthma&COPD) Market`, 
         `NEB Market`, `Non-Oral Expectorant Market`, `Betaloc Oral Market`, 
         `Plendil Market`, `HTN Market`, `Crestor Market`, `XZK Market`, 
         `Brilinta Market`, `Onglyza Market`, `NIAD Market`, `Forxiga(SGLT2) Market`, 
         `IOAD Market`, `Antianemic Market`, `Lokelma Market`, VBP_Excu, VBP)
  arrange(`年季`, `省份`, `城市`, Pack_ID)

# `通用名`, `标通`, `商品名`, `剂型`, `规格`, `转换比`, `单位`, `包装`, 
# `生产企业`, `企业合并`, `单支单片`, IMS.药品ID, Corp_TYPE







