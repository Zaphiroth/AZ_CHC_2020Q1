# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC
# Purpose:      Dashboard
# programmer:   Zhe Liu
# Date:         26-02-2020
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- History result ----
# pack desc
pack.desc <- read_xls("02_Inputs/AZ_PACKID_UPDATE Apr'20.xls")

pack.desc1 <- read.xlsx("02_Inputs/CHPA-Sanofi file.xlsx") %>% 
  mutate(PACK_CODE = stri_pad_left(PACK_CODE, 7, 0))

# history
az.history <- read.xlsx("02_Inputs/AZ_CHC_2017Q1_2019Q4_Delivery_Final_0610（HTN+Crestor Market）.xlsx")
colnames(az.history) <- gsub("[.]", " ", colnames(az.history))

# az.history.result <- az.history %>% 
#   group_by(year = `年`, quarter = `年季`, province = `省份`, city = `城市`, TA, 
#            atc4 = `ATC Code IV`, packid = Pack_ID) %>% 
#   summarise(units = sum(`数量（盒）`, na.rm = TRUE), 
#             sales = sum(`金额（元）`, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(atc4 = if_else(packid == "Others", 
#                         "Others", 
#                         atc4))

az.history.format <- az.history %>% 
  left_join(city.en, by = c("省份" = "Province_C", "城市" = "City_C")) %>% 
  select(Year = `年`, YQ = `年季`, Province_C = `省份`, City_C = `城市`, 
         Province_E, City_E, 
         Molecule_C = `标通`, PROD_DES_C, `剂型`, 
         `规格`, `转换比`, `单位`, `包装`, CORP_DES_C, 
         `购买方式`, `Total Unit` = `数量（盒）`, `Value (RMB)` = `金额（元）`, 
         `Counting Unit` = `单支单片`, `价格`, Pack_ID, `IMS 药品ID`, Mole_Ename, 
         Prod_Ename, Corp_EName, Corp_TYPE, `ATC Code IV`, TA, 
         `PPI (Oral/IV) Market`, 
         `Linaclotide Market`, `Symbicort Market`, `Respules (Asthma&COPD) Market`, 
         `NEB Market`, `Non-Oral Expectorant Market`, `Betaloc Oral Market`, 
         `Plendil Market`, `HTN Market`, `Crestor Market`, `XZK Market`, 
         `Brilinta Market`, `Onglyza Market`, `NIAD Market`, `Forxiga(SGLT2) Market`, 
         `IOAD Market`, `Antianemic Market`, `Lokelma Market`, VBP_Excu, VBP) %>% 
  setDT() %>% 
  melt(id.vars = c('Year', 'YQ', 'Province_C', 'City_C', 'Province_E', 'City_E', 
                   'Molecule_C', 'PROD_DES_C', '剂型', '规格', '转换比', '单位', 
                   '包装', 'CORP_DES_C', '购买方式', 'Total Unit', 'Value (RMB)', 
                   'Counting Unit', '价格', 'Pack_ID', 'IMS 药品ID', 
                   'Mole_Ename', 'Prod_Ename', 'Corp_EName', 'Corp_TYPE', 
                   'ATC Code IV', 'TA', 'VBP_Excu', 'VBP'), 
       measure.vars = c('PPI (Oral/IV) Market', 'Linaclotide Market', 
                        'Symbicort Market', 'Respules (Asthma&COPD) Market', 
                        'NEB Market', 'Non-Oral Expectorant Market', 
                        'Betaloc Oral Market', 'Plendil Market', 'HTN Market', 
                        'Crestor Market', 'XZK Market', 'Brilinta Market', 
                        'Onglyza Market', 'NIAD Market', 'Forxiga(SGLT2) Market', 
                        'IOAD Market', 'Antianemic Market', 'Lokelma Market'), 
       variable.name = "Market", 
       value.name = "flag", 
       variable.factor = FALSE) %>% 
  filter(flag == 1) %>% 
  left_join(pack.desc, by = c('Pack_ID' = 'PACK_COD')) %>% 
  left_join(pack.desc1, by = c('Pack_ID' = 'PACK_CODE')) %>% 
  mutate(PACK_DES = if_else(is.na(PACK_DES), PACK_NAME, PACK_DES),
         `IMS 药品ID` = stri_paste(stri_sub(Pack_ID, 1, 5), "-", Pack_ID),
         `IMS 药品ID` = if_else(Pack_ID == "Others", "Others", `IMS 药品ID`)) %>% 
  select(Market, Year, YQ, Province_C, City_C, Province_E, City_E, Molecule_C, 
         PROD_DES_C, `剂型`, `规格`, `转换比`, `单位`, Pack_DESC = PACK_DES, 
         CORP_DES_C, `购买方式`, `Total Unit`, `Value (RMB)`, `Counting Unit`, 
         `价格`, Pack_ID, `IMS 药品ID`, Mole_Ename, Prod_Ename, Corp_EName, 
         Corp_TYPE, `ATC Code IV`, TA, VBP_Excu, VBP)

write.xlsx(az.history.format, "03_Outputs/AZ_CHC_2017Q1_2019Q4_Delivery_Format.xlsx")


##---- 2020Q1 result ----
az.20q1 <- read.xlsx('02_Inputs/AZ_CHC_2020q1_final_v3.xlsx')
colnames(az.20q1) <- gsub("[.]", " ", colnames(az.20q1))

az.20q1.format <- az.20q1 %>% 
  left_join(city.en, by = c("省份" = "Province_C", "城市" = "City_C")) %>% 
  select(Year = `年`, YQ = `年季`, Province_C = `省份`, City_C = `城市`, 
         Province_E, City_E, 
         Molecule_C = `标通`, PROD_DES_C, `剂型`, 
         `规格`, `转换比`, `单位`, `包装`, CORP_DES_C, 
         `购买方式`, `Total Unit` = `数量（盒）`, `Value (RMB)` = `金额（元）`, 
         `Counting Unit` = `单支单片`, `价格`, Pack_ID, `IMS 药品ID`, Mole_Ename, 
         Prod_Ename, Corp_EName, Corp_TYPE, `ATC Code IV`, TA, 
         `PPI (Oral/IV) Market`, 
         `Linaclotide Market`, `Symbicort Market`, `Respules (Asthma&COPD) Market`, 
         `NEB Market`, `Non-Oral Expectorant Market`, `Betaloc Oral Market`, 
         `Plendil Market`, `HTN Market`, `Crestor Market`, `XZK Market`, 
         `Brilinta Market`, `Onglyza Market`, `NIAD Market`, `Forxiga(SGLT2) Market`, 
         `IOAD Market`, `Antianemic Market`, `Lokelma Market`, VBP_Excu, VBP) %>% 
  setDT() %>% 
  melt(id.vars = c('Year', 'YQ', 'Province_C', 'City_C', 'Province_E', 'City_E', 
                   'Molecule_C', 'PROD_DES_C', '剂型', '规格', '转换比', '单位', 
                   '包装', 'CORP_DES_C', '购买方式', 'Total Unit', 'Value (RMB)', 
                   'Counting Unit', '价格', 'Pack_ID', 'IMS 药品ID', 
                   'Mole_Ename', 'Prod_Ename', 'Corp_EName', 'Corp_TYPE', 
                   'ATC Code IV', 'TA', 'VBP_Excu', 'VBP'), 
       measure.vars = c('PPI (Oral/IV) Market', 'Linaclotide Market', 
                        'Symbicort Market', 'Respules (Asthma&COPD) Market', 
                        'NEB Market', 'Non-Oral Expectorant Market', 
                        'Betaloc Oral Market', 'Plendil Market', 'HTN Market', 
                        'Crestor Market', 'XZK Market', 'Brilinta Market', 
                        'Onglyza Market', 'NIAD Market', 'Forxiga(SGLT2) Market', 
                        'IOAD Market', 'Antianemic Market', 'Lokelma Market'), 
       variable.name = "Market", 
       value.name = "flag", 
       variable.factor = FALSE) %>% 
  filter(flag == 1) %>% 
  filter(!(Pack_ID %in% c('6900404', '1749306'))) %>% 
  left_join(pack.desc, by = c('Pack_ID' = 'PACK_COD')) %>% 
  left_join(pack.desc1, by = c('Pack_ID' = 'PACK_CODE')) %>% 
  mutate(PACK_DES = if_else(is.na(PACK_DES), PACK_NAME, PACK_DES),
         `IMS 药品ID` = stri_paste(stri_sub(Pack_ID, 1, 5), "-", Pack_ID),
         `IMS 药品ID` = if_else(Pack_ID == "Others", "Others", `IMS 药品ID`),
         TA = if_else(Pack_ID == 'Others', 'CV', TA),
         Mole_Ename = if_else(Pack_ID == 'Others', 'Others', Mole_Ename),
         `Counting Unit` = if_else(is.na(`Counting Unit`), 0, `Counting Unit`)) %>% 
  select(Market, Year, YQ, Province_C, City_C, Province_E, City_E, Molecule_C, 
         PROD_DES_C, `剂型`, `规格`, `转换比`, `单位`, Pack_DESC = PACK_DES, 
         CORP_DES_C, `购买方式`, `Total Unit`, `Value (RMB)`, `Counting Unit`, 
         `价格`, Pack_ID, `IMS 药品ID`, Mole_Ename, Prod_Ename, Corp_EName, 
         Corp_TYPE, `ATC Code IV`, TA, VBP_Excu, VBP)

write.xlsx(az.20q1.format, '03_Outputs/AZ_CHC_2020Q1_Delivery_Format.xlsx')

az.result <- bind_rows(az.history.format, az.20q1.format)

write.xlsx(az.result, '03_Outputs/AZ_CHC_2017Q1_2020Q1_Delivery_Format.xlsx')


##---- 购买方式 ----
az.total1 <- read.xlsx('02_Inputs/AZ_CHC_2017Q1_2020Q1_0703.xlsx', sheet = 2)
colnames(az.total1) <- gsub("[.]", " ", colnames(az.total1))

market.info1 <- distinct(market.mapping, Market = `小市场`, `购买方式`) %>% 
  bind_rows(distinct(market.mapping, Market = `大市场`, `购买方式`)) %>% 
  filter(!is.na(Market))

az.result1 <- az.total1 %>% 
  select(-`购买方式`) %>% 
  left_join(market.info1, by = 'Market') %>% 
  select(Market, Year, YQ, Province_C, City_C, Province_E, City_E, Molecule_C, 
         PROD_DES_C, `剂型`, `规格`, `转换比`, `单位`, Pack_DESC, 
         CORP_DES_C, `购买方式`, `Total Unit`, `Value (RMB)`, `Counting Unit`, 
         `价格`, Pack_ID, `IMS 药品ID`, Mole_Ename, Prod_Ename, Corp_EName, 
         Corp_TYPE, `ATC Code IV`, TA, VBP_Excu, VBP)

write.xlsx(az.result1, '03_Outputs/AZ_CHC_2017Q1_2020Q1_0703v2.xlsx')











