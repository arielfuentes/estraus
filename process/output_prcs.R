library(openxlsx)
#summary ----
sum_pred <- rf_new_pred %>%
  mutate(Ser = str_sub(SerSen, 1, 3), SUBEN = replace_na(SUBEN, 0)) %>%
  group_by(Ser) %>%
  summarise(SUBEN = sum(SUBEN), SUB.Pred = sum(.pred)) %>%
  ungroup() %>%
  mutate('%SUBEN' = SUBEN/sum(SUBEN)*100, 
         '%SUB.Pred' = SUB.Pred/sum(SUB.Pred)*100)
#write excel ----
wb <- createWorkbook()
addWorksheet(wb = wb, sheetName = "resumen%")
addWorksheet(wb = wb, sheetName = "Pred")
writeData(wb, sheet = "resumen%", sum_pred, 1, 1)
writeData(wb, sheet = "Pred", rf_new_pred, 1, 1)
saveWorkbook(wb, file = paste0("output/PredANTF.xlsx"))
