library(tidyverse)
library(readxl)

# Lendo os bancos de dados
fisica_quimica <- readxl::read_excel("data-raw/fisica-quimica.xlsx")
dplyr::glimpse(fisica_quimica)


usc <- fisica_quimica %>%
  mutate(Usc = ifelse(Us <= 0, NA, Us)) %>%
  mutate(Usc= ifelse(is.na(Usc), mean(Usc,na.rm=TRUE), Usc)) %>%
  pull(Usc)

fisica_quimica$Us <- usc
write_rds(fisica_quimica,"data/fisica_quimica")

# Est C
estoque_carbono <- readxl::read_excel("data-raw/estoque-carbono.xlsx")
dplyr::glimpse(estoque_carbono)
write_rds(estoque_carbono,"data/estoque_carbono")

# Biomassa seca
biomassa <- readxl::read_excel("data-raw/biomassa-seca.xlsx")
dplyr::glimpse(biomassa)
write_rds(biomassa,"data/biomassa")

## Mechendo nos dados de Safira
arquivos_planta <- list.files(path = "data-raw/Dados Safira/SAFIRA 2ª Coleta (colheita cana planta)",
           pattern = ".xls",
           full.names = TRUE)

arquivos_soca <- list.files(path = "data-raw/Dados Safira/SAFIRA 4ª coleta (colheita 1 soca)",
                            pattern = ".xls",
                            full.names = TRUE)

leitura_arquivo <- function(caminho){
  # caminho <- arquivos_planta[6]
  n_sheets <- length(excel_sheets(caminho))
  da <- readxl::read_excel(caminho) %>%
    janitor::clean_names()
  colunas <- names(da)
  area = volume = diametro = comprimento = comprimento_diametro_m2 =
    comprimento_diametro_05 = comprimento_diametro_2 = 0
  fvad<-c("fibra_number",
          "volume_mm3",
          "area_superficial_mm2",
          "diametro_ponderado_mm")
  if(n_sheets == 1 & length(colunas) == 4){ ### planta (não temos duas abas por planilha)
    if(sum(colunas == fvad)==4){
      volume = sum(da$volume_mm3,na.rm = TRUE)
      area = sum(da$area_superficial_mm2,na.rm = TRUE)
      diametro = mean(da$diametro_ponderado_mm,na.rm = TRUE)
    } else if(sum(str_detect(colunas, "comprimento"))>0){
      comprimento = sum(da[-1],na.rm = TRUE)
      n_aux <- length(da)
      comprimento_diametro_05 <- da %>%
        select(matches("1_mm")) %>%  pull() %>%  sum()
      comprimento_diametro_2 <- da %>%
        select(matches("2_mm")) %>%  pull() %>%  sum()
      if(n_aux >=3){
        comprimento_diametro_m2 = sum(da[-(1:3)],na.rm = TRUE)
      } else{
        comprimento_diametro_m2 = 0
      }
    }
  } else if(n_sheets == 1 & sum(str_detect(colunas, "comprimento"))>0){
    comprimento = sum(da[-1],na.rm = TRUE)
    n_aux <- length(da)
    comprimento_diametro_05 <- da %>%
      select(matches("_1_mm")) %>%  pull() %>%  sum()
    comprimento_diametro_2 <- da %>%
      select(matches("_2_mm")) %>%  pull() %>%  sum()
    if(n_aux >=3){
      comprimento_diametro_m2 = sum(da[-(1:3)],na.rm = TRUE)
    } else{
      comprimento_diametro_m2 = 0
    }
  } else if(n_sheets == 2){ # para as duas pois na soca temos duas abas por planilha
    volume = sum(da$volume_mm3,na.rm = TRUE)
    area = sum(da$area_superficial_mm2,na.rm = TRUE)
    diametro = mean(da$diametro_ponderado_mm,na.rm = TRUE)
    da_comp <- readxl::read_excel(caminho,
                                  sheet = excel_sheets(caminho)[2]) %>%
      janitor::clean_names()
    comprimento = sum(da_comp[-1],na.rm = TRUE)
    n_aux <- length(da_comp)
    comprimento_diametro_05 <- da_comp %>%
      select(matches("_1_mm")) %>%  pull() %>%  sum()
    comprimento_diametro_2 <- da_comp %>%
      select(matches("_2_mm")) %>%  pull() %>%  sum()
    if(n_aux >=3){
      comprimento_diametro_m2 = sum(da_comp[-(1:3)],na.rm = TRUE)
    } else{
      comprimento_diametro_m2 = 0
      }
  }
  texto <- str_split(caminho,"/", simplify = TRUE)[,4]
  texto <- str_remove_all(texto," ")
  texto <- str_remove_all(texto,".xls")
  texto <- str_split(texto,"-", simplify = TRUE)
  vl <- c(texto,volume,area,diametro,comprimento,
          comprimento_diametro_05,comprimento_diametro_2,
          comprimento_diametro_m2)
  names(vl) <- c("tratamento",
                 "repeticao",
                 "local",
                 "prof",
                 "volume",
                 "area",
                 "diametro",
                 "comprimento_total",
                 "comprimento_05",
                 "comprimento_2",
                 "comprimento_m2")
  return(vl)
};leitura_arquivo(arquivos_planta[7])
leitura_arquivo(arquivos_soca[103])

saida <- purrr::map_df(arquivos_planta, leitura_arquivo)
View(saida)
diametro_sonda <- 0.475
safira_cana_planta <- saida %>%
  mutate(
    prof = str_remove(prof,"EXCEL2"),
    prof = str_remove(prof,"EXCEL3"),
    prof = str_remove(prof,"EXCEL4"),
    volume = as.numeric(volume),
    area = as.numeric(area),
    diametro = as.numeric(diametro),
    comprimento_total = as.numeric(comprimento_total),
    comprimento_05 = as.numeric(comprimento_05),
    comprimento_2 = as.numeric(comprimento_2),
    comprimento_m2 = as.numeric(comprimento_m2)
  ) %>%
  group_by(tratamento,repeticao,local,prof) %>%
  summarise(
    volume = sum(volume),
    area = sum(area),
    diametro = sum(diametro),
    comprimento_total = sum(comprimento_total),
    comprimento_05 = sum(comprimento_05),
    comprimento_2 = sum(comprimento_2),
    comprimento_m2 = sum(comprimento_m2)
  ) %>%
  mutate(
    volume_cm3_dm3 = volume/1000/diametro_sonda,
    area_cm2_dm3 = area/100/diametro_sonda,
    comprimento_total_cm_dm3 = comprimento_total/10/diametro_sonda,
    comprimento_05_cm_dm3 = comprimento_05/10/diametro_sonda,
    comprimento_2_cm_dm3 = comprimento_2/10/diametro_sonda,
    comprimento_m2_cm_dm3 = comprimento_m2/10/diametro_sonda,
  )


saida_soca <- purrr::map_df(arquivos_soca, leitura_arquivo)
safira_cana_soca <- saida_soca %>%
  mutate(
    prof = str_remove(prof,"EXCEL2"),
    prof = str_remove(prof,"EXCEL3"),
    prof = str_remove(prof,"EXCEL4"),
    volume = as.numeric(volume),
    area = as.numeric(area),
    diametro = as.numeric(diametro),
    comprimento_total = as.numeric(comprimento_total),
    comprimento_05 = as.numeric(comprimento_05),
    comprimento_2 = as.numeric(comprimento_2),
    comprimento_m2 = as.numeric(comprimento_m2)
  ) %>%
  group_by(tratamento,repeticao,local,prof) %>%
  summarise(
    volume = sum(volume),
    area = sum(area),
    diametro = sum(diametro),
    comprimento_total = sum(comprimento_total),
    comprimento_05 = sum(comprimento_05),
    comprimento_2 = sum(comprimento_2),
    comprimento_m2 = sum(comprimento_m2)
  ) %>%
  mutate(
    volume_cm3_dm3 = volume/1000/diametro_sonda,
    area_cm2_dm3 = area/100/diametro_sonda,
    comprimento_total_cm_dm3 = comprimento_total/10/diametro_sonda,
    comprimento_05_cm_dm3 = comprimento_05/10/diametro_sonda,
    comprimento_2_cm_dm3 = comprimento_2/10/diametro_sonda,
    comprimento_m2_cm_dm3 = comprimento_m2/10/diametro_sonda,
  )

writexl::write_xlsx(safira_cana_planta,"data/safira_cana_planta.xlsx")
write_rds(safira_cana_planta,"data/raiz_cana_planta")
writexl::write_xlsx(safira_cana_soca,"data/safira_cana_soca.xlsx")
write_rds(safira_cana_soca,"data/raiz_cana_soca")
# for(i in seq_along(arquivos_soca)) leitura_arquivo(arquivos_soca[i])

