library(tidyverse)
library(readxl)

# Lendo os bancos de dados
fisica_quimica <- readxl::read_excel("data-raw/fisica-quimica.xlsx")
dplyr::glimpse(fisica_quimica)

# Est C
estoque_carbono <- readxl::read_excel("data-raw/estoque-carbono.xlsx")
dplyr::glimpse(estoque_carbono)

# Biomassa seca
biomassa <- readxl::read_excel("data-raw/biomassa-seca.xlsx")
dplyr::glimpse(biomassa)

## Mechendo nos dados de Safira
arquivos_planta <- list.files(path = "data-raw/Dados Safira/SAFIRA 2ª Coleta (colheita cana planta)",
           pattern = ".xls",
           full.names = TRUE)

arquivos_soca <- list.files(path = "data-raw/Dados Safira/SAFIRA 4ª coleta (colheita 1 soca)",
                            pattern = ".xls",
                            full.names = TRUE)

leitura_arquivo <- function(caminho){
  # caminho <- arquivos_soca[1]
  n_sheets <- length(excel_sheets(caminho))
  da <- readxl::read_excel(caminho) %>%
    janitor::clean_names()
  colunas <- names(da)
  area = volume = diametro = comprimento = 0
  fvad<-c("fibra_number",
          "volume_mm3",
          "area_superficial_mm2",
          "diametro_ponderado_mm")
  if(n_sheets == 1){
    if(sum(colunas == fvad)==4){
      volume = sum(da$volume_mm3,na.rm = TRUE)
      area = sum(da$area_superficial_mm2,na.rm = TRUE)
      diametro = mean(da$diametro_ponderado_mm,na.rm = TRUE)
    } else if(sum(str_detect(colunas, "comprimento"))>0){
      comprimento = sum(da[-1],na.rm = TRUE)
    }
  } else if(n_sheets == 2){
    volume = sum(da$volume_mm3,na.rm = TRUE)
    area = sum(da$area_superficial_mm2,na.rm = TRUE)
    diametro = mean(da$diametro_ponderado_mm,na.rm = TRUE)
    da_comp <- readxl::read_excel(caminho,
                                  sheet = excel_sheets(caminho)[2]) %>%
      janitor::clean_names()
    comprimento = sum(da_comp[-1],na.rm = TRUE)
  }
  texto <- str_split(caminho,"/", simplify = TRUE)[,4]
  texto <- str_remove_all(texto," ")
  texto <- str_remove_all(texto,".xls")
  texto <- str_split(texto,"-", simplify = TRUE)
  vl <- c(texto,volume,area,diametro,comprimento)
  names(vl) <- c("tratamento",
                 "repeticao",
                 "local",
                 "prof","volume","area","diametro","comprimento")
  return(vl)
}
leitura_arquivo(arquivos_planta[1])
leitura_arquivo(arquivos_soca[10])

saida <- purrr::map_df(arquivos_planta, leitura_arquivo)
View(saida)

safira_cana_planta <- saida %>%
  mutate(
    prof = str_remove(prof,"EXCEL2"),
    prof = str_remove(prof,"EXCEL3"),
    prof = str_remove(prof,"EXCEL4"),
    volume = as.numeric(volume),
    area = as.numeric(area),
    diametro = as.numeric(diametro),
    comprimento = as.numeric(comprimento),
  ) %>%
  group_by(tratamento,repeticao,local,prof) %>%
  summarise(
    volume = sum(volume),
    area = sum(area),
    diametro = sum(diametro),
    comprimento = sum(comprimento)
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
    comprimento = as.numeric(comprimento),
  ) %>%
  group_by(tratamento,repeticao,local,prof) %>%
  summarise(
    volume = sum(volume),
    area = sum(area),
    diametro = sum(diametro),
    comprimento = sum(comprimento)
  )
getwd()
writexl::write_xlsx(safira_cana_planta,"data/safira_cana_planta.xlsx")
writexl::write_xlsx(safira_cana_soca,"data/safira_cana_soca.xlsx")
