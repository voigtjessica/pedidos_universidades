#Bibliotecas:
library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)
library(janitor)
library(xlsx)
library(googledrive)
library(googlesheets)
library(stringi)


drive_find(n_max=10)

#Função:
como_data <- function(x) {
  
  stopifnot(require(dplyr))
  x <- gsub(" .*", "", x)
  y <- gsub(".*/", "", x)
  x <- if_else((nchar(y)==4), as.Date(x, format="%d/%m/%Y"),
               as.Date(x, format="%d/%m/%y"))
  
}

#Preparando os bancos:

#Versão do anexo dos pedidos (que não possuem órgão de destino)

#Autenticação:
gs_ls() 

#Importando todos os pedidos ANALISADOS:
sheet <- gs_title("anexo_relatorio_lai2018")
banco_pedidos_lai <- gs_read(sheet, ws="PedidosLAI")

banco_pedidos_lai <- banco_pedidos_lai %>%
  clean_names()

setwd("C:/Users/coliv/Documents/pedidos_universidades/bancos originais")
# save(banco_pedidos_lai, file="banco_pedidos_lai.Rdata")
# load("banco_pedidos_lai.Rdata")
#vou adicionar o órgao de destino da CGU a partir do arquivo de classificação do colab

setwd("C:/Users/coliv/Documents/pedidos_universidades")
load("base_cgu_completa.Rdata")

protocolos <- base_cgu_completa %>%
  mutate(protocolo = str_trim(protocolo, side = c("both"))) %>%
  select(protocolo, destino) 

#Fazendo o arquivo final
cgu_final <- banco_pedidos_lai %>%
  filter(orgao == "CGU – Controladoria-Geral da União" ) %>%
  mutate(protocolo = str_trim(protocolo, side = c("both"))) %>%
  left_join(protocolos, by = c("protocolo"))

# save(cgu_final, file="cgu_final.Rdata")
# ############# Posso começar daqui

setwd("C:/Users/coliv/Documents/pedidos_universidades")
load("base_cgu_completa.Rdata")
load("cgu_final.Rdata")

# Relação dos institutos e universidades cujos pedidos queremos analisar:

uni <- c("	FUNRei - Fundação Universidade Federal de São João Del Rei	"	,
         "	FURG – Fundação Universidade Federal do Rio Grande	"	,
         "	IF BAIANO – Instituto Federal de Educação, Ciência e Tecnologia Baiano	"	,
         "	IF FLUMINENSE – Instituto Federal de Educação, Ciência e Tecnologia Fluminense	"	,
         "	IF GOIANO – Instituto Federal de Educação, Ciência e Tecnologia Goiano	"	,
         "	IFAC – Instituto Federal de Educação, Ciência e Tecnologia do Acre	"	,
         "	IFAL – Instituto Federal de Educação, Ciência e Tecnologia de Alagoas	"	,
         "	IFAM – Instituto Federal de Educação, Ciência e Tecnologia do Amazonas	"	,
         "	IFAP – Instituto Federal de Educação, Ciência e Tecnologia do Amapá	"	,
         "	IFB – Instituto Federal de Educação, Ciência e Tecnologia de Brasília	"	,
         "	IFBA – Instituto Federal de Educação, Ciência e Tecnologia da Bahia	"	,
         "	IFC – Instituto Federal de Educação, Ciência e Tecnologia Catarinense	"	,
         "	IFCE – Instituto Federal de Educação, Ciência e Tecnologia do Ceará	"	,
         "	IFES – Instituto Federal de Educação, Ciência e Tecnologia do Espírito Santo	"	,
         "	IFFAR – Instituto Federal de Educação, Ciência e Tecnologia Farroupilha	"	,
         "	IFG – Instituto Federal de Educação, Ciência e Tecnologia de Goiás	"	,
         "	IFMA – Instituto Federal de Educação, Ciência e Tecnologia do Maranhão	"	,
         "	IFMG – Instituto Federal de Educação, Ciência e Tecnologia de Minas Gerais	"	,
         "	IFMGSE – Instituto Federal de Educação, Ciência e Tecnologia do Sudeste de Minas Gerais	"	,
         "	IFMS – Instituto Federal de Educação, Ciência e Tecnologia do Mato Grosso do Sul	"	,
         "	IFMT – Instituto Federal de Educação, Ciência e Tecnologia do Mato Grosso	"	,
         "	IFNMG – Instituto Federal de Educação, Ciência e Tecnologia do Norte de Minas Gerais	"	,
         "	IFPA – Instituto Federal de Educação, Ciência e Tecnologia do Pará	"	,
         "	IFPB – Instituto Federal de Educação, Ciência e Tecnologia da Paraíba	"	,
         "	IFPE – Instituto Federal de Educação, Ciência e Tecnologia de Pernambuco	"	,
         "	IFPI – Instituto Federal de Educação, Ciência e Tecnologia do Piauí	"	,
         "	IFPR – Instituto Federal de Educação, Ciência e Tecnologia do Paraná	"	,
         "	IFRJ – Instituto Federal de Educação, Ciência e Tecnologia do Rio de Janeiro	"	,
         "	IFRN – Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Norte	"	,
         "	IFRO – Instituto Federal de Educação, Ciência e Tecnologia de Rondônia	"	,
         "	IFRR – Instituto Federal de Educação, Ciência e Tecnologia de Roraima	"	,
         "	IFRS – Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul	"	,
         "	IFSC – Instituto Federal de Educação, Ciência e Tecnologia de Santa Catarina	"	,
         "	IFSE – Instituto Federal de Educação, Ciência e Tecnologia de Sergipe	"	,
         "	IFSP – Instituto Federal de Educação, Ciência e Tecnologia de São Paulo	"	,
         "	IFSPE – Instituto Federal de Educação, Ciência e Tecnologia do Sertão Pernambucano	"	,
         "	IFSul – Instituto Federal de Educação, Ciência e Tecnologia Sul-Rio-Grandense	"	,
         "	IFSULDEMINAS – Instituto Federal de Educação, Ciência e Tecnologia do Sul de Minas Gerais	"	,
         "	IFTM – Instituto Federal de Educação, Ciência e Tecnologia do Triângulo Mineiro	"	,
         "	IFTO – Instituto Federal de Educação, Ciência e Tecnologia do Tocantins	"	,
         "	INPA – Instituto Nacional de Pesquisas da Amazônia	"	,
         "	INPE-MCT – Instituto Nacional de Pesquisas Espaciais	"	,
         "	IPEA – Fundação Instituto de Pesquisa Econômica Aplicada	"	,
         "	UFABC – Fundação Universidade Federal do ABC	"	,
         "	UFAC – Fundação Universidade Federal do Acre	"	,
         "	UFAL – Universidade Federal de Alagoas	"	,
         "	UFAM – Fundação Universidade do Amazonas	"	,
         "	UFBA – Universidade Federal da Bahia	"	,
         "	UFC – Universidade Federal do Ceará	"	,
         "	UFCA - Universidade Federal do Cariri	"	,
         "	UFCG – Universidade Federal de Campina Grande	"	,
         "	UFCSPA – Fundação Universidade Federal de Ciências da Saúde de Porto Alegre	"	,
         "	UFERSA-RN – Universidade Federal Rural do Semi-Árido	"	,
         "	UFES – Universidade Federal do Espírito Santo	"	,
         "	UFESBA - Universidade Federal do Sul da Bahia	"	,
         "	UFF – Universidade Federal Fluminense	"	,
         "	UFFS – Universidade Federal da Fronteira Sul	"	,
         "	UFG – Universidade Federal de Goiás	"	,
         "	UFGD – Fundação Universidade Federal da Grande Dourados	"	,
         "	UFJF – Universidade Federal de Juiz de Fora	"	,
         "	UFLA – Universidade Federal de Lavras	"	,
         "	UFMA – Fundação Universidade Federal do Maranhão	"	,
         "	UFMG – Universidade Federal de Minas Gerais	"	,
         "	UFMS – Fundação Universidade Federal de Mato Grosso do Sul	"	,
         "	UFMT – Fundação Universidade Federal de Mato Grosso	"	,
         "	UFOB - Universidade Federal do Oeste da Bahia	"	,
         "	UFOP – Fundação Universidade Federal de Ouro Preto	"	,
         "	UFOPA – Universidade Federal do Oeste do Pará	"	,
         "	UFPA – Universidade Federal do Pará	"	,
         "	UFPB – Universidade Federal da Paraíba	"	,
         "	UFPE – Universidade Federal de Pernambuco	"	,
         "	UFPel – Fundação Universidade Federal de Pelotas	"	,
         "	UFPI – Fundação Universidade Federal do Piauí	"	,
         "	UFPR – Universidade Federal do Paraná	"	,
         "	UFRA – Universidade Federal Rural da Amazônia	"	,
         "	UFRB – Universidade Federal do Recôncavo da Bahia	"	,
         "	UFRGS – Universidade Federal do Rio Grande do Sul	"	,
         "	UFRJ – Universidade Federal do Rio de Janeiro	"	,
         "	UFRN – Universidade Federal do Rio Grande do Norte	"	,
         "	UFRPE – Universidade Federal Rural de Pernambuco	"	,
         "	UFRR – Fundação Universidade Federal de Roraima	"	,
         "	UFRRJ – Universidade Federal Rural do Rio de Janeiro	"	,
         "	UFS – Fundação Universidade Federal de Sergipe	"	,
         "	UFSC – Universidade Federal de Santa Catarina	"	,
         "	UFSCar – Fundação Universidade Federal de São Carlos	"	,
         "	UFSM – Universidade Federal de Santa Maria	"	,
         "	UFT – Fundação Universidade Federal do Tocantins	"	,
         "	UFTM – Universidade Federal do Triângulo Mineiro	"	,
         "	UFU – Universidade Federal de Uberlândia	"	,
         "	UFV – Fundação Universidade Federal de Viçosa	"	,
         "	UFVJM – Universidade Federal dos Vales do Jequitinhonha e Mucuri	"	,
         "	UNB – Fundação Universidade de Brasília	"	,
         "	UNIFAL-MG – Universidade Federal de Alfenas	"	,
         "	UNIFAP – Fundação Universidade Federal do Amapá	"	,
         "	UNIFEI – Universidade Federal de Itajubá	"	,
         "	UNIFESP – Universidade Federal de São Paulo	"	,
         "	UNIFESSPA – Universidade Federal do Sul e Sudeste do Pará	"	,
         "	UNILA – Universidade Federal da Integração Latino-Americana	"	,
         "	UNILAB – Universidade da Integração Internacional da Lusofonia Afro-Brasileira	"	,
         "	UNIPAMPA – Fundação Universidade Federal do Pampa	"	,
         "	UNIR – Fundação Universidade Federal de Rondônia	"	,
         "	UNIRIO – Universidade Federal do Estado do Rio de Janeiro	"	,
         "	UNIVASF – Fundação Universidade Federal do Vale do São Francisco	"	,
         "	UTFPR – Universidade Tecnológica Federal do Paraná	"	)

#Retirando whitespaces antes e depois do nome dos institutos/universidades
uni <- trimws(uni, which = c("both"))

#total pedidos para universidades:
uni_total <- base_cgu_completa %>%
  filter(destino %in% uni) %>%
  mutate(estado = gsub(".*do|.*da|.*de", "", destino),
         estado = ifelse(grepl("Fluminense",destino), "Rio de Janeiro", estado),
         estado = ifelse(grepl("UFLA", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("Janeiro",estado), "Rio de Janeiro", estado),
         estado = ifelse(grepl("UFRGS", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("IFRN", destino), "Rio Grande do Norte", estado),
         estado = ifelse(grepl("UFJF", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("UNIVASF", destino), "Pernambuco", estado),
         estado = ifelse(grepl("UFCG", destino), "Paraíba", estado),
         estado = ifelse(grepl("UFSM", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("UFSCar", destino), "São Paulo", estado),
         estado = ifelse(grepl("UFMS", destino), "Mato Grosso do Sul", estado),
         estado = ifelse(grepl("Uberlândia", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("IF BAIANO", destino), "Bahia", estado),
         estado = ifelse(grepl("Viçosa", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("Mineiro", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("UFERSA-RN", destino), "Rio Grande do Norte", estado),
         estado = ifelse(grepl("UFABC", destino), "São Paulo", estado),
         estado = ifelse(grepl("Ouro Preto", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("Goiano", destino), "Goiás", estado),
         estado = ifelse(grepl("Catarinense", destino), "Santa Catarina", estado),
         estado = ifelse(grepl("Pernambucano", destino), "Pernambuco", estado),
         estado = ifelse(grepl("Cariri", destino), "Ceará", estado),
         estado = ifelse(grepl("UFVJM", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("UNILAB", destino), "NA", estado),
         estado = ifelse(grepl("São João Del Rei", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("UFGD", destino), "Mato Grosso do Sul", estado),
         estado = ifelse(grepl("Pelotas", destino), "Rio Grande do Sul", estado),
         estado = ifelse(destino == "UFFS – Universidade Federal da Fronteira Sul", "NA", estado),
         estado = ifelse(grepl("Farroupilha", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("FURG", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("UNIPAMPA", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("IFRS", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("Alfenas", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("Itajubá", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("IFSul", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("Porto Alegre", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("UNILA", destino), "Paraná", estado),
         estado = ifelse(grepl("IPEA", destino), "Distrito Federal", estado),
         estado = ifelse(grepl("Brasília", destino), "Distrito Federal", estado),
         estado = ifelse(grepl("INPE-MCT", destino), "São Paulo", estado),
         estado = ifelse(grepl("IFMS", destino), "Mato Grosso do Sul", estado),
         estado = trimws(estado, which = c("both")))

nrow(uni_total) #21020

#Banco de análise: banco CLASSIFICADO
banco_uni <- cgu_final %>%
  filter(destino %in% uni) %>%
  select(-c(possui_anexo, orgao, orgao_cgu))
 
# #Removendo alguns objetos para eu não me confundir
# rm(cgu_final)
# rm(banco_pedidos_lai)
# rm(protocolos)
# rm(sheet)

######################################################################################################################
#Panorama Geral:

  

#Percentual dos pedidos feitos à universidades em comparação a CGU
nrow(uni_total) / nrow(base_cgu_completa)   #23% de todos os pedidos feitos à CGU no período.

#Distribuição entre os institutos
por_destino <- uni_total %>%
  group_by(destino) %>%
  summarise(total_pedidos = n()) %>%
  arrange(desc(total_pedidos))
  
# Distribuição por UF não mostrou nada interessante - apaguei

#Tamanho da amostra

so_pedidos <- banco_uni %>%
  filter(tipo_da_interacao == "Pedido") %>%
  nrow()

(so_pedidos/nrow(uni_total) ) * 100


########################################################################################################
# Analises das classificações:

#Preciso verificar se o atendido está em qual parte do empilhamento
# preciso ver se foi atendido em qual instância
#consertar objeto que vai gerar os gráficos para que ele considere apenas pedidos e o
# resultado final (atendimento) ou um assunto

banco_so_pedidos <- banco_uni %>%
  filter(tipo_da_interacao == "Pedido") 

#Atendimento:
atendimento <- banco_so_pedidos %>%
  filter(atendimento != "Não Classificado") %>%
  filter(!is.na(atendimento)) %>%
  group_by(atendimento) %>%
  summarise(total_pedidos = n(),
            perc = paste0(round(total_pedidos/880 *100, 0), " %"))

mycols <- c("#ffa600", "#ff6361", "#bc5090", "#58508d")

ggplot(atendimento, aes(x="", y=total_pedidos, fill=atendimento)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = total_pedidos, label = perc), color = "white", 
            position = position_stack(vjust = 0.6))+
  scale_fill_manual(values = mycols) +
  theme_void()

#Distribuição do tema do pedido
dist_tema <- banco_so_pedidos %>%
  group_by(area_tematica) %>%
  summarise(total = n(),
            perc = total / 706) %>%
  ungroup() %>%
  mutate(perc2 = paste0(round(perc*100, 2), "%")) %>%
  arrange(desc(total))


dist_tema$area_tematica <- factor(dist_tema$area_tematica , 
                                  levels = dist_tema$area_tematica[order(dist_tema$perc)])

ggplot(dist_tema, aes(x=area_tematica, y=perc, label=perc2)) +
  geom_segment( aes(x=area_tematica, xend=area_tematica, y=0, yend=perc),
                color="grey") +
  geom_point( color="#adadad", size=9, alpha= 0.9) +
  theme_light(base_size = 14) +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) + scale_y_continuous(name ="", labels = scales::percent)  +
  scale_x_discrete(name ="") +
  geom_text(color = "black", size = 3)

 
# Distribuição de assuntos

# Versão Tabela1 que depois foi formatada
dist_assunto2 <- banco_so_pedidos %>%
  group_by(area_tematica, assunto) %>%
  summarise(total_assunto = n(),
            perc_assunto_total = round((total_assunto/706)*100, 2)) %>%
   arrange(desc(total_assunto))

write.xlsx(as.data.frame(dist_assunto2),
           file="dist_assunto2.xlsx", sheetName="assuntos",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)



write.xlsx(as.data.frame(banco_uni), 
           file="banco_pedidos_universidades.xlsx", sheetName="Página 1",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

drive_upload(
  "banco_pedidos_universidades.xlsx",
  path="~/TB/2019/Encontro de E-sic Min Infraestrutura/",
  name = "banco_pedidos_universidades",
  type = "spreadsheet")

#Distribuição dos temas
dist_tema <- banco_so_pedidos %>%
  group_by(area_tematica) %>%
  summarise(total = n(),
            perc = paste0(round(total/706, 2)*100, " %"))


#Distribuição por assunto dentro das universidades e institutos:

uni_dist_assunto <- banco_so_pedidos %>%
  group_by(destino, area_tematica, assunto) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(destino) %>%
  mutate(total_universidade = sum(total),
         perc_universidade = round(total/total_universidade,2)*100) %>%
  ungroup() %>%
  arrange(desc(total_universidade), desc(total)) %>%
  select(-c(total))


#Quantos não foram atendidos?
banco_uni %>%
  filter(tipo_da_interacao == "Pedido",
         atendimento != "Não Classificado") %>%
  group_by(atendimento) %>%
  summarise(total = n(),
            perc = total/706)

#Ver quais assuntos tem maior taxa de não atendimento:
banco_uni %>%
  filter(tipo_da_interacao == "Pedido",
         atendimento == "Não Atendido") %>%
  group_by(area_tematica, assunto) %>%
  summarise(total = n())

#montando um banco de não atendidos:
nao_atendidos <- banco_so_pedidos %>%
  filter(atendimento == "Não Atendido") 

#Achando o banco de amostra no drive
amostra_sheet <- gs_title("df_amostra")
#Adicionando linhas na planilhona
gs_add_row(amostra_sheet, ws = "não atendidos", input = nao_atendidos, verbose = TRUE)


#Não atendidos por área temática

na_area_tematica <- nao_atendidos %>%
  group_by(area_tematica) %>%
  summarise(total_nao_atendidos = n()) %>%
  left_join(dist_tema, by=c("area_tematica")) %>%
  select(-c(perc)) %>%
  mutate(perc_nao_atendidos = total_nao_atendidos/total) %>%
  arrange(desc(perc_nao_atendidos)) %>%
  mutate(perc_nao_atendidos2 = paste0( round(perc_nao_atendidos,2)*100, "%")) %>%
  select(-c(total_nao_atendidos, total, perc_nao_atendidos))

na_assuntos <- nao_atendidos %>%
  group_by(area_tematica, assunto) %>%
  summarise(total_na_assunto = n()) %>%
  left_join(dist_assunto2) %>%
  mutate(percentual_nao_atendimento = total_na_assunto/total_assunto) %>%
  select(area_tematica, assunto, percentual_nao_atendimento) %>%
  arrange(desc(percentual_nao_atendimento)) %>%
  mutate(percentual_nao_atendimento = paste0(round(percentual_nao_atendimento,2)*100, "%"))


write.xlsx(as.data.frame(na_area_tematica), 
           file="na_area_tematica.xlsx", sheetName="Página 1",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

write.xlsx(as.data.frame(na_assuntos), 
           file="na_assuntos.xlsx", sheetName="Página 1",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)



#######################################################################################
###### Agora é só validação das categorias

#1. tamanho da amostra:

amostra <- banco_so_pedidos %>%
  group_by(area_tematica, assunto) %>%
  summarise(total = n(),
            amostra = round(0.1* total, 0))

df_amostra <- data.frame()

for(i in 1:nrow(amostra)){
  
  larea_tematica <- amostra[i,1]
  lassunto <- amostra[i,2]
  n <- as.numeric(amostra[i,4])
  
  a <- banco_so_pedidos %>%
    filter(area_tematica %in% larea_tematica) %>%
    filter(assunto %in% lassunto)
  
  b <- sample_n(a, size = n)
  
  df_amostra <- rbind(df_amostra, b)
}

for(i in 1:nrow(amostra)){
  
  larea_tematica <- amostra[i,1]
  lassunto <- amostra[i,2]
  n <- as.numeric(amostra[i,4])
  
  if(n == 0){
    c <- banco_so_pedidos %>%
      filter(area_tematica %in% larea_tematica) %>%
      filter(assunto %in% lassunto)
    
    df_amostra <- rbind(df_amostra, c)
  }}

write.xlsx(as.data.frame(df_amostra), 
           file="df_amostra.xlsx", sheetName="Página 1",
           col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

drive_upload(
  "df_amostra.xlsx",
  path="~/TB/2019/Encontro de E-sic Min Infraestrutura/",
  name = "df_amostra",
  type = "spreadsheet")

# Amostra complementar:

df_amostra_complementar <- data.frame()

for(i in 1:nrow(amostra)){
  
  larea_tematica <- amostra[i,1]
  lassunto <- amostra[i,2]
  n <- as.numeric(amostra[i,3])
  
  if(n < 18 & n > 5){
    d <- banco_so_pedidos %>%
      filter(area_tematica %in% larea_tematica) %>%
      filter(assunto %in% lassunto) %>%
      mutate(Obs = NA)
    
    df_amostra_complementar <- rbind(df_amostra_complementar, d)
  }}

#Achando o banco de amostra no drive
amostra_sheet <- gs_title("df_amostra")
#Adicionando linhas na planilhona
gs_add_row(amostra_sheet, ws = "Página 1", input = df_amostra_complementar, verbose = TRUE)
 

### Contando quantas vezes as palavras ocorrem nos pedidos (contagem simples)

cont_pedidos <- banco_so_pedidos$conteudo

#retirando acentos
cont_pedidos <- stri_trans_general(cont_pedidos, "Latin-ASCII")
cont_pedidos <- tolower(cont_pedidos)
cont_pedidos <- gsub("\\.", " ", cont_pedidos)
cont_pedidos <- gsub("\\/", " ", cont_pedidos)
cont_pedidos <- gsub("\\(", " ", cont_pedidos)
cont_pedidos <- gsub("\\)", " ", cont_pedidos)
cont_pedidos <- gsub(",", " ", cont_pedidos)

palavras <- str_split(cont_pedidos, " ")
palavras <- unlist(palavras, use.names=FALSE)

palavras <- data.frame(expressoes = palavras)

palavras <- palavras %>%
  group_by(expressoes) %>%
  summarise(total = n())

expressoes <- c("auditoria", "campus", "cargo", "vagas", "vaga", "servidores", "cargos", 
                "codigo", "dados", "aproveitamento", "mestrado", "professor", 
                "relatorios", "alunos", "graduacao", "pos-graduacao", "despesasdiarias", 
                "pagamento", "cota")

df <- banco_so_pedidos %>%
  select(conteudo) %>%
  mutate(conteudo = stri_trans_general(conteudo, "Latin-ASCII"),
         conteudo  =  tolower(conteudo), 
         conteudo  =  gsub("\\.", " ", conteudo),
         conteudo  =  gsub("\\/", " ", conteudo),
         conteudo  =  gsub("\\(", " ", conteudo),
         conteudo  =  gsub("\\)", " ", conteudo),
         conteudo  =  gsub(",", " ", conteudo),
         auditoria = ifelse(grepl("auditoria", conteudo),1, 0),
         campus = ifelse(grepl("campus", conteudo),1, 0),
         cargo = ifelse(grepl("cargo", conteudo),1,ifelse(grepl("cargos", conteudo), 1, 0)),
         vagas = ifelse(grepl("vagas", conteudo),1, ifelse(grepl("vaga", conteudo),1,0)),
         servidores = ifelse(grepl("servidores", conteudo),1, 0),
         codigo = ifelse(grepl("codigo", conteudo),1, 0),
         dados = ifelse(grepl("dados", conteudo),1, 0),
         aproveitamento = ifelse(grepl("aproveitamento", conteudo),1, 0),
         mestrado = ifelse(grepl("mestrado", conteudo),1, 0),
         professor = ifelse(grepl("professor", conteudo),1, 0),
         relatorios = ifelse(grepl("relatorios", conteudo),1, 0),
         alunos = ifelse(grepl("alunos", conteudo),1, 0),
         graduacao = ifelse(grepl("graduacao", conteudo),1, 0),
         pos_graduacao = ifelse(grepl("pos-graduacao", conteudo),1, 0),
         despesasdiarias = ifelse(grepl("despesasdiarias", conteudo), 1, 0),
         pagamento = ifelse(grepl("pagamento", conteudo), 1, 0),
         cota = ifelse(grepl("cota", conteudo), 1, ifelse(grepl("cotas", conteudo),1,0))) %>%
  adorn_totals("row") %>%
  filter(conteudo == "Total")

cont <- colnames(df)

df[2,] <- cont

df <- as.data.frame(t(df), row.names = FALSE)
  
colnames(df) <- c("Total", "Conteudo")

df1 <- df %>%
  filter(Total != "Total") %>%
  select(Conteudo, Total) 

###################################