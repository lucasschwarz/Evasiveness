##############################################################################
#O DATASET DE TESTE ESTÁ SENDO O ARQUIVO MEETINGS, LOCALIZADO NA PASTA PICTURES, DO MEU DESKTOP PRINCIPAL
##############################################################################

##Importando ASM como MEETINGS

MEETINGS_ORIGINAL <- MEETINGS #backup copy of my original dataset
MEETINGS <- na.omit(MEETINGS) #limpando todos os NAs

##############################################################################
#EVASIVE SHAREHOLDER MEETINGS: DISTANCE-BASED MEASURES
##############################################################################


##CREATING HEADQUARTERS (INDEPENDENT VARIABLE)
#HEADQUARTERS is an indicator variable that equals 1 one if the annual meeting takes place at company headquarters in a given year and 0 otherwise.
#HQ and ZIPMEET are the main inputs, where HQ = headquarters ZIP Code and ZIPMEET = event location ZIP Code.

MEETINGS$HEADQUARTERS <- ifelse(MEETINGS$HQ == MEETINGS$ZIPMEET, 1, 0) #if HQ ZIP code = Meeting ZIP Code, 1. Otherwise, 0.


##CREATING LNDISTANCE (INDEPENDENT VARIABLE)
#"distance" is the raw distance, in miles, between HQ and event' location, and LNDISTANCE is the natural logarithm of one plus the distance, in miles, between company headquarters and the annual meeting location, based upon ZIP code data.
library(zipcodeR)
DISTANCE_temp <- zip_distance(MEETINGS$HQ, MEETINGS$ZIPMEET, lonlat = TRUE, units = "miles") #calculating the raw distance (miles) between HQ and meeting location.
DISTANCE_temp$zipcode_a <- NULL #drop useless column
DISTANCE_temp$zipcode_b <- NULL #drop useless column
DISTANCE_temp$LNDISTANCE <- log((1+DISTANCE_temp$distance)) #creating LNDISTANCE on the temporary dataframe
MEETINGS <-cbind(MEETINGS, DISTANCE_temp) #merging LNDISTANCE with my main dataframe
rm(DISTANCE_temp) #drop temporary dataframe


##CREATING REMOTE (INDEPENDENT VARIABLE)
#REMOTE is an indicator variable that equals one if the annual shareholder meeting takes place at a remote location (both REMOTE_HQ = 1 and REMOTE_AIRPORT = 1, simultaneously)

#Creating REMOTE_HQ
#Assigning NA if is NA, 1 if the distance between event and headquarters is bigger than 50 miles and 0 otherwise.
#REMOTE_HQ is an indicator variable that equals one if the distance between HQ and the event' location is bigger than 50 miles, and 0 otherwise.
remote_hq_func <- function(x){
  if(is.na(x)){
    return(NA)
  }
  if(x > 50){
    return(1)
  }
  else{
    return(0)
  }
}

MEETINGS$REMOTE_HQ <- lapply(MEETINGS$distance,remote_hq_func) #create REMOTE_HQ column on my main dataframe


#Converting event' location ZIP Codes to geographic coordinates (latitude and longitude)


df <- MEETINGS


library(zipcodeR)

latitudes <- list()
longitudes <- list()

lat <- "NULL"
lng <- "NULL"

for(i in rownames(df))
{
  tryCatch(
    expr = {
      lat <- geocode_zip(df[i,"ZIPMEET"])[1,"lat"]
      lng <- geocode_zip(df[i,"ZIPMEET"])[1,"lng"]
    },
    error = function(e){ 
      lat <- NA
      lng <- NA
    }
  )
  if(!(df[i,"ZIPMEET"]=="VIRTUAL")& !is.na(df[i,"ZIPMEET"])){
    latitudes <- append(latitudes,lat)
    longitudes <- append(longitudes,lng)
  }
  else{
    latitudes <- append(latitudes,NA)
    longitudes <- append(longitudes,NA)
  }
}


MEETINGS$lat_ZIPMEET <- latitudes
MEETINGS$lng_ZIPMEET <- longitudes

rm(latitudes) #drop useless df
rm(longitudes) #drop useless df
rm(lat) #drop useless df
rm(lng) #drop useless df
rm(df) #drop useless df


#Identifying nearest large hub airports to the event
#Obtanining the distance between event' location and the nearest large hub airport
#API: Amadeus Airports, token required
#https://developers.amadeus.com/self-service/category/air/api-doc/airport-nearest-relevant/api-reference
#url: https://test.api.amadeus.com/v1/reference-data/locations/airports?latitude=%s&longitude=%s&radius=500&sort=relevance
#Get Access Token

#REMOTE_AIRPORT is an indicator variable that equals 1 if the distance between event location and the nearest large hub airport is bigger than 50 miles, and 0 otherwise.

library(httr)
library(rjson)

df <- MEETINGS

distances <- list()
sf <- list()
st <- list()
dn <- list()
iatas <- list()

for( i in 1:nrow(df) ){
  if(!is.na(df[i, "lat_ZIPMEET"])&!is.na(df[i, "lng_ZIPMEET"])& !is.na(df[i, "REMOTE_HQ"]) & df[i,"REMOTE_HQ"]==1){
    query <- sprintf("https://test.api.amadeus.com/v1/reference-data/locations/airports?latitude=%s&longitude=%s&radius=500&sort=relevance", df[i, "lat_ZIPMEET"], df[i, "lng_ZIPMEET"])
    getdata <- GET(url=query, add_headers(Authorization="Bearer OI2tL9GX5VhwKGzNHjCp8R1xkAlv"))
    body <- fromJSON(content(getdata,type="text", encoding = "UTF-8"))
    print(getdata[["status_code"]])
    
    if (getdata[["status_code"]] == 200 && length(body[["data"]])>0){
      
      distances <- append(distances, body[["data"]][[1]][["distance"]][["value"]])
      sf <- append(sf, body[["data"]][[1]][["analytics"]][["flights"]][["score"]])
      st <- append(st, body[["data"]][[1]][["analytics"]][["travelers"]][["score"]])
      dn <- append(dn, body[["data"]][[1]][["detailedName"]])
      iatas <- append(iatas, body[["data"]][[1]][["iataCode"]])
      print(sprintf("distance: %s", distances[i-1]))
      print(i)
      next
    }
  }
  distances <- append(distances, NA)
  sf <- append(sf, NA)
  st <- append(st, NA)
  dn <- append(dn, NA)
  iatas <- append(iatas, NA)
  print(i)
}

MEETINGS$distance_event_nearest_airport_km_destination <- distances
MEETINGS$score_flights_destination <- sf
MEETINGS$score_travelers_destination <- st
MEETINGS$datailed_name_destination <- dn
MEETINGS$IATA_destination <- iatas

backups_requisicoes_destination <- MEETINGS #backup copy to avoid new data requests
backups_requisicoes_destination <- backups_requisicoes_destination[, 22:ncol(backups_requisicoes_destination)] #backup copy to avoid new data requests

rm(st) #drop useless 
rm(dn) #drop useless 
rm(iatas) #drop useless 
rm(body) #drop useless 
rm(df) #drop useless 
rm(getdata) #drop useless 


###WORKING DATA####
##Creating REMOTE
#Under construction (necessário verificar)
kmtomiles <- function(x){
  if(is.na(x)){
    return(x)
  }
  else{
    return(x * 0.621371)
  }
}

MEETINGS$distance_miles <- lapply(MEETINGS$distance_event_nearest_airport_km_destination, kmtomiles)
MEETINGS$REMOTE_AIRPORT <- ifelse(MEETINGS$distance_miles > 50, 1, 0)

MEETINGS$REMOTE <- ifelse(MEETINGS$REMOTE_HQ == 0 & is.na(MEETINGS$REMOTE_AIRPORT), 0,
                          ifelse(MEETINGS$REMOTE_HQ == 1 & MEETINGS$REMOTE_AIRPORT == 1, 1,
                                 ifelse(MEETINGS$REMOTE_HQ == 1 & MEETINGS$REMOTE_AIRPORT == 0, 0,
                                        ifelse(is.na(MEETINGS$REMOTE_HQ), NA,
                                               ifelse(is.na(MEETINGS$REMOTE_AIRPORT), 0, NA)))))


###WORKING DATA####



#I will employ the following procedures to develop TRAVEL (INDEPENDENT VARIABLE)
#Obtanining the distance between headquarters' location and the nearest large hub airport
#Converting headquarters' location ZIP Codes to geographic coordinates (latitude and longitude)
#API: Amadeus Airports, token required
###não pode ter nada na coluna HQ diferente de VIRTUAL ou ZIP Code. Se tiver endereço, por ex. vai falhar.


df <- MEETINGS 


library(zipcodeR)

latitudes <- list()
longitudes <- list()

lat <- "NULL"
lng <- "NULL"

for(i in rownames(df))
{
  tryCatch(
    expr = {
      lat <- geocode_zip(df[i,"HQ"])[1,"lat"]
      lng <- geocode_zip(df[i,"HQ"])[1,"lng"]
    },
    error = function(e){ 
      lat <- NA
      lng <- NA
    }
  )
  if(!is.na(df[i,"HQ"])){
    latitudes <- append(latitudes,lat)
    longitudes <- append(longitudes,lng)
  }
  else{
    latitudes <- append(latitudes,NA)
    longitudes <- append(longitudes,NA)
  }
}

MEETINGS$lat_HQ <- latitudes
MEETINGS$lng_HQ <- longitudes

rm(latitudes) #drop useless 
rm(longitudes) #drop useless 
rm(lat) #drop useless 
rm(lng) #drop useless 
rm(df) #drop useless 



library(httr)
library(rjson)


df <- MEETINGS

distances <- list()
sf <- list()
st <- list()
dn <- list()
iatas <- list()

for( i in 1:nrow(df) ){
  if(!is.na(df[i, "lat_HQ"])&!is.na(df[i, "lng_HQ"])& !is.na(df[i, "distance"])& df[i,"distance"]>=250){
    query <- sprintf("https://test.api.amadeus.com/v1/reference-data/locations/airports?latitude=%s&longitude=%s&radius=500&sort=relevance", df[i, "lat_HQ"], df[i, "lng_HQ"])
    getdata <- GET(url=query, add_headers(Authorization="Bearer GnucU1DyhmeJQkROL7lF7gYXBkea"))
    body <- fromJSON(content(getdata,type="text", encoding = "UTF-8"))
    print(getdata[["status_code"]])
    if (getdata[["status_code"]] == 200 && length(body[["data"]])>0){
      
      distances <- append(distances, body[["data"]][[1]][["distance"]][["value"]])
      sf <- append(sf, body[["data"]][[1]][["analytics"]][["flights"]][["score"]])
      st <- append(st, body[["data"]][[1]][["analytics"]][["travelers"]][["score"]])
      dn <- append(dn, body[["data"]][[1]][["detailedName"]])
      iatas <- append(iatas, body[["data"]][[1]][["iataCode"]])
      print(sprintf("distance: %s", distances[i-1]))
      print(i)
      next
    }
  }
  distances <- append(distances, NA)
  sf <- append(sf, NA)
  st <- append(st, NA)
  dn <- append(dn, NA)
  iatas <- append(iatas, NA)
  print(i)
}


MEETINGS$distance_event_nearest_airport_km_origin <- distances
MEETINGS$score_flights_origin <- sf
MEETINGS$score_travelers_origin <- st
MEETINGS$datailed_name_origin <- dn
MEETINGS$IATA_origin <- iatas

backups_requisicoes_origin <- MEETINGS #backup copy to avoid new data requests
backups_requisicoes_origin <- backups_requisicoes_origin[, 29:ncol(backups_requisicoes_origin)] #backup copy to avoid new data requests

rm(distances) #drop useless 
rm(sf) #drop useless 
rm(st) #drop useless 
rm(dn) #drop useless 
rm(iatas) #drop useless 
rm(body) #drop useless 
rm(df) #drop useless 
rm(getdata) #drop useless 



##CREATING TRAVEL (INDEPENDENT VARIABLE)

#Criando variável para preparar coleta manual de driving times (para casos entre 2 e 250...) e para casos >250 (driving time entre HQ e aeroport do HQ e aeroporto do evento e evento)

#driving_time_direct representa o tempo de direção estimado entre o HQ e o ZIPMEET. Tempo direto que será utilizado para criar a variável TRAVELTIME.
MEETINGS$driving_time_direct <- ifelse(MEETINGS$distance >= 2 & MEETINGS$distance < 250, "todo", NA)


#criando tempos de deslocamento entre HQ e aeroporto próximo a HQ
#criando tempos de deslocamento entre evento e aeroporto próximo ao destino
#coletas manuais todo
MEETINGS$driving_time_HQ_to_IATA_origin <- ifelse(MEETINGS$distance >= 250, "todo", NA)
MEETINGS$driving_time_ZIPMEET_to_IATA_destination <- ifelse(MEETINGS$distance >= 250, "todo", NA)

#EXPORTANDO DATAFRAME PARA COLETAS MANUAIS DE TEMPOS DE DIREÇÃO!!
#"TODO" SIGNIFICA ESPAÇO A PREENCHER, HAND-COLLECTED.


###EXPORTAR ARQUIVO PARA COLETA MANUAL...

MEETINGS_EXPORTAR <- apply(MEETINGS,2,as.character) #GAMBIARRA
write.csv(MEETINGS_EXPORTAR, "MEETINGS_COLETA_DRIVING.csv", row.names = TRUE)

###DETALHES: DADOS DO ARQUIVO MEETING_HORAS JÁ ESTÁ EM HORAS E JÁ TEM DADOS P/ CONTROLES...
### APÓS PREENCHER, IMPORTAR... "MEETINGS_COLETA_DRIVING_DONE_HORAS" no arquivo MEETINGS

MEETINGS$driving_time_HQ_to_IATA_origin <- as.numeric(MEETINGS$driving_time_HQ_to_IATA_origin)
MEETINGS$driving_time_ZIPMEET_to_IATA_destination <- as.numeric(MEETINGS$driving_time_ZIPMEET_to_IATA_destination)



#Calculando distância entre IATAS...

#FUNCIONANDO

##preciso baixar uma lista para #valid_IATA_codes 
###O ARQUIVO DE REFERENCIA FOI OBTIDO DO GITHUB
##https://datahub.io/core/airport-codes#data

#Importando dados sobre IATAs válidos...
airportcodes <- read.csv("G:/PhD/PhD Dissertation/1 Evasive shareholder meetings and stock price crash risk/Routines and Data/IATA/airport-codes_csv.csv")
valid_IATA_codes <- airportcodes$iata_code


library(airportr)

MEETINGS$distances_IATAS <- sapply(1:nrow(MEETINGS), function(i) {
  origin <- MEETINGS$IATA_origin[i]
  destination <- MEETINGS$IATA_destination[i]
  if (is.na(origin) || is.na(destination)) {
    return(NA)
  }
  if (origin %in% valid_IATA_codes == FALSE || destination %in% valid_IATA_codes == FALSE) {
    return(NA)
  }
  ifelse(is.na(airport_distance(origin, destination)), NA, airport_distance(origin, destination))
})


#TEM QUE TER TUDO NUMERICO NAQUELES DRIVING TIMES ALI ACIMA...
MEETINGS$flight_TRAVEL_TIME <- ((MEETINGS$distances_IATAS/800)+0.5+1.5)+MEETINGS$driving_time_HQ_to_IATA_origin + MEETINGS$driving_time_ZIPMEET_to_IATA_destination


#CRIANDO A VARIÁVEL TRAVEL
functiontraveltime <- function(row){
  if(is.na(row["distance"])){
    return(NA)
  }
  else if(row["distance"] < 1){
    return(0.05)
  }
  else if(row["distance"] >= 1 && row["distance"] <2){
    return(0.10)
  }
  else if(row["distance"] >= 2 && row["distance"] < 250){
      return(row[["driving_time_direct"]])
  }
  else if(row["distance"] >= 250){
      return(row[["flight_TRAVEL_TIME"]])
  }
  else{
    return(NA)
  }
}



MEETINGS$TRAVEL <- apply(MEETINGS,1,functiontraveltime)
MEETINGS$TRAVEL <- as.numeric(MEETINGS$TRAVEL)


##Variáveis de controle... (NÃO PARECE NECESSÁRIO)
#MEETINGS$ib <- as.numeric(MEETINGS$ib)
#MEETINGS$seq <- as.numeric(MEETINGS$seq)
# MEETINGS$dt <- as.numeric(MEETINGS$dt)
# MEETINGS$at <- as.numeric(MEETINGS$at)
# MEETINGS$mkvalt <- as.numeric(MEETINGS$mkvalt)
# MEETINGS$xrd <- as.numeric(MEETINGS$xrd)


###SIGMA: Desvio-padrão dos retornos semanais durante o ano, já está na base CRSP
###RET: Média de retornos de ações no ano, já está na base CRSP (MULTIPLICADO POR 100!)
###ROA:
MEETINGS$ROA <- MEETINGS$ib / MEETINGS$at
###SIZE:
MEETINGS$SIZE <- log(MEETINGS$mkvalt)
#MTB:
MEETINGS$MTB <- MEETINGS$mkvalt / MEETINGS$seq
#LEV:
MEETINGS$LEV <- MEETINGS$dt / MEETINGS$at
#RD:
MEETINGS$RD <- MEETINGS$xrd / MEETINGS$at



###CRIANDO DUMMIES MANUALMENTE
#alternativa é criar via FACTOR!
#SIC
#MEETINGS$twodigitsic <- substr(MEETINGS$sic, start = 1, stop = 2)

#library(dplyr)
#SIC_dummies <- model.matrix(~ factor(twodigitsic) - 1, data = MEETINGS)
#colnames(SIC_dummies) <- paste0("SIC_", levels(factor(MEETINGS$twodigitsic)))
#dados_com_dummies <- bind_cols(MEETINGS, SIC_dummies)

#MEETINGS <- dados_com_dummies
#### FAZENDO MERGER COM CRSP...

#IMPORTAR CRSP_CONSOLIDATED (NA VERDADE, JÁ RODAR A PARTIR DELE)
#CRSP <- CRSP_consolidated
###LEMBRAR QUE A BASE ASM JÁ DEVERÁ CONTER TODAS AS VARIÁVEIS NECESSÁRIAS
CRSP <- CRSP_consolidated

ASM <- MEETINGS
CRSP$fyear <- format(as.Date(CRSP$datefix), "%Y")
CRSP$fyear <- as.numeric(CRSP$fyear)
CRSP$CUSIPcorrigido <- substr(CRSP$CUSIP, 1, 6) ##considerando um match de 6 dígitos
ASM$CUSIPcorrigido <- substr(ASM$cusip, 1, 6) #considerando um match de 6 dígitos
DF_MATCH <- merge(ASM, CRSP, by = c("CUSIPcorrigido", "fyear"))
DF_MATCH <- data.frame(DF_MATCH)


#################WORKINGGG

####CRIANDO DATAFRAME PARA ESTATÍSTICA DESCRITIVA E REGRESSÕES
DF_MATCH$Days <- DF_MATCH$Dias


###criando DATAFRAME PARA OLS

#dfols <- subset(DF_MATCH, select = c("cusip","CUSIPcorrigido", "fyear", "conm", "HEADQUARTERS", "REMOTE","LNDISTANCE","TRAVEL", "NCSKEW","DUVOL","COUNT","SIGMA","RET","ROA","SIZE","MTB","LEV", "SIC_34", "SIC_23", "SIC_20", "SIC_28", "SIC_56", "SIC_50", "SIC_36", "SIC_58", "SIC_38", "SIC_33", "SIC_73", "SIC_80", "SIC_57", "SIC_35", "SIC_55", "SIC_51", "SIC_26", "SIC_70", "SIC_27", "SIC_79", "SIC_54", "SIC_99", "SIC_87", "SIC_37", "SIC_30", "SIC_39", "SIC_22", "SIC_10", "SIC_78", "SIC_13", "SIC_16", "SIC_24", "SIC_59", "SIC_82", "SIC_12", "SIC_32", "SIC_17", "SIC_83", "SIC_53", "SIC_75", "SIC_25", "SIC_15", "SIC_31", "SIC_14", "SIC_29", "SIC_81", "SIC_72", "SIC_52", "SIC_21", "Dias"))

DF_MATCH$twodigitsic <- substr(DF_MATCH$sic, start = 1, stop = 2)

dfols <- subset(DF_MATCH, select = c("cusip","CUSIPcorrigido", "fyear", "conm", "HEADQUARTERS", "REMOTE","LNDISTANCE","TRAVEL", "NCSKEW","DUVOL","COUNT","SIGMA","RET","ROA","SIZE","MTB","LEV", "twodigitsic", "Dias"))


#desconsiderando alavancagem menor que 1 
dfols <- subset(dfols, LEV <= 1)
#removendo NAs
dfols <- na.omit(dfols)
#Replace NaN & Inf with NA
dfols[is.na(dfols) | dfols=="Inf"] = NA

#######WINSORIZANDO VARIÁVEIS CONTÍNUAS (EXCETO DE CRASH_RISK)

#Winsorizando  para descritiva a 1% e 99%

library(DescTools)
dfols$LNDISTANCE <- Winsorize(dfols$LNDISTANCE, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$TRAVEL <- Winsorize(dfols$TRAVEL, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$SIGMA <- Winsorize(dfols$SIGMA, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$RET <- Winsorize(dfols$RET, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$ROA <- Winsorize(dfols$ROA, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$SIZE <- Winsorize(dfols$SIZE, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$LEV <- Winsorize(dfols$LEV, probs = c(0.01, 0.99), na.rm = TRUE)

#criando RET times 100
dfols$RET <- dfols$RET * 100
#dias não é variável contínua, não pode winsorizar
#dfols$Dias <- Winsorize(dfols$Dias, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$MTB <- Winsorize(dfols$MTB, probs = c(0.01, 0.99), na.rm = TRUE)


dfols <- as.data.frame(dfols)



#STANDBY: Criando leads das variáveis dependentes

library(dplyr)

#NCSKEW lead
#(dplyr)
#dfols <- dfols %>%
#  group_by(CUSIPcorrigido) %>%
#  mutate(NCSKEW_lead = lead(NCSKEW, n = 1))

#DUVOL lead
#library(dplyr)
#dfols <- dfols %>%
#  group_by(CUSIPcorrigido) %>%
#  mutate(DUVOL_lead = lead(DUVOL, n = 1))

#COUNT lead
# library(dplyr)
# dfols <- dfols %>%
#  group_by(CUSIPcorrigido) %>%
 # mutate(COUNT_lead = lead(COUNT, n = 1))



#Criando lags das variáveis independentes...


library(dplyr)

dfols <- dfols %>%
  arrange(CUSIPcorrigido, fyear) %>% # ordenar por Empresa e Ano
  group_by(CUSIPcorrigido) %>% # agrupar por Empresa
  mutate(SIGMA_lag = lag(SIGMA)) # criar a variável de lag para cada Empresa

dfols <- dfols %>%
  arrange(CUSIPcorrigido, fyear) %>% # ordenar por Empresa e Ano
  group_by(CUSIPcorrigido) %>% # agrupar por Empresa
  mutate(RET_lag = lag(RET)) # criar a variável de lag para cada Empresa

dfols <- dfols %>%
  arrange(CUSIPcorrigido, fyear) %>% # ordenar por Empresa e Ano
  group_by(CUSIPcorrigido) %>% # agrupar por Empresa
  mutate(ROA_lag = lag(ROA)) # criar a variável de lag para cada Empresa

dfols <- dfols %>%
  arrange(CUSIPcorrigido, fyear) %>% # ordenar por Empresa e Ano
  group_by(CUSIPcorrigido) %>% # agrupar por Empresa
  mutate(SIZE_lag = lag(SIZE)) # criar a variável de lag para cada Empresa

dfols <- dfols %>%
  arrange(CUSIPcorrigido, fyear) %>% # ordenar por Empresa e Ano
  group_by(CUSIPcorrigido) %>% # agrupar por Empresa
  mutate(MTB_lag = lag(MTB)) # criar a variável de lag para cada Empresa

dfols <- dfols %>%
  arrange(CUSIPcorrigido, fyear) %>% # ordenar por Empresa e Ano
  group_by(CUSIPcorrigido) %>% # agrupar por Empresa
  mutate(LEV_lag = lag(LEV)) # criar a variável de lag para cada Empresa



dfols <- dfols %>%
  arrange(CUSIPcorrigido, fyear) %>% # ordenar por Empresa e Ano
  group_by(CUSIPcorrigido) %>% # agrupar por Empresa
  mutate(HEADQUARTERS_lag = lag(HEADQUARTERS)) # criar a variável de lag para cada Empresa

dfols <- dfols %>%
  arrange(CUSIPcorrigido, fyear) %>% # ordenar por Empresa e Ano
  group_by(CUSIPcorrigido) %>% # agrupar por Empresa
  mutate(REMOTE_lag = lag(REMOTE)) # criar a variável de lag para cada Empresa

dfols <- dfols %>%
  arrange(CUSIPcorrigido, fyear) %>% # ordenar por Empresa e Ano
  group_by(CUSIPcorrigido) %>% # agrupar por Empresa
  mutate(LNDISTANCE_lag = lag(LNDISTANCE)) # criar a variável de lag para cada Empresa

dfols <- dfols %>%
  arrange(CUSIPcorrigido, fyear) %>% # ordenar por Empresa e Ano
  group_by(CUSIPcorrigido) %>% # agrupar por Empresa
  mutate(TRAVEL_lag = lag(TRAVEL)) # criar a variável de lag para cada Empresa








#removendo NAs de leads e lags
##REMOVER ESSE NA NÃO PARECE RAZOÁVEL!
dfols <- na.omit(dfols)
dfols <- subset(dfols, fyear != 2010)


##ESTATISTICA DESCRITIVA
dfstargazer <- subset(dfols, select = c("cusip","CUSIPcorrigido", "fyear", "conm", "HEADQUARTERS", "REMOTE","LNDISTANCE","TRAVEL", "NCSKEW_lead","DUVOL_lead","COUNT_lead","SIGMA","RET","ROA","SIZE","MTB","LEV", "Dias","NCSKEW"))
dfstargazer <- as.data.frame(dfstargazer)
stargazer(dfstargazer)




#criando controles de industry & year
year_fe <- factor(dfols$fyear) 
industry_fe <- factor(dfols$twodigitsic) 

#criando tabela com observações por setor...
contagem <- table(dfols$twodigitsic)
contagem$var <- table(dfols$twodigitsic)
teste <- data.frame(contagem)

#criandotabela com observações por ano
contagemano <- table(dfols$fyear)
contagemano$var <- table(dfols$fyear)

###TESTING THE HYPOTHESES: DISTANCE-BASED EVASIVENESS AND SPCR

ols1 <- lm(NCSKEW ~ HEADQUARTERS + SIGMA_lag + RET_lag + ROA_lag + SIZE_lag + MTB_lag + LEV_lag, data = dfols)
ols2 <- lm(NCSKEW ~ HEADQUARTERS + SIGMA_lag + RET_lag + ROA_lag + SIZE_lag + MTB_lag + LEV_lag + year_fe + industry_fe, data = dfols)
ols3 <- lm(NCSKEW_lead ~ LNDISTANCE + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)
ols4 <- lm(NCSKEW_lead ~ LNDISTANCE + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)
ols5 <- lm(NCSKEW_lead ~ REMOTE + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)
ols6 <- lm(NCSKEW_lead ~ REMOTE + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)
ols7 <- lm(NCSKEW_lead ~ TRAVEL + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)
ols8 <- lm(NCSKEW_lead ~ TRAVEL + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)

stargazer(ols1,ols2,ols3,ols4,ols5,ols6,ols7,ols8, font.size = "small")

summary(ols2)

library(stargazer)
stargazer(ols1)

###TESTING THE HYPOTHESES: NUMBER OF DAYS (COMMITMENT AND CRASH RISK)

ols9 <- lm(NCSKEW_lead ~ Dias + SIGMA + RET + ROA + MTB + LEV, data = dfols)
ols10 <- lm(NCSKEW_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)
ols11 <- lm(DUVOL_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)
ols12 <- lm(DUVOL_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)
ols13 <- lm(COUNT_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)
ols14 <- lm(COUNT_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)

stargazer(ols9,ols10,ols11,ols12,ols13,ols14, font.size = "small")

#Additional analysis: extremely evasiveness firms...

###ROBUSTNESS CHECKS: FIRM FIXED EFFECTS
#descartar... dados muito travados para isso
install.packages("plm")
library(plm)
plm <- plm(DUVOL ~ HEADQUARTERS_lag + SIGMA_lag + RET_lag + ROA_lag + SIZE_lag + MTB_lag + LEV_lag, data = dfols, model = "within")
summary(plm)



########PSM (working, balance não tá legal...)
library(MatchIt)

mod_match = matchit(HEADQUARTERS_lag ~ SIGMA_lag + RET_lag + ROA_lag + SIZE_lag + MTB_lag + LEV_lag + year_fe + industry_fe, data = dfols, method = "nearest", ratio = 1)

# A common standard in the propensity score literature more generally is that a standardized difference in means greater than 0.1 or 0.2 represents a substantial difference between groups, such that standard regression adjustment for that covariate may be unreliable (Stuart, 2010).
summary(mod_match)

plot(mod_match, type = "density")
plot(mod_match, type = "hist")

install.packages("cobalt")
library(cobalt)
love.plot(mod_match, binary = "std", 
          thresholds = c(m = .1)) 

plot(mod_match)
base_prop= match.data(mod_match)
#regressão PSM...
reg1 = lm(NCSKEW ~ REMOTE_lag + SIGMA_lag + RET_lag + ROA_lag + SIZE_lag + MTB_lag, data = base_prop)
summary(reg1)


##psm pelo CHATGPT

propensity <- glm(HEADQUARTERS_lag ~ SIGMA_lag + RET_lag + ROA_lag + SIZE_lag + MTB_lag + LEV_lag, data = dfols, family = binomial)
treatment$propensity_score <- predict(propensity, type = "response")


##ENTROPY
install.packages("EBweights")
install.packages("EBglm")
library(EBweights)
library(EBglm)



weights <- EBweights(treatment ~ covariate_1 + covariate_2, data = df)



















#working 
olsdias <- lm(NCSKEW_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)

olsdias1 <- lm(DUVOL_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)


summary(olsdias)







dfstargazer <- subset(DF_MATCH, select = c("cusip","CUSIPcorrigido", "fyear", "conm", "HEADQUARTERS", "REMOTE","LNDISTANCE","TRAVEL", "NCSKEW","DUVOL","COUNT","SIGMA","RET","ROA","SIZE","MTB","LEV", "Dias"))
dfstargazer <- na.omit(dfstargazer)
#Replace NaN & Inf with NA
dfstargazer[is.na(dfstargazer) | dfstargazer=="Inf"] = NA
#Winsorizando  para descritiva

library(DescTools)
dfstargazer$LNDISTANCE <- Winsorize(dfstargazer$LNDISTANCE, probs = c(0.01, 0.99), na.rm = TRUE)
dfstargazer$TRAVEL <- Winsorize(dfstargazer$TRAVEL, probs = c(0.01, 0.99), na.rm = TRUE)
dfstargazer$SIGMA <- Winsorize(dfstargazer$SIGMA, probs = c(0.01, 0.99), na.rm = TRUE)
dfstargazer$RET <- Winsorize(dfstargazer$RET, probs = c(0.01, 0.99), na.rm = TRUE)
dfstargazer$ROA <- Winsorize(dfstargazer$ROA, probs = c(0.01, 0.99), na.rm = TRUE)
dfstargazer$SIZE <- Winsorize(dfstargazer$SIZE, probs = c(0.01, 0.99), na.rm = TRUE)
dfstargazer$LEV <- Winsorize(dfstargazer$LEV, probs = c(0.01, 0.99), na.rm = TRUE)
#Dias não é variável contínua
#dfstargazer$Dias <- Winsorize(dfstargazer$Dias, probs = c(0.01, 0.99), na.rm = TRUE)
dfstargazer$MTB <- Winsorize(dfstargazer$MTB, probs = c(0.01, 0.99), na.rm = TRUE)

dfstargazer <- na.omit(dfstargazer)
dfstargazer  <- subset(dfstargazer, LEV <= 1)

stargazer(dfstargazer, out = "\\textwidth")
stargazer(dfstargazer, out="output.tex", out.width="8in")
stargazer(dfstargazer, out="output.tex", out.width="12cm")


stargazer(dfstargazer)

#Para ter R&D de variável #dfols_rd <- subset(DF_MATCH, select = c("cusip","CUSIPcorrigido", "fyear", "conm", "HEADQUARTERS", "REMOTE","LNDISTANCE","TRAVEL", "NCSKEW","DUVOL","COUNT","SIGMA","RET","ROA","SIZE","MTB","LEV","RD", "SIC_34", "SIC_23", "SIC_20", "SIC_28", "SIC_56", "SIC_50", "SIC_36", "SIC_58", "SIC_38", "SIC_33", "SIC_73", "SIC_80", "SIC_57", "SIC_35", "SIC_55", "SIC_51", "SIC_26", "SIC_70", "SIC_27", "SIC_79", "SIC_54", "SIC_99", "SIC_87", "SIC_37", "SIC_30", "SIC_39", "SIC_22", "SIC_10", "SIC_78", "SIC_13", "SIC_16", "SIC_24", "SIC_59", "SIC_82", "SIC_12", "SIC_32", "SIC_17", "SIC_83", "SIC_53", "SIC_75", "SIC_25", "SIC_15", "SIC_31", "SIC_14", "SIC_29", "SIC_81", "SIC_72", "SIC_52", "SIC_21"))

semna <- na.omit(dfols)
ignorandolevmaiorque1 <- subset(semna, LEV <= 1)

##summary statistics
library(stargazer)
stargazer(ignorandolevmaiorque1)


library(DescTools)
ignorandolevmaiorque1$MTB <- Winsorize(ignorandolevmaiorque1$MTB, probs = c(0.01, 0.99), na.rm = TRUE)

#Replace NaN & Inf with NA
ignorandolevmaiorque1[is.na(ignorandolevmaiorque1) | ignorandolevmaiorque1=="Inf"] = NA


#Winsorizando variáveis contínuas...

ignorandolevmaiorque1$LNDISTANCE <- Winsorize(ignorandolevmaiorque1$LNDISTANCE, probs = c(0.01, 0.99), na.rm = TRUE)
ignorandolevmaiorque1$TRAVEL <- Winsorize(ignorandolevmaiorque1$TRAVEL, probs = c(0.01, 0.99), na.rm = TRUE)
ignorandolevmaiorque1$SIGMA <- Winsorize(ignorandolevmaiorque1$SIGMA, probs = c(0.01, 0.99), na.rm = TRUE)
ignorandolevmaiorque1$RET <- Winsorize(ignorandolevmaiorque1$RET, probs = c(0.01, 0.99), na.rm = TRUE)
ignorandolevmaiorque1$ROA <- Winsorize(ignorandolevmaiorque1$ROA, probs = c(0.01, 0.99), na.rm = TRUE)
ignorandolevmaiorque1$SIZE <- Winsorize(ignorandolevmaiorque1$SIZE, probs = c(0.01, 0.99), na.rm = TRUE)
ignorandolevmaiorque1$LEV <- Winsorize(ignorandolevmaiorque1$LEV, probs = c(0.01, 0.99), na.rm = TRUE)
ignorandolevmaiorque1$Dias <- Winsorize(ignorandolevmaiorque1$Dias, probs = c(0.01, 0.99), na.rm = TRUE)


stargazer(ignorandolevmaiorque1)

#DUVOL lead
library(dplyr)
ignorandolevmaiorque1 <- ignorandolevmaiorque1 %>%
  group_by(CUSIPcorrigido) %>%
  mutate(DUVOL_lead = lead(DUVOL, n = 1))

#NCSKEW lead
library(dplyr)
ignorandolevmaiorque1 <- ignorandolevmaiorque1 %>%
  group_by(CUSIPcorrigido) %>%
  mutate(NCSKEW_lead = lead(NCSKEW, n = 1))

#COUNT lead
library(dplyr)
ignorandolevmaiorque1 <- ignorandolevmaiorque1 %>%
  group_by(CUSIPcorrigido) %>%
  mutate(COUNT_lead = lead(COUNT, n = 1))

#Replace NaN & Inf with NA
ignorandolevmaiorque1[is.na(ignorandolevmaiorque1) | ignorandolevmaiorque1=="Inf"] = NA

ols1 <- lm(DUVOL_lead ~ NCSKEW + HEADQUARTERS + SIGMA + RET + ROA + SIZE + MTB + LEV, data = ignorandolevmaiorque1)
ols2 <- lm(DUVOL_lead ~ HEADQUARTERS + SIGMA + RET + ROA + SIZE + MTB + LEV + SIC_34 + SIC_23 + SIC_20 + SIC_28 + SIC_56 + SIC_50 + SIC_36 + SIC_58 + SIC_38 + SIC_33 + SIC_73 + SIC_80 + SIC_57 + SIC_35 + SIC_55 + SIC_51 + SIC_26 + SIC_70 + SIC_27 + SIC_79 + SIC_54 + SIC_99 + SIC_87 + SIC_37 + SIC_30 + SIC_39 + SIC_22 + SIC_10 + SIC_78 + SIC_13 + SIC_16 + SIC_24 + SIC_59 + SIC_82 + SIC_12 + SIC_32 + SIC_17 + SIC_83 + SIC_53 + SIC_75 + SIC_25 + SIC_15 + SIC_31 + SIC_14 + SIC_29 + SIC_81 + SIC_72 + SIC_52, data = ignorandolevmaiorque1)

ols3 <- lm(NCSKEW_lead  ~ REMOTE + SIGMA + RET + ROA + SIZE + MTB + LEV, data = ignorandolevmaiorque1)
ols4 <- lm(NCSKEW_lead ~ REMOTE + SIGMA + RET + ROA + SIZE + MTB + LEV + SIC_34 + SIC_23 + SIC_20 + SIC_28 + SIC_56 + SIC_50 + SIC_36 + SIC_58 + SIC_38 + SIC_33 + SIC_73 + SIC_80 + SIC_57 + SIC_35 + SIC_55 + SIC_51 + SIC_26 + SIC_70 + SIC_27 + SIC_79 + SIC_54 + SIC_99 + SIC_87 + SIC_37 + SIC_30 + SIC_39 + SIC_22 + SIC_10 + SIC_78 + SIC_13 + SIC_16 + SIC_24 + SIC_59 + SIC_82 + SIC_12 + SIC_32 + SIC_17 + SIC_83 + SIC_53 + SIC_75 + SIC_25 + SIC_15 + SIC_31 + SIC_14 + SIC_29 + SIC_81 + SIC_72 + SIC_52, data = ignorandolevmaiorque1)

ols5 <- lm(NCSKEW_lead  ~ LNDISTANCE + SIGMA + RET + ROA + SIZE + MTB + LEV, data = ignorandolevmaiorque1)
ols6 <- lm(NCSKEW_lead ~ LNDISTANCE + SIGMA + RET + ROA + SIZE + MTB + LEV + SIC_34 + SIC_23 + SIC_20 + SIC_28 + SIC_56 + SIC_50 + SIC_36 + SIC_58 + SIC_38 + SIC_33 + SIC_73 + SIC_80 + SIC_57 + SIC_35 + SIC_55 + SIC_51 + SIC_26 + SIC_70 + SIC_27 + SIC_79 + SIC_54 + SIC_99 + SIC_87 + SIC_37 + SIC_30 + SIC_39 + SIC_22 + SIC_10 + SIC_78 + SIC_13 + SIC_16 + SIC_24 + SIC_59 + SIC_82 + SIC_12 + SIC_32 + SIC_17 + SIC_83 + SIC_53 + SIC_75 + SIC_25 + SIC_15 + SIC_31 + SIC_14 + SIC_29 + SIC_81 + SIC_72 + SIC_52, data = ignorandolevmaiorque1)

ols7 <- lm(NCSKEW_lead  ~ TRAVEL + SIGMA + RET + ROA + SIZE + MTB + LEV, data = ignorandolevmaiorque1)
ols8 <- lm(NCSKEW_lead ~ TRAVEL + SIGMA + RET + ROA + SIZE + MTB + LEV + SIC_34 + SIC_23 + SIC_20 + SIC_28 + SIC_56 + SIC_50 + SIC_36 + SIC_58 + SIC_38 + SIC_33 + SIC_73 + SIC_80 + SIC_57 + SIC_35 + SIC_55 + SIC_51 + SIC_26 + SIC_70 + SIC_27 + SIC_79 + SIC_54 + SIC_99 + SIC_87 + SIC_37 + SIC_30 + SIC_39 + SIC_22 + SIC_10 + SIC_78 + SIC_13 + SIC_16 + SIC_24 + SIC_59 + SIC_82 + SIC_12 + SIC_32 + SIC_17 + SIC_83 + SIC_53 + SIC_75 + SIC_25 + SIC_15 + SIC_31 + SIC_14 + SIC_29 + SIC_81 + SIC_72 + SIC_52, data = ignorandolevmaiorque1)

stargazer(ols1,ols2,ols3,ols4,ols5,ols6,ols7,ols8, font.size = "small")

stargazer(dfstargazer)

stargazer(ols1,ols2)

summary(ols6)

##criando dummies para ano

##fator = dummy de ano, ignorando 2010
testedummies <- factor(ignorandolevmaiorque1$fyear) 
testeindustry <- factor(ignorandolevmaiorque1$twodigitsic) 

ols8 <- lm(NCSKEW_lead ~ testedummies + TRAVEL + SIGMA + RET100 + ROA + SIZE + MTB + LEV + SIC_34 + SIC_23 + SIC_20 + SIC_28 + SIC_56 + SIC_50 + SIC_36 + SIC_58 + SIC_38 + SIC_33 + SIC_73 + SIC_80 + SIC_57 + SIC_35 + SIC_55 + SIC_51 + SIC_26 + SIC_70 + SIC_27 + SIC_79 + SIC_54 + SIC_99 + SIC_87 + SIC_37 + SIC_30 + SIC_39 + SIC_22 + SIC_10 + SIC_78 + SIC_13 + SIC_16 + SIC_24 + SIC_59 + SIC_82 + SIC_12 + SIC_32 + SIC_17 + SIC_83 + SIC_53 + SIC_75 + SIC_25 + SIC_15 + SIC_31 + SIC_14 + SIC_29 + SIC_81 + SIC_72 + SIC_52, data = ignorandolevmaiorque1)
summary(ols8)

testeindustry <- factor(MEETINGS$twodigitsic) 
print(testeindustry)

hist(ignorandolevmaiorque1$SIZE)

##CRIANDO LEADS para as variáveis dependentes...
##RETTIMES100

ignorandolevmaiorque1$RET100 <- ignorandolevmaiorque1$RET * 100
mean(ignorandolevmaiorque1$RET100)

#################working#################

## para CRIAR ESTATÍSTICA DESCRITIVA (TABELA) com composição setorial:
sum(dados_com_dummies$SIC_22 == 1)
#será calculado o número de observações por setor...





NCSKEW <- aggregate(ignorandolevmaiorque1$NCSKEW, by = list(ignorandolevmaiorque1$fyear), mean)
DUVOL <- aggregate(ignorandolevmaiorque1$DUVOL, by = list(ignorandolevmaiorque1$fyear), mean)







library(dplyr)

DF_MATCH_CONTROLES <- DF_MATCH_CONTROLES %>%
  group_by(CUSIPcorrigido) %>%
  mutate(DUVOL_lead = lead(DUVOL, n = 1))

library(DescTools)
DF_MATCH$RET_winsor <- Winsorize(DF_MATCH$RET, probs = c(0.01,0.99), na.rm = TRUE)


#Replace NaN & Inf with NA
DF_MATCH_CONTROLES[is.na(DF_MATCH_CONTROLES) | DF_MATCH_CONTROLES=="Inf"] = NA

ols <- lm(NCSKEW_lead ~ Dias.y + SIGMA + RET + ROA + SIZE + MTB + LEV, data = df_sub)
summary(ols)

library(stargazer)
stargazer(ols)

df_sub <- subset(DF_MATCH_CONTROLES, fyear >= 2009 & fyear <= 2019)
#WINSORIZAÇÃO A 
library(DescTools)
DF_MATCH$NCSKEW_winsor <- Winsorize(DF_MATCH$NCSKEW, probs = c(0.01,0.99), na.rm = TRUE)
DF_MATCH$LNDISTANCE_winsor <- Winsorize(DF_MATCH$LNDISTANCE, probs = c(0.01,0.99), na.rm = TRUE)

##explicando SIGMA negativo: 
#https://www.sciencedirect.com/science/article/pii/S0929119920301267?casa_token=FbScuE8W2lgAAAAA:r3n-P5nV_jaxYiWv9IY6-fMT-ZkEi1wDigIXIGQtMX9sIFQ3Fp3IwM6s6DJB3g1gXEk3DeXDSBc

###EXPORTAR ARQUIVO PARA TESTAR NO STATA...

STATA <- apply(DF_MATCH,2,as.character) #GAMBIARRA
write.csv(STATA, "STATA.csv", row.names = TRUE)

##########END
####################
install.packages("plm")
library(plm)

dados_painel <- pdata.frame(DF_MATCH_CONTROLES, index = c("CUSIPcorrigido", "fyear"))
modelo_fe <- plm(NCSKEW_lead ~ REMOTE + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dados_painel, model = "within")
summary(modelo_fe)

CRSP$fyear <- format(as.Date(CRSP$datefix), "%Y")

CRSP$fyear <- as.numeric(CRSP$fyear)
CRSP$CUSIPcorrigido <- substr(CRSP$CUSIP, 1, 6) ##considerando um match de 6 dígitos
MEETINGS$CUSIPcorrigido <- substr(MEETINGS$cusip, 1, 6) #considerando um match de 6 dígitos

MEETINGS <- merge(MEETINGS, CRSP, by = c("CUSIPcorrigido", "fyear"))
MEETINGS <- na.omit(DF_MATCH)


library(dplyr)
data_mod <- MEETINGS %>%                           
  group_by(CUSIPcorrigido) %>%
  dplyr::mutate(laggedval = lead(NCSKEW, n = 1, default = NA))

library(dplyr)
data_mod <- DF_MATCH %>%                           
  group_by(CUSIPcorrigido) %>%
  dplyr::mutate(laggedRET = lag(RET, n = 1, default = NA))

library(DescTools)
teste <- Winsorize(data_mod$LNDISTANCE, probs = c(0.01, 0.99), na.rm = TRUE)


df_wins <- apply(data_mod, 2, function(x) Winsorize(x, probs = c(0.01, 0.99)))