##ANALYZES 
  

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




