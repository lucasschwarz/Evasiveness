##Creating dataframe for summary statistics and analyzes
DF_MATCH$Days <- DF_MATCH$Dias
DF_MATCH$twodigitsic <- substr(DF_MATCH$sic, start = 1, stop = 2)

dfols <- subset(DF_MATCH, select = c("cusip","CUSIPcorrigido", "fyear", "conm", "HEADQUARTERS", "REMOTE","LNDISTANCE","TRAVEL", "NCSKEW","DUVOL","COUNT","SIGMA","RET","ROA","SIZE","MTB","LEV", "twodigitsic", "Dias"))

#Dropping firms with leverage higher than 1 (Negative Shareholders' Equity)
dfols <- subset(dfols, LEV <= 1)
#omiting NAs
dfols <- na.omit(dfols)
#replacing NaN & Inf with NAs
dfols[is.na(dfols) | dfols=="Inf"] = NA


##Winsorizing continuous variables, except stock price crash risk variables at 1% in each tail. 

library(DescTools)
dfols$LNDISTANCE <- Winsorize(dfols$LNDISTANCE, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$TRAVEL <- Winsorize(dfols$TRAVEL, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$SIGMA <- Winsorize(dfols$SIGMA, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$RET <- Winsorize(dfols$RET, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$ROA <- Winsorize(dfols$ROA, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$SIZE <- Winsorize(dfols$SIZE, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$LEV <- Winsorize(dfols$LEV, probs = c(0.01, 0.99), na.rm = TRUE)
dfols$MTB <- Winsorize(dfols$MTB, probs = c(0.01, 0.99), na.rm = TRUE)

#Creating RET times 100
dfols$RET <- dfols$RET * 100

#I'm not sure whether I should winsorize "Dias".
#dfols$Dias <- Winsorize(dfols$Dias, probs = c(0.01, 0.99), na.rm = TRUE)

dfols <- as.data.frame(dfols)


##Creating lags for independent variables
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

#omiting NAs from lags (should I?)
dfols <- na.omit(dfols)
dfols <- subset(dfols, fyear != 2010)


##Summary statistics
dfstargazer <- subset(dfols, select = c("cusip","CUSIPcorrigido", "fyear", "conm", "HEADQUARTERS", "REMOTE","LNDISTANCE","TRAVEL", "NCSKEW_lead","DUVOL_lead","COUNT_lead","SIGMA","RET","ROA","SIZE","MTB","LEV", "Dias","NCSKEW"))
dfstargazer <- as.data.frame(dfstargazer)
stargazer(dfstargazer)

#Industry & year fixed-effects
year_fe <- factor(dfols$fyear) 
industry_fe <- factor(dfols$twodigitsic) 

#Creating a table with observations by industry
contagem <- table(dfols$twodigitsic)
contagem$var <- table(dfols$twodigitsic)
teste <- data.frame(contagem)

#Creating a table with observations by year
contagemano <- table(dfols$fyear)
contagemano$var <- table(dfols$fyear)


##Analyzes
#Analyzes: Main Hypotheses
#H1: Distance-based evasiveness and stock price crash risk

ols1 <- lm(NCSKEW ~ HEADQUARTERS + SIGMA_lag + RET_lag + ROA_lag + SIZE_lag + MTB_lag + LEV_lag, data = dfols)
ols2 <- lm(NCSKEW ~ HEADQUARTERS + SIGMA_lag + RET_lag + ROA_lag + SIZE_lag + MTB_lag + LEV_lag + year_fe + industry_fe, data = dfols)
ols3 <- lm(NCSKEW_lead ~ LNDISTANCE + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)
ols4 <- lm(NCSKEW_lead ~ LNDISTANCE + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)
ols5 <- lm(NCSKEW_lead ~ REMOTE + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)
ols6 <- lm(NCSKEW_lead ~ REMOTE + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)
ols7 <- lm(NCSKEW_lead ~ TRAVEL + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)
ols8 <- lm(NCSKEW_lead ~ TRAVEL + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)

stargazer(ols1,ols2,ols3,ols4,ols5,ols6,ols7,ols8, font.size = "small")



#Additional analysis: relationship between commitment date lag and stock price crash risk 

ols9 <- lm(NCSKEW_lead ~ Dias + SIGMA + RET + ROA + MTB + LEV, data = dfols)
ols10 <- lm(NCSKEW_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)
ols11 <- lm(DUVOL_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)
ols12 <- lm(DUVOL_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)
ols13 <- lm(COUNT_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV, data = dfols)
ols14 <- lm(COUNT_lead ~ Dias + SIGMA + RET + ROA + SIZE + MTB + LEV + year_fe + industry_fe, data = dfols)

stargazer(ols9,ols10,ols11,ols12,ols13,ols14, font.size = "small")

#Additional analysis: extremely evasive firms
##Interaction between distance-based evasiveness and timing-based evasiveness







#Robustness checks

#Firm-fixed effects

install.packages("plm")
library(plm)
plm <- plm(DUVOL ~ HEADQUARTERS_lag + SIGMA_lag + RET_lag + ROA_lag + SIZE_lag + MTB_lag + LEV_lag, data = dfols, model = "within")


##

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


##psm pelo gp_t

propensity <- glm(HEADQUARTERS_lag ~ SIGMA_lag + RET_lag + ROA_lag + SIZE_lag + MTB_lag + LEV_lag, data = dfols, family = binomial)
treatment$propensity_score <- predict(propensity, type = "response")


##ENTROPY balancing (será válido?)
install.packages("EBweights")
install.packages("EBglm")
library(EBweights)
library(EBglm)


weights <- EBweights(treatment ~ covariate_1 + covariate_2, data = df)

#Different threshold for REMOTE variable. 25, instead 50 miles (50 miles is too restrictive).



