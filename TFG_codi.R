# CODI UTILITZAT TFG
## Autor: Berta Ortiz Arissó

################################################################################

## Llibreries utilitzades

library(openxlsx) 
library(dplyr)
library(ggplot2)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)

################################################################################

## 1. Importació de la base de dades

codes <- read.xlsx("Diseño registro ADULTO ENSE 2017_PUBLICACIÓN.xlsx",
                   colNames = FALSE)

names(codes) <- c("variable", "width", "begin", "end", "description")
codes <- codes[complete.cases(codes), ]
codes <- codes[codes$width != "LONGITUD", ]
rownames(codes) <- NULL

ense17full <- read.fwf("MICRODAT.CA.txt",
                       widths = as.numeric(codes$width),
                       col.names = codes$variable)
ense17codes <- codes[, c("variable", "description")]

ense17 <- ense17full # Guardem la base de dades original


## 2. Condicions de la mostra

  #2.1: L'informant és la persona seleccionada 
ense17 <- subset(ense17, ense17$PROXY_0 == 1)

  #2.2: La persona enquestada esta actualment traballant
ense17 <- subset(ense17, ense17$ACTIVa == 1)
nrow(ense17) 

  #2.3: L'enquestat ha contestat la variable d'interès
table(ense17$M47a)
ense17 <- ense17 %>%
  filter(!(M47a %in% c(8, 9)))
nrow(ense17) 


## 3. Recodificació de l'outcome (variable d'interès)

table(ense17$M47a)
summary(ense17$M47a)

ense17$M47a <- ifelse(ense17$M47a <= 4, "baix", "alt") 
table(ense17$M47a)

## 4. Significació de variables

## 5. Selecció de variables

ense17 <- ense17 %>%
  select(-IDENTHOGAR, -A7_2a, - ACTIVa)

# PROXYS
ense17 <- ense17 %>% select(-starts_with("PROXY"))

# E. CARACTERÍSTICAS DEMOGRÁFICAS
ense17 <- ense17  %>%  select(-E1_1, -E2_1b, -E2_1c, -E2_1d, - E3, -E4)

# F. RELACIÓN CON LA ACTIVIDAD ECONOMICA
ense17 <- ense17 %>%
  select(-starts_with("F"), F13, F15, F16, F17, F18a_2, F19a_2, FACTORADULTO) 

# G.ESTADO DE SALUD "ESPECÍFICO"
ense17 <- ense17 %>%
  select(-G24, -starts_with("G25"), G25a_20, G25a_21, G25b_20, G25b_21)

# H.ACCIDENTALIDAD
ense17 <- ense17 %>%
  select(-starts_with("H"))

# I. RESTRICCIÓN DE LA ACTIVIDAD
ense17 <- ense17 %>%
  select(-starts_with("I"), I28_1, IMCa)

#K. LIMITACIONES, FÍSICAS , SENSORIALES Y COGNITIVAS
ense17 <- ense17 %>%
  select(-starts_with("K"))

#L. LIMITACIONES PARA LA REALIZACIÓN DE LAS ACTIVIDADES DE LA VIDA COTIDIANA
ense17 <- ense17 %>%
  select(-starts_with("L"), L45, L46) 


#N. CONSULTAS MÉDICAS Y OTROS SERVICIOS AMBULATORIOS
ense17 <- ense17 %>%
  select(-starts_with("N"), NIVEST)

#O. HOSPITALIZACIONES y URGENCIAS (dejamos seguro sanitario)

ense17 <- ense17 %>%
  select(-starts_with("O"), O66, O75, O84_2, O84_3, O84_4)

##P. CONSUMO DE MEDICAMENTOS (2 semanas)
ense17 <- ense17 %>%
  select(-starts_with("P"), P85, P86, P87_2a, P87_4a, P87_7a, P87_12a, P87_14a)

#Q. PRÁCTICAS PREVENTIVAS
ense17 <- ense17 %>%
  select(-starts_with("Q"))

#R. NECESIDADES DE ATENCIÓN NO CUBIERTAS
ense17 <- ense17 %>% 
  select(-R108_2)

#S. CARACTERÍSTICAS FÍSICAS 
ense17 <- ense17 %>%
  select(-starts_with("S"), SEXOa)

#T. ACTIVIDAD FÍSICA 
ense17 <- ense17 %>%
  select(-T114_1, -T114_2, -T116_1, -T116_2, -T118_1, -T118_2, -T119_2)

#U. ALIMENTACIONS
ense17 <- ense17 %>%
  select(-U120_1a, -U120_7a, -U120_15a, -U120FZ, -U120CANTFZ)

#U2. HIGIENE DENTAL
ense17 <- ense17 %>%
  select(-starts_with("U2"))

#V. TABACO
ense17 <- ense17 %>%
  select(-starts_with("V"), V121)

#W. CONSUMO DE ALCOHOL (W128)
ense17 <- ense17 %>%
  select(-starts_with("W128"), -W129)

#Altres variables que no son rellevants:
ense17 <- ense17 %>%
  select(-Y134, -Y135)


## 6. Base de dades pre-definida

summary(ense17)
ncol(ense17)

## 6.1. Inchoerències

###6.1.1.BMI
summary(ense17$IMCa)

df.bmi <- ense17full %>%
  filter(ACTIVa == 1) %>%
  select(IMC_categoria = IMCa, altura_cm = S109, peso_kg = S110) %>%
  mutate(
    altura_m = altura_cm / 100,
    bmi_calculado = peso_kg / (altura_m ^ 2),
    IMC_calc_cat = case_when(
      bmi_calculado < 18.5 ~ 1,              # Peso insuficiente
      bmi_calculado < 25   ~ 2,              # Normopeso
      bmi_calculado < 30   ~ 3,              # Sobrepeso
      bmi_calculado >= 30  ~ 4,              # Obesidad
      TRUE ~ NA_integer_
    ),
    inconsistente = IMC_categoria != IMC_calc_cat
  )

sum(df.bmi$inconsistente, na.rm = TRUE)
head(df.bmi[df.bmi$inconsistente == TRUE, ])

table(ense17$IMCa)

## 6.1.2. Treball
summary(ense17$EDADa)

Q1 <- quantile(ense17$EDADa, 0.25, na.rm = TRUE)
Q3 <- quantile(ense17$EDADa, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers_edad <- ense17 %>% filter(EDADa < lower_bound | EDADa > upper_bound)

ense17 %>%
  mutate(fila = row_number()) %>%     
  filter(EDADa > 77) %>%
  select(fila, EDADa, F19a_2) %>%
  arrange(EDADa)

## 6.2. Recodificació de variables

assignar_seccio <- function(codi) {
  codi <- as.numeric(substr(codi, 1, 2))  
  if (codi >= 1   & codi <= 3)   return("A")  # Agricultura
  if (codi >= 5   & codi <= 9)   return("B")  # Indústries extractives
  if (codi >= 10  & codi <= 33)  return("C")  # Indústria manufacturera
  if (codi == 35)                return("D")  # Subministrament d'energia
  if (codi >= 36  & codi <= 39)  return("E")  # Aigua i residus
  if (codi >= 41  & codi <= 43)  return("F")  # Construcció
  if (codi >= 45  & codi <= 47)  return("G")  # Comerç
  if (codi >= 49  & codi <= 53)  return("H")  # Transport
  if (codi >= 55  & codi <= 56)  return("I")  # Hostaleria
  if (codi >= 58  & codi <= 63)  return("J")  # Informació i comunicació
  if (codi >= 64  & codi <= 66)  return("K")  # Finances i assegurances
  if (codi >= 68  & codi <= 68)  return("L")  # Activitats immobiliàries
  if (codi >= 69  & codi <= 75)  return("M")  # Professionals, ciència i tècnica
  if (codi >= 77  & codi <= 82)  return("N")  # Serveis administratius
  if (codi >= 84  & codi <= 84)  return("O")  # Administració pública
  if (codi >= 85  & codi <= 85)  return("P")  # Educació
  if (codi >= 86  & codi <= 88)  return("Q")  # Sanitat i serveis socials
  if (codi >= 90  & codi <= 93)  return("R")  # Arts i oci
  if (codi >= 94  & codi <= 96)  return("S")  # Altres serveis
  if (codi >= 97  & codi <= 98)  return("T")  # Llars com a ocupadors
  if (codi == 99)                return("U")  # Organismes extraterritorials
  return(NA)
}

ense17$F18a_2Recode <- sapply(ense17$F18a_2, assignar_seccio)
prop.table(table(ense17$F18a_2Recode))
ense17 <- ense17 %>%
  select(-F18a_2)

ense17$F19a_2Recode1 <- substr(ense17$F19a_2, 1, 1)
ense17$F19a_2Recode2 <- substr(ense17$F19a_2, 1, 2)

prop.table(table(ense17$F19a_2Recode1))
prop.table(table(ense17$F19a_2Recode2))

ense17 <- ense17 %>%
  select(-F19a_2, -F19a_2Recode2)

##6.2.2. Valors NS/NC

recode_89 <- function(data, vars) {
  for (var in vars) {
    data[[var]] <- as.character(data[[var]])
    data[[var]][data[[var]] %in% c("8", "9")] <- "89"
    data[[var]] <- as.factor(data[[var]])
  }
  return(data)
}

vars <- names(ense17)
vars <- vars[-c(1,2, 62:64, 65, 68, 69, 78)]
ense17 <- recode_89(ense17, vars)
summary(ense17)

ense17$F13[ense17$F13 %in% c("98", "99")] <- "89"
ense17$F17[ense17$F17 %in% c("98", "99")] <- "89"

## 7. NA's


## 7.1. Variables Activitat fisica

ense17 %>%
  filter(is.na(T113)) %>%          # seleccionem només les files amb NA a T113
  summarise(
    total_na = n(),                                # total de NA a T113
    majors_70 = sum(EDADa > 70, na.rm = TRUE),      # quants tenen més de 70 anys
    percent_majors_70 = mean(EDADa > 70, na.rm = TRUE) * 100  # percentatge
  )

ense17$T113[is.na(ense17$T113)] <- "89"


ense17 %>%
  filter(is.na(T115)) %>%          # seleccionem només les files amb NA a T113
  summarise(
    total_na = n(),                                # total de NA a T113
    majors_70 = sum(EDADa > 70, na.rm = TRUE),      # quants tenen més de 70 anys
    percent_majors_70 = mean(EDADa > 70, na.rm = TRUE) * 100  # percentatge
  )

ense17$T115[is.na(ense17$T115)] <- "89"


ense17 %>%
  filter(is.na(T117)) %>%          # seleccionem només les files amb NA a T113
  summarise(
    total_na = n(),                                # total de NA a T113
    majors_70 = sum(EDADa > 70, na.rm = TRUE),      # quants tenen més de 70 anys
    percent_majors_70 = mean(EDADa > 70, na.rm = TRUE) * 100  # percentatge
  )

ense17$T117[is.na(ense17$T117)] <- "89"


ense17 %>%
  filter(is.na(T119_1)) %>%          # seleccionem només les files amb NA a T113
  summarise(
    total_na = n(),                                # total de NA a T113
    majors_70 = sum(EDADa > 70, na.rm = TRUE),      # quants tenen més de 70 anys
    percent_majors_70 = mean(EDADa > 70, na.rm = TRUE) * 100  # percentatge
  )

ense17$T119_1[is.na(ense17$T119_1)] <- "89"



### 7.2. Variables G25

ense17 %>%
  filter(G25a_20 == 2) %>%
  summarise(tot_na = all(is.na(G25b_20))) 

sum(is.na(ense17$G25b_20))
sum(ense17$G25a_20 == 2)

ense17 <- ense17 %>%
  mutate(
    G25b_20 = ifelse(G25a_20 == 2, 2, G25b_20)
  )
sum(is.na(ense17$G25b_20))

ense17 <- ense17 %>%
  select(-G25a_20)


ense17 %>%
  filter(G25a_21 == 2) %>%
  summarise(tot_na = all(is.na(G25b_21))) 

sum(is.na(ense17$G25b_21))
sum(ense17$G25a_21 == 2)

ense17 <- ense17 %>%
  mutate(
    G25b_21 = ifelse(G25a_21 == 2, 2, G25b_21)
  )
sum(is.na(ense17$G25b_21))

ense17 <- ense17 %>%
  select(-G25a_21)

### 7.3. Variables P

vars <- c("P87_2a", "P87_4a", "P87_7a", "P87_12a", "P87_14a")

for (var in vars) {
  tot_na <- ense17 %>%
    filter(P85 == 2 & P86 == 2) %>%
    summarise(tot_na = all(is.na(.data[[var]])))
  print(paste0(var, ": todos NA en subset? ", tot_na$tot_na))
}

for (var in vars) {
  cat(var, "- NAs antes:", sum(is.na(ense17[[var]])), "\n")
}

ense17 <- ense17 %>%
  mutate(
    across(all_of(vars), ~ ifelse(P85 == 2 & P86 == 2, 2, .))
  )

for (var in vars) {
  cat(var, "- NAs después:", sum(is.na(ense17[[var]])), "\n")
}


rows_con_na <- ense17 %>%
  mutate(row_id = row_number()) %>%
  filter(if_any(all_of(vars), is.na)) %>%
  pull(row_id)

### 7.4. Variable F

sum(is.na(ense17$F18a_2Recode))
which(is.na(ense17$F18a_2))

sum(complete.cases(ense17))
ense17 <- ense17[complete.cases(ense17), ]

dim(ense17)

## 9. Models

set.seed(1632830)
train_idx <- createDataPartition(ense17$M47a, p = 0.8, list = FALSE)
train_data <- ense17[train_idx, ]
test_data <- ense17[-train_idx, ]

## 9.1. Arbres de classificació

  # Model sense pesos
model_tree <- rpart(M47a ~ .  -FACTORADULTO, 
                    data = train_data,
                    method = "class")
rpart.plot(model_tree)
model_tree_pred <- predict(model_tree, newdata = test_data, type = "class")
model_tree_cm <- confusionMatrix(model_tree_pred, test_data$M47a)

model_tree_probs <- predict(model_tree, newdata = test_data, type = "prob")[, "baix"]
model_tree_ROC <- roc(test_data$M47a, model_tree_probs)
plot(model_tree_ROC, col = "blue", print.auc = TRUE)


  # Model amb pesos
model_tree_w <- rpart(M47a ~ .-FACTORADULTO, 
                    data = train_data,
                    method = "class",
                    weights = train_data$FACTORADULTO)
rpart.plot(model_tree_w)
model_tree_w_pred <- predict(model_tree_w, newdata = test_data, type = "class")
model_tree_w_cm <- confusionMatrix(model_tree_w_pred, test_data$M47a)

model_tree_w_probs <- predict(model_tree_w, newdata = test_data, type = "prob")[, "baix"]
model_tree_w_ROC <- roc(test_data$M47a, model_tree_w_probs)
plot(model_tree_w_ROC, col = "blue", print.auc = TRUE)

  # Comparació
tab_dt <- table(
  model_tree_pred == test_data$M47a,
  model_tree_w_pred == test_data$M47a
)

mcnemar.test(tab_dt)

## 9.2. Random Forest

# Entrenament amb cross-validation dins train_data
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,  
  savePredictions = "final"
)

# Model sense pesos
model_rf <- train(M47a ~ . -FACTORADULTO, 
                  data = train_data,
                  method = "rf",
                  metric = "ROC", 
                  tuneGrid = expand.grid(mtry = 7:12),
                  trControl = ctrl, 
                  num.trees = 500)

importance_rf <- varImp(model_rf)
print(importance_rf)
plot(importance_rf, top = 15)

model_rf_pred <- predict(model_rf, newdata = test_data)
model_rf_probs <- predict(model_rf, newdata = test_data, type = "prob")

model_rf_cm <- confusionMatrix(model_rf_pred, test_data$M47a, positive = "alt")
roc_rf <- roc(response = test_data$M47a, predictor = model_rf_probs[, "alt"])
auc(roc_rf)

# Model amb pesos
model_rf_w <- train(M47a ~ . -FACTORADULTO, 
                    data = train_data,
                    method = "rf",
                    metric = "ROC", 
                    weights = train_data$FACTORADULTO,   
                    tuneGrid = expand.grid(mtry = 7:12),
                    trControl = ctrl, 
                    num.trees = 500)

importance_rf_w <- varImp(model_rf_w)
print(importance_rf_w)
plot(importance_rf_w, top = 15)

model_rf_w_pred <- predict(model_rf_w, newdata = test_data)
model_rf_w_probs <- predict(model_rf_w, newdata = test_data, type = "prob")

model_rf_w_cm <- confusionMatrix(model_rf_w_pred, test_data$M47a, positive = "alt")
roc_rf_w <- roc(response = test_data$M47a, predictor = model_rf_w_probs[, "alt"])
auc(roc_rf_w)

# Comparació
tab <- table(
  model_rf_pred == test_data$M47a,
  model_rf_w_pred == test_data$M47a
)

mcnemar.test(tab)


## 9.3. Gradient Boosting

tune_grid <- expand.grid(
  nrounds = 200,
  max_depth = 5,
  eta = 0.05,
  gamma = 0,
  colsample_bytree = 0.75,
  min_child_weight = 1,
  subsample = 0.75
)

# Model sense pesos
model_gb <- train(M47a ~. -FACTORADULTO,
                  data = train_data, 
                  method = "xgbTree",
                  trControl = ctrl,
                  tuneGrid = tune_grid)

varImpPlot <- varImp(model_gb)
plot(varImpPlot, top = 15)

model_gb_pred <- predict(model_gb, newdata = test_data)
model_gb_probs <- predict(model_gb, newdata = test_data, type = "prob")

model_gb_cm <- confusionMatrix(model_gb_pred, test_data$M47a, positive = "alt")
roc_gb <- roc(response = test_data$M47a, predictor = model_gb_probs[, "alt"])
auc(roc_gb)

# Model amb pesos

model_gb_w <- train(M47a ~. -FACTORADULTO,
                    data = train_data, 
                    method = "xgbTree",
                    weights = train_data$FACTORADULTO,
                    trControl = ctrl,
                    tuneGrid = tune_grid)
varImpPlot <- varImp(model_gb_w)
plot(varImpPlot, top = 15)

model_gb_w_pred <- predict(model_gb_w, newdata = test_data)
model_gb_w_probs <- predict(model_gb_w, newdata = test_data, type = "prob")

model_gb_w_cm <- confusionMatrix(model_gb_w_pred, test_data$M47a, positive = "alt")
roc_gb_w <- roc(response = test_data$M47a, predictor = model_gb_w_probs[, "alt"])
auc(roc_gb_w)

# Comparació
tab <- table(
  model_gb_pred == test_data$M47a,
  model_gb_w_pred == test_data$M47a
)

mcnemar.test(tab)

### ANNEX: Descriptives

prop.table(table(ense17$M47a, ense17$SEXOa), margin = 2)
  #S'ha repetit aquest codi per a totes les variables necessaries

### ANNEX: Resultats

plot(roc(test_data$M47a, model_tree_probs), 
     col = "#87CEFA", 
     lwd = 2,
     print.auc = FALSE,
     main = "Comparació corbes ROC")

lines(roc(test_data$M47a, model_rf_probs[, "alt"]), col = "#9370DB", lwd = 2)  
lines(roc(test_data$M47a, model_gb_probs[, "alt"]), col = "#FF69B4", lwd = 2)  

lines(roc(test_data$M47a, model_tree_w_probs), col = adjustcolor("#87CEFA", alpha.f = 0.5), lwd = 2, lty = 2)
lines(roc(test_data$M47a, model_rf_w_probs[, "alt"]), col = adjustcolor("#9370DB", alpha.f = 0.5), lwd = 2, lty = 2)
lines(roc(test_data$M47a, model_gb_w_probs[, "alt"]), col = adjustcolor("#FF69B4", alpha.f = 0.5), lwd = 2, lty = 2)

legend("bottomright", legend = c("Arbre sense pesos", "Random Forest sense pesos", "Gradient Boosting sense pesos",
                                 "Arbre amb pesos", "Random Forest amb pesos", "Gradient Boosting amb pesos"),
       col = c("#87CEFA", "#9370DB", "#FF69B4", 
               adjustcolor("#87CEFA", alpha.f = 0.5),
               adjustcolor("#9370DB", alpha.f = 0.5),
               adjustcolor("#FF69B4", alpha.f = 0.5)),
       lwd = 1, lty = c(1,1,1,2,2,2),
       cex = 0.7)
