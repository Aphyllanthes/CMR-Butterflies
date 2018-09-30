library(dplyr)
library(tidyr)
library(RMark)

## load data: ------------
raw_data <- read.csv("Project/raw_data_8.csv")

raw_data_2 <- raw_data %>% 
  mutate_at(
    vars(grep("s",colnames(.))), 
    .funs = funs(ifelse(. >1, 1, .))
  ) %>% 
  mutate(ch = paste0(!!!.[,grep("s",colnames(.))]))

inp_data <- raw_data_2 %>% 
  select(ch) %>% 
  mutate(freq = 1)

## Popan modell
inp_data_process1 <- process.data(inp_data, model = "POPAN")
inp_design1 <- RMark::make.design.data(inp_data_process1)

# inp_design1$p$predictor <- predictor
# inp_design1$c$predictor <- predictor[2:length(predictor)]

model.parameters = list(p = list(formula=~1 ),
                        Phi = list(formula=~1 ), 
                        pent = list(formula=~1 ))
mPOPAN <- RMark::mark(data = inp_data_process1, 
                  ddl = inp_design1, 
                  model = "POPAN",
                  model.parameters = model.parameters
)

## 
model.parameters = list(p = list(formula=~1 ),
                        Phi = list(formula=~1 , fixed=1), 
                        pent = list(formula=~1, fixed=0 ))
mPOPAN2 <- RMark::mark(data = inp_data_process1, 
                      ddl = inp_design1, 
                      model = "POPAN",
                      model.parameters = model.parameters
)

## open with time
model.parameters = list(p = list(formula=~1 + time ),
                        Phi = list(formula=~1), #probability of survival
                        pent = list(formula=~1)) # probability of entrance
mPOPANt <- RMark::mark(data = inp_data_process1, 
                       ddl = inp_design1, 
                       model = "POPAN",
                       model.parameters = model.parameters
)

## mPopan weather
inp_design1$p$weather <- c(0.5, .3, .2, .8, .4, .9, 1, 1)
inp_design1$c$weather <- c( .3, .2, .8, .4, .9, 1, 1)

model.parameters = list(p = list(formula=~1 + weather),
                        Phi = list(formula=~1), #probability of survival
                        pent = list(formula=~1)) # probability of entrance
mPOPANw <- RMark::mark(data = inp_data_process1, 
                       ddl = inp_design1, 
                       model = "POPAN",
                       model.parameters = model.parameters
)

#pradlamda
inp_data_process1 <- process.data(inp_data, model = "Pradlambda")
inp_design1 <- RMark::make.design.data(inp_data_process1)

model.parameters = list(p = list(formula=~1),
                        Phi = list(formula=~1), #probability of survival
                        Lambda = list(formula=~1)) # probability of entrance
mPradLambda <- RMark::mark(data = inp_data_process1, 
                       ddl = inp_design1, 
                       model = "Pradlambda",
                       model.parameters = model.parameters
)




## model function ------------
model_fun <- function(
  modeltype = "FullHet", formula, inp_data, name = "", predictor = "", 
  formula_p= "", formula_pent = "", formula_phi="", formula_lambda = "", addresults = T
) {
  inp_data_process1 <- process.data(inp_data, model = modeltype)
  inp_design1 <- RMark::make.design.data(inp_data_process1)
  
  if(is.numeric(predictor)){
    inp_design1$p$predictor <- predictor
    inp_design1$c$predictor <- predictor[2:length(predictor)]
  }
  if(! modeltype %in% c("POPAN", "Pradlambda")){
    model.parameters = list(p = formula)
  } else {
    model.parameters = list(p = formula_p,
                            Phi = formula_phi)
    ifelse(modeltype == "POPAN", model.parameters$pent <- formula_pent,
           model.parameters$Lambda <- formula_lambda)
  }

  m0 <- RMark::mark(data = inp_data_process1, 
                    ddl = inp_design1, 
                    model = modeltype,
                    model.parameters = model.parameters
  )
  
  if(addresults == T){
    res <- m0$results$derived$`N Population Size`
    res <- res %>% 
      mutate(name = name, AIC = m0$results$AICc, modeltype = modeltype)
    if(exists("results")) {
      results <<- rbind(results, res)
    } else {results <<- res} 
  }
  return(m0)
}

#2 FullHet models ###############
#2.1
mth_result <- model_fun(
  "FullHet", 
  formula = list(formula = ~1 + time + mixture, share = TRUE), 
  inp_data = inp_data, name = "mth"
)

## FullHet model with the weather and mixture:
mwh_result <- model_fun(
  "FullHet", 
  formula = list(formula = ~1 + predictor + mixture, share = TRUE), 
  inp_data = inp_data, name = "mwh",
  predictor = c(0.5, .3, .2, .8, .4, .9, 1, 1)
)

# ## model with weather and behavioral response
# mwb_result <- model_fun(
#   "FullHet", 
#   formula = list(formula = ~1 + predictor + c, share = TRUE), 
#   inp_data = inp_data, name = "mwb",
#   predictor = c(0.5, .3, .2, .8, .4)
# )

## m0h
m0h_result <- model_fun(
  "FullHet", 
  formula = list(formula = ~1, share = TRUE), 
  inp_data = inp_data, name = "m0h"
)

mtb_result <- model_fun(
  "FullHet", 
  formula = list(formula = ~1 + c, share = TRUE), 
  inp_data = inp_data, name = "mtb"
)

collect.models()

## Also try POPAN model (open population)
#to test for closure violation. use model to explore the closure violation. 
#phi

mPOPAN_result$results$derived$`Gross N* Population Size`
mPOPAN_result$results$derived


mPOPAN2_result$results$derived$`Gross N* Population Size`

## Test of clousure. Capture software on line. 


## Cleaning the folders:
junk <- dir(pattern=".tmp")
file.remove(junk)
junk <- dir(pattern=".vcv")
file.remove(junk)
junk <- dir(pattern=".res")
file.remove(junk)
junk <- dir(pattern=".out")
file.remove(junk)
junk <- dir(pattern=".inp")
file.remove(junk)

junk <- dir(path = "Project/", pattern=".tmp")
file.remove(paste0("Project/",junk))
junk <- dir(path = "Project/", pattern=".vcv")
file.remove(paste0("Project/",junk))
junk <- dir(path = "Project/", pattern=".res")
file.remove(paste0("Project/",junk))
junk <- dir(path = "Project/", pattern=".out")
file.remove(paste0("Project/",junk))
junk <- dir(path = "Project/", pattern=".inp")
file.remove(paste0("Project/",junk))

## Notes: ###################
# dying of
# migration from and to the study site
# biased sampling (higher probability for recaptures than captures)
# 

# Plants:
### Nardus stricta
### Juncus trifidus
### Deschampsia flexuosa (?)
## Flowering:
### Campanula scheuchzerii
### Gentiana spec.
### Phyteuma hemisphaericum
### Hieracium spec.

## from the path at the river: Molinia caerulea


## literature:

# capture-recapture and reomoval methods for sampling closed populations, 
#Los Alamos national laboratory

#Handbook of capture -recapture analysis. 
#Editeed by Stevens C. Amstrup, Trent L. McDonald and Bryan F.J. Manly. 
#Princeton university press