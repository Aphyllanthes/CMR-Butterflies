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

## model function ------------
Closed_model <- function(
  formula, inp_data, name = "", predictor = ""
) {
  inp_data_process1 <- process.data(inp_data, model = "Closed")
  inp_design1 <- RMark::make.design.data(inp_data_process1)
  
  if(is.numeric(predictor)){
    inp_design1$p$predictor <- predictor
    inp_design1$c$predictor <- predictor[2:length(predictor)]
  }
  model.parameters = list(p = formula)
  m0 <- RMark::mark(data = inp_data_process1, 
                    ddl = inp_design1, 
                    model = "Closed",
                    model.parameters = model.parameters
                    )
                    
  res <- m0$results$derived$`N Population Size`
  res <- res %>% 
    mutate(name = name, AIC = m0$results$AICc, modeltype = "Closed")
  if(exists("results")) {
    results <<- rbind(results, res)
  } else {results <<- res}
  return(m0)
}

#1 Closed models ###############

#1.1 only intercept
m0_result <- Closed_model(
  formula = list(formula = ~1, share = TRUE), 
  inp_data = inp_data, name = "m0"
)

##1.2 model mt: The session number is a factorial number
mt_result <- Closed_model(
  formula = list(formula = ~1 + time, share = TRUE), 
  inp_data = inp_data, name = "mt"
)

##1.3 model mtb: different probability for recaptures
# mtb_result <- Closed_model(
#   formula = list(formula = ~1 + time + c, share = T), ## share = T used to be working but now is not...  
#   inp_data = inp_data, name = "mtb"
# )

##1.4- model mTrend: Trend of 
mtrend_result <- Closed_model(
  formula = list(formula = ~1 + Time, share = TRUE), 
  inp_data = inp_data, name = "mtrend"
)

##1.5 model with weather
mw_result <- Closed_model(
  formula = list(formula = ~1 + predictor, share = TRUE), 
  inp_data = inp_data, name = "mw",
  predictor = c(0.5, .3, .2, .8, .4, .9, 1, 1)
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

collect.models()

## Also try POPAN model (open population)
#to test for closure violation. use model to explore the closure violation. 
#phi


#pent p of entrance
p=list(formula=~1 )
Phi=list(formula=~1) #probability of survival
pent=list(formula=~1) # probability of entrance

mPOPAN_result <- model_fun(
  modeltype = "POPAN",
  formula_p = p, formula_phi = Phi, formula_pent = pent, 
  inp_data = inp_data, name = "mpopan", addresults=F)

mPOPAN_result$results$derived$`Gross N* Population Size`
mPOPAN_result$results$derived


Phi=list(formula=~1, fixed=1) #probability of survival
pent=list(formula=~1, fixed=0) # probability of entrance

mPOPAN2_result <- model_fun(
  modeltype = "POPAN", formula_p = p, formula_phi = Phi ,
  formula_pent = pent, addresults=F,
  inp_data = inp_data, name = "mpopan2")

mPOPAN2_result$results$derived$`Gross N* Population Size`


# open with time
p=list(formula=~1 + time )
Phi=list(formula=~1) #probability of survival
pent=list(formula=~1 ) # probability of entrance

mPOPANtime_result <- model_fun(
  modeltype = "POPAN",
  formula_p = p, formula_phi = Phi ,
  formula_pent = pent, addresults=F,
  inp_data = inp_data, name = "mpopan_t")

mPOPANtime_result$results$derived$`Gross N* Population Size`

## not working:
p=list(formula=~1 + predictor )

mPOPANw_result <- model_fun(
  modeltype = "POPAN",
  predictor = c(0.5, .3, .2, .8, .4, .9, 1, 1),
  formula_p = p, formula_phi = Phi ,
  formula_pent = pent, addresults=F,
  inp_data = inp_data, name = "mpopan_w")

#pradlamda
p=list(formula=~1)
Phi=list(formula=~1) #probability of survival
Lambda=list(formula=~1) # probability of entrance

mPradlambda_result <- model_fun(
  modeltype = "Pradlambda",
  formula_p = p, formula_phi = Phi, formula_lambda = Lambda, 
  inp_data = inp_data, name = "mPradlambda", addresults = F)

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