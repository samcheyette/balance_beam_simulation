
data {
    int NROWS; 
    int NSUBJ;
    int NITEMS;
    
    int<lower=0,upper=1> weightSideLeft[NROWS]; // binary L/R
    int<lower=0,upper=1> lowMemLoad[NROWS];
    int<lower=0,upper=1> highMemLoad[NROWS];

    int subject[NROWS];
    int item[NROWS];
    
    real modLeft[NROWS];
    real modBal[NROWS];
    real modRight[NROWS];
    
    int<lower=0, upper=1> choseLeft[NROWS]; 
    int<lower=0, upper=1> choseBal[NROWS]; 
    int<lower=0, upper=1> choseRight[NROWS]; 

}

transformed data {
}

parameters {
  
  real useWeightGrp_icpt;
  real<lower=0>  weightVar_icpt;
  
  real useWeightGrp_lowmem;
  real<lower=0>  weightVar_lowmem;

  
  real useWeightGrp_highmem;
  real<lower=0> weightVar_highmem;
  
  
  real<lower=0,upper=1> randomResponseGrp;
  real<lower=0> randomScale;
  
  real useWeightSubj_icpt[NSUBJ];
  real useWeightSubj_lowmem[NSUBJ];
  real useWeightSubj_highmem[NSUBJ];

  real<lower=0,upper=1> randomResponseSubj[NSUBJ];
    
}


    
transformed parameters {
  
}
    
model { 
  //group-level weight-use parameters
  useWeightGrp_icpt ~ normal(0,3);
  weightVar_icpt ~ exponential(1);
  
  useWeightGrp_lowmem ~ normal(0,3);
  weightVar_lowmem ~ exponential(1);

  useWeightGrp_highmem ~ normal(0,3);
  weightVar_highmem ~ exponential(1);
  
  //subject-level weight-use parameters
  useWeightSubj_icpt ~ normal(useWeightGrp_icpt, weightVar_icpt);
  useWeightSubj_lowmem ~ normal(useWeightGrp_lowmem, weightVar_lowmem);
  useWeightSubj_highmem ~ normal(useWeightGrp_highmem, weightVar_highmem);

  
  //group-level noise/ random response parameters
  randomResponseGrp ~ beta(1,20);
  randomScale ~ exponential(0.1);

  randomResponseSubj ~ beta(randomScale*randomResponseGrp, randomScale * (1-randomResponseGrp));
  
  for (r in 1:NROWS) {
      int s = subject[r];
      int i = item[r];
      int low_mem = lowMemLoad[r];
      int high_mem = highMemLoad[r];

      int wl = weightSideLeft[r];
      int resp_l = choseLeft[r];
      int resp_b = choseBal[r];
      int resp_r = choseRight[r];

      real pUseWeight = inv_logit(useWeightSubj_icpt[s] + useWeightSubj_lowmem[s] *low_mem + useWeightSubj_highmem[s] *high_mem);
      real pRand = randomResponseSubj[s];
      
      real pChooseLeft = (1-pRand) * (pUseWeight * wl + (1-pUseWeight) * modLeft[r]) + pRand/3;
      real pChooseBal =  (1-pRand) * (1-pUseWeight) * modBal[r] + pRand/3;
      real pChooseRight = (1-pRand) * (pUseWeight * (1-wl) + (1-pUseWeight) * modRight[r]) + pRand/3;
      

      target += log(pChooseLeft * resp_l + pChooseBal * resp_b + pChooseRight * resp_r);
      
      
    }

}

generated quantities {
}
