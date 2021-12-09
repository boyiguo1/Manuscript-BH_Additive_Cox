make_null_res <- function(fam){
  if(fam == "gaussian")
    data.frame(deviance = NA, R2 = NA, mse = NA, mae = NA)
  else if(fam == "binomial")
    data.frame(deviance = NA, auc = NA, mse = NA, mae = NA, misclassification = NA)
  else if(fam == "poisson")
    data.frame(deviance = NA, mse = NA, mae = NA)
  else if(fam == "cox")
    data.frame(deviance = NA, Cindex = NA)
}
