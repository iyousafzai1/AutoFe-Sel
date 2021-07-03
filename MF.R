# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

getMfeFeatures = function(data) {

  common.summary = c("kurtosis", "max", "mean", "median", "min", "sd", "skewness", "var", "hist")

  cat("   - mfe general features \n")
  general = tryCatch({
    unlist(mfe:::general(formula = as.formula("Class ~ ."), data = data,
      features = "all", summary = common.summary))
  }, error = function(err) {
    cat("    * got some error - returning empty vector ... \n")
    print(err)
    return(numeric(0))
  })

  cat("   - mfe statistical features \n")
  statistical = tryCatch({
    unlist(mfe:::statistical(formula = as.formula("Class ~ ."), data = data,
      features = "all", summary = common.summary))
 }, error = function(err) {
    cat("    * got some error - returning empty vector ... \n")
    print(err)
    return(numeric(0))
  })

 
  cat("   - mfe info theo features \n")
  infotheo = tryCatch({
    unlist(mfe:::infotheo(formula = as.formula("Class ~ ."), data = data,
      features = "all", summary = common.summary))
  }, error = function(err) {
    cat("    * got some error - returning empty vector ... \n")
    print(err)
    return(numeric(0))
  })

  #cat("   - mfe discriminant features \n")
  #discriminant = tryCatch({
    #unlist(mfe:::discriminant(formula = as.formula("Class ~ ."), data = data,
     # features = mfe::ls.discriminant()[-8], summary = common.summary))
      # sdration raises a segmentation fault on server
 #}, error = function(err) {
    #cat("    * got some error - returning empty vector ... \n")
    #print(err)
#    return(numeric(0))
 # })



  # final output
  obj = list(general = general, statistical = statistical, infotheo = infotheo)                                          #discriminant = discriminant, has been removed by me



  return(obj)
}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
