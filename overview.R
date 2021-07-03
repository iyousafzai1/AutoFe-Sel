# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
setwd("C:/Users/Irfan khan/OneDrive/PHD/paper final experiments/MfeatExtractor-master")
# returns an overview of all datasets existing in the mfeat directory, 
# returns a data frame, and save the csv file

overview = function() {

  data.files = list.files(path = "mfeats/")

  aux = lapply(data.files, function(dataset) {

    inner.files = list.files(path = paste0("mfeats/", dataset), full.names = TRUE)

    comp = any(grepl("ComplexityFeatures", inner.files))
    inft = any(grepl("InfotheoFeatures", inner.files))
    stat  = any(grepl("StatFeatures", inner.files))
 

    ret = c(comp, inft, stat)
    names(ret) = c("comp", "inft", "stat")

    return(ret)
  })

  df = data.frame(do.call("rbind", aux))
  df = cbind(data.files, df)
  print(df)
  write.csv(x = df, file = "overview.csv")

}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

overview()

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
