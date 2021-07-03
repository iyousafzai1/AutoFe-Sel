# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

  devtools::load_all()
  datafile = "C:/Users/Irfan khan/OneDrive/PHD/paper final experiments/MfeatExtractor-master/data/datasets"
 

  data = RWeka::read.arff(file = paste0("data/datasets/", datafile, ".arff"))
  pre.data = preProcessing(data = data)

   comp     = getCompFeatures(data = pre.data)
   inft     = getCnetFeatures(data = pre.data)
   stat = getPCAFeatures(data = pre.data)

  runExtraction(datafile = datafile)
  
# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
