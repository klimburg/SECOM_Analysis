# Author: Kevin C Limburg
# Description: Script for downloading the SECOM dataset from the UCI machine
#              learning repository, https://archive.ics.uci.edu/ml/datasets/SECOM

# check os type and set download method accordingly
os<-Sys.info()["sysname"]
if(os == "Windows"){
  dl.method <- "auto"
} else {
  dl.method = "curl"
}
  

# check if data directory exists if not create it
if(!file.exists("./data")) dir.create("./data")

# check if files already exist, if not download
if(!file.exists("./data/secom.data")){
    download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.data",
                  "./data/secom.data",
                  method = dl.method)
}

if(!file.exists("./data/secom.names")){
    download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom.names",
                  "./data/secom.names",
                  method = dl.method)
}

if(!file.exists("./data/secom_labels.data")){
    download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/secom/secom_labels.data",
                  "./data/secom_labels.data",
                  method = dl.method)
}