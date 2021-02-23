#################################################################################################
# Exhaustive Partitions                                                                           #
# Copyright (C) 2021                                                                             #
#                                                                                                #
# This code is free software: you can redistribute it and/or modify it under the terms of the    #
# GNU General Public License as published by the Free Software Foundation, either version 3 of   #  
# the License, or (at your option) any later version. This code is distributed in the hope       #
# that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for    #
# more details.                                                                                  #     
#                                                                                                #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
#                                                                                                #
##################################################################################################

##################################################################################################
# Script 5 - Run                                                                                 #
##################################################################################################

##################################################################################################
# Java Options Configuration                                                                     #
##################################################################################################
options(java.parameters = "-Xmx16g")



##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/ExhaustivePartitions", sep="")
  setwd(FolderRoot)
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/ExhaustivePartitions", sep="")
  setwd(FolderRoot)
}
setwd(FolderRoot)
FolderScripts = paste(FolderRoot, "/R/", sep="")


##################################################################################################
# LOAD INTERNAL LIBRARIES                                                                        #
##################################################################################################
FolderScripts = paste(FolderRoot, "/R/", sep="")
setwd(FolderScripts)

cat("\nLoad Sources")

cat("\n\tlibraries.R")
setwd(FolderScripts)
source("libraries.R")

cat("\n\tutils.R")
setwd(FolderScripts)
source("utils.R")

cat("\n\tbuildAndTestPartitions.R")
setwd(FolderScripts)
source("buildAndTestPartitions.R")

cat("\n\tevaluation.R")
setwd(FolderScripts)
source("evaluation.R")



##################################################################################################
# Opens the file "datasets.csv"                                                                  #
##################################################################################################
#cat("\nOpen Dataset Infomation File\n")
diretorios = directories()
setwd(FolderRoot)
datasets = data.frame(read.csv("datasets.csv"))
n = nrow(datasets)


##################################################################################################
# n_dataset: number of the dataset in the "datasets.csv"                                         #
# number_cores: number of cores to paralell                                                      #
# number_folds: number of folds for cross validation                                             # 
##################################################################################################
exhaustivePartitions <- function(number_dataset, number_cores, number_folds, id_part){
  
  if(number_cores == 0){
    cat("\nZero is a disallowed value for number_cores. Please choose a value greater than or equal to 1.")
  } else {
    cl <- parallel::makeCluster(number_cores)
    doParallel::registerDoParallel(cl)
    print(cl)
    
    if(number_cores==1){
      cat("\n\n################################################################################################")
      cat("\n# Running Sequentially!                                                                          #")
      cat("\n##################################################################################################\n\n") 
    } else {
      cat("\n\n################################################################################################")
      cat("\n# Running in parallel with ", number_cores, " cores!                                             #")
      cat("\n##################################################################################################\n\n") 
    }
  }
  cl = cl
  
  retorno = list()
  
  cat("\n\n################################################################################################")
  cat("\n# RUN: Get dataset information: ", number_dataset, "                                             #")
  ds = datasets[number_dataset,]
  names(ds)[1] = "Id"
  dataset_name = toString(ds$Name)
  cat("\n# Dataset: ", dataset_name, "                                                                     #")   
  
  cat("\n# RUN: Generate folders                                                                           #")
  timeFolders = system.time(folders <- directoriesDataset(dataset_name)) 
  
  # C:\Users\elain\ExhaustivePartitions\datasets\folds\emotions\NamesLabels
  folder = paste(diretorios$folderFolds, "/", dataset_name, "/NamesLabels", sep="")
  setwd(folder)
  arquivo = paste(dataset_name, "-NamesLabels.csv", sep="")
  namesLabels = data.frame(read.csv(arquivo))
  namesLabels = c(namesLabels$x)
  
  info <- infoPartitions(id_part, dataset_name, folders$folderExhaustive)
  
  cat("\n# RUN: Build and Test Bell Partitions                                                            #")
  timeComPart = system.time(resPart <- partition(id_part, ds, dataset_name, number_folds, 
              namesLabels, folders$folderExhaustive, folders$folderCVTR,folders$folderCVTS)) 
  
  #setwd(folders$folderExhaustive)
  #name2 = paste(dataset_name, "-partition.RData", sep="")
  #save(resPart, file = name2)
  
  cat("\n# RUN: Matrix Correlation                                                                        #")
  timeGather = system.time(resGather <- gather(id_part, ds, dataset_name, number_folds, 
                                                   namesLabels, folders$folderExhaustive)) 
  
  #setwd(folders$folderExhaustive)
  #name4 = paste(dataset_name, "-gather.RData", sep="")
  #save(resGather, file = name4)
  
  cat("\n# Run: Evaluation Fold                                                                            #")
  timeEval = system.time(resEval <- eval(id_part, ds, dataset_name, number_folds, 
                                               namesLabels, folders$folderExhaustive)) 
  
  #setwd(folders$folderExhaustive)
  #name6 = paste(dataset_name, "-eval.RData", sep="")
  #save(resEval, file = name6)
  
  cat("\n# Run: Gather Evaluation                                                                          #")
  timeGE = system.time(resGE <- gatherEvaluation(id_part, ds, dataset_name, number_folds, 
                                    namesLabels, folders$folderExhaustive)) 
  
  #setwd(folders$folderExhaustive)
  #name8 = paste(dataset_name, "-eval.RData", sep="")
  #save(resGather, file = name6)
  
  cat("\n# Run: Save Runtime                                                                               #")
  Runtime = rbind(timeComPart, timeGather, timeEval, timeGE)
  Folder <- paste(diretorios$folderResults, "/", dataset_name, "/Exhaustive/Partition-", id_part, sep="")
  setwd(Folder)
  name = paste("Partition-", info$numberOfPartition ,"-Runtime-run.csv", sep="")
  write.csv(Runtime, name)
  
  cat("\n# Run: Stop Parallel                                                                              #")
  parallel::stopCluster(cl) 
  
  gc()
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################