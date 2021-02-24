##################################################################################################
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
# Script 2 - Utils
##################################################################################################


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
# FUNCTION DIRECTORIES                                                                           #
#   Objective:                                                                                   #
#      Creates all the necessary folders for the project. These are the main folders that must   # 
#      be created and used before the script starts to run                                       #  
#   Parameters:                                                                                  #
#      None                                                                                      #
#   Return:                                                                                      #
#      All path directories                                                                      #
##################################################################################################
directories <- function(){
  
  retorno = list()
  
  folderResults = paste(FolderRoot, "/results", sep="")
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dirResults = dir(folderResults)
    n_Results = length(folderResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dirResults = dir(folderResults)
    n_Results = length(folderResults)
  }
  
  folderUtils = paste(FolderRoot, "/utils", sep="")
  if(dir.exists(folderUtils) == TRUE){
    setwd(folderUtils)
    dirUtils = dir(folderUtils)
    n_Utils = length(folderUtils)
  } else {
    dir.create(folderUtils)
    setwd(folderUtils)
    dirUtils = dir(folderUtils)
    n_Utils = length(dirUtils)
  }
  
  folderDatasets = paste(FolderRoot, "/datasets", sep="")
  if(dir.exists(folderDatasets) == TRUE){
    setwd(folderDatasets)
    dirDatasets = dir(folderDatasets)
    n_Datasets = length(dirDatasets)
  } else {
    dir.create(folderDatasets)
    setwd(folderDatasets)
    dirDatasets = dir(folderDatasets)
    n_Datasets = length(dirDatasets)
  }
  
  folderDO = paste(folderDatasets, "/originals", sep="")
  if(dir.exists(folderDO) == TRUE){
    setwd(folderDO)
    dirDO = dir(folderDO)
    n_DO = length(dirDO)
  } else {
    dir.create(folderDO)
    setwd(folderDO)
    dirDO = dir(folderDO)
    n_DO = length(dirDO)
  }
  
  folderFolds = paste(folderDatasets, "/folds", sep="")
  if(dir.exists(folderFolds) == TRUE){
    setwd(folderFolds)
    dirFolds = dir(folderFolds)
    n_Folds = length(dirFolds)
  } else {
    dir.create(folderFolds)
    setwd(folderFolds)
    dirFolds = dir(folderFolds)
    n_Folds = length(dirFolds)
  }
  
  folderBellPart = paste(FolderRoot, "/BellPartitions", sep="")
  if(dir.exists(folderBellPart) == TRUE){
    setwd(folderBellPart)
    dirBellPart = dir(folderBellPart)
    n_BellPart = length(dirBellPart)
  } else {
    dir.create(folderBellPart)
    setwd(folderBellPart)
    dirBellPart = dir(folderBellPart)
    n_BellPart = length(dirBellPart)
  }
  
  # return folders
  retorno$folderResults = folderResults
  retorno$folderUtils = folderUtils
  retorno$folderDatasets = folderDatasets
  retorno$folderDO = folderDO
  retorno$folderFolds = folderFolds
  retorno$folderBellPart = folderBellPart
  
  # return of folder contents
  retorno$dirResults = dirResults
  retorno$dirUtils = dirUtils
  retorno$dirDatasets = dirDatasets
  retorno$dirDO = dirDO
  retorno$dirFolds = dirFolds
  retorno$dirBellPartitions = dirBellPart
  
  # return of the number of objects inside the folder
  retorno$n_Results = n_Results
  retorno$n_Utils = n_Utils
  retorno$n_Datasets = n_Datasets
  retorno$n_DO = n_DO
  retorno$n_Folds = n_Folds
  retorno$n_BellPartitions = n_BellPart
  
  return(retorno)
  gc()
}



##################################################################################################
# FUNCTION CREATING FOLDER PRINCIPALS                                                            #
#   Objective                                                                                    #
#       Creates the specific folders for the specific dataset                                    #
#   Parameters                                                                                   #
#       dataset_name: dataset name. It is used to create the folders.                            #
#   Return:                                                                                      #
#      All path directories                                                                      #
##################################################################################################
directoriesDataset<- function(dataset_name){
  
  diretorios = directories()
  
  retorno = list()
  
  folderFolds = paste(diretorios$folderFolds, "/", dataset_name, sep="")
  if(dir.exists(folderFolds) == TRUE){
    setwd(folderFolds)
    dir_Folds = dir(folderFolds)
    n_Folds = length(dir_Folds)
  } else {
    dir.create(folderFolds)
    setwd(folderFolds)
    dir_Folds = dir(folderFolds)
    n_Folds = length(dir_Folds)
  }
  
  folderDataset = paste(diretorios$folderResults, "/", dataset_name, sep="")
  if(dir.exists(folderDataset) == TRUE){
    setwd(folderDataset)
    dir_Dataset = dir(folderDataset)
    n_Dataset = length(dir_Dataset)
  } else {
    dir.create(folderDataset)
    setwd(folderDataset)
    dir_Dataset = dir(folderDataset)
    n_Dataset = length(dir_Dataset)
  }
  
  #folderReports = paste(folderDataset, "/Reports", sep="")
  #if(dir.exists(folderReports) == TRUE){
  #  setwd(folderReports)
  #  dir_Reports = dir(folderReports)
  #  n_Reports = length(dir_Reports)
  #} else {
  #  dir.create(folderReports)
  #  setwd(folderReports)
  #  dir_Reports = dir(folderReports)
  #  n_Reports = length(dir_Reports)
  #}
  
  folderCV = paste(folderFolds, "/CrossValidation/", sep="")
  if(dir.exists(folderCV) == TRUE){
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  } else {
    dir.create(folderCV)
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  }
  
  folderCVTR = paste(folderFolds, "/CrossValidation/Tr", sep="")
  if(dir.exists(folderCVTR) == TRUE){
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  } else {
    dir.create(folderCVTR)
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  }
  
  folderCVTS = paste(folderFolds, "/CrossValidation/Ts", sep="")
  if(dir.exists(folderCVTS) == TRUE){
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  } else {
    dir.create(folderCVTS)
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  }
  
  folderExhaustive = paste(folderDataset, "/Exhaustive", sep="")
  if(dir.exists(folderExhaustive) == TRUE){
    setwd(folderExhaustive)
    dir_Exhaustive = dir(folderExhaustive)
    n_Exhaustive = length(dir_Exhaustive)
  } else {
    dir.create(folderExhaustive)
    setwd(folderExhaustive)
    dir_Exhaustive = dir(folderExhaustive)
    n_Exhaustive = length(dir_Exhaustive)
  }
  
  retorno$folderFolds = folderFolds
  retorno$folderDataset = folderDataset
  #retorno$folderReports = folderReports
  retorno$folderExhaustive = folderExhaustive
  retorno$folderCV = folderCV
  retorno$folderCVTR = folderCVTR
  retorno$folderCVTS = folderCVTS
  
  retorno$dir_Folds = dir_Folds
  retorno$dir_Dataset = dir_Dataset
  #retorno$dir_Reports = dir_Reports
  retorno$dir_Exhaustive = dir_Exhaustive
  retorno$dir_folderCV = dir_folderCV
  retorno$dir_folderCVTR = dir_folderCVTR
  retorno$dir_folderCVTS = dir_folderCVTS
  
  retorno$n_Folds = n_Folds
  retorno$n_Dataset = n_Dataset
  #retorno$n_Reports = n_Reports
  retorno$n_Exhaustive = n_Exhaustive
  retorno$n_folderCV = n_folderCV
  retorno$n_folderCVTS = n_folderCVTR
  retorno$n_folderCVTS = n_folderCVTS
  
  return(retorno)
  
}


##################################################################################################
# FUNCTION CONVERT TO ARFF                                                                       #
#     Objective:                                                                                 #
#        Convert csv file correctly to arff file                                                 #
#     Parameters                                                                                 #
#        arg 1: existing csv file name                                                           #
#        arg 2: name of the arff file to be created                                              #
#        arg 3: specific number of labels that are part of the file. Example: starts at label    # 
#        30 and ends at label 50.                                                                #
#     Return:                                                                                    #
#        The arff file in the specific folder                                                    #
##################################################################################################
converteArff <- function(arg1, arg2, arg3, FolderUtils){  
  str = paste("java -jar ", FolderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
  print(system(str))
  cat("\n\n")  
}



##################################################################################################
# FUNCTION INFO DATA SET                                                                         #
#  Objective                                                                                     #
#     Gets the information that is in the "datasets.csv" file.                                    #  
#  Parameters                                                                                    #
#     dataset: the specific dataset                                                              #
#  Return                                                                                        #
#     Everything in the spreadsheet                                                              #
##################################################################################################
infoDataSet <- function(dataset){
  retorno = list()
  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$domain = dataset$Domain
  retorno$instances = dataset$Instances
  retorno$attributes = dataset$Attributes
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$MeanIR
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabelStart
  retorno$labEnd = dataset$LabelEnd
  return(retorno)
  gc()
}

infoPartitions <- function(id_part,dataset_name,FolderDS){
  
  retorno = list()
  
  # setando o diretório específico
  diretorios = directories()
  FolderBP = paste(diretorios$folderBellPart, "/", dataset_name, sep="")
  
  # obtendo a lista de partições
  setwd(FolderBP)
  nome = paste(dataset_name, "-partitions.csv", sep="")
  bell = data.frame(read.csv(nome))
  
  # obtendo o total de grupos por cada partição
  nome2 = paste(dataset_name, "-groupsPerPartitions.csv", sep="")
  groupsPerPartitions = data.frame(read.csv(nome2))
  
  # obtendo o formato da partição
  specificPartition = bell %>% filter(., bell$part == id_part)
  
  # obtendo o total de grupos da partição
  groupSecificPartition = groupsPerPartitions %>% filter(., groupsPerPartitions$part == id_part)
  
  # obtendo o número da partição
  numeroDaParticao = groupSecificPartition$part
  
  # obtendo o número de grupos da partição
  numeroDeGruposDaParticao = groupSecificPartition$totalGroups
  
  # criando a pasta específica da partição
  FolderPartition = paste(FolderDS, "/Partition-", numeroDaParticao, sep="")
  
  retorno$FolderBP = FolderBP
  retorno$bell = bell
  retorno$groupsPerPartitions = groupsPerPartitions 
  retorno$specificPartition = specificPartition
  retorno$specificPartition = specificPartition
  retorno$numberOfPartition = numeroDaParticao
  retorno$numberGroupsOfPartition = numeroDeGruposDaParticao
  retorno$FolderPartition = FolderPartition
  
  return(retorno)
  
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
