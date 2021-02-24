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
# Script 3 - Build and Test Bell Partitions                                                      #
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
diretorios = directories()


##################################################################################################
# FUNCTION PARTITION c                                                                           #
#  Objective                                                                                     #
#     Build and Test the partition                                                               #
#  Parameters                                                                                    #
#     id_part: the id of the partition                                                           #
#     ds: information dataset                                                                    #
#     dataset_name: the name of the dataset                                                      #
#     number_folds: X-Folds cross-validation                                                     #
#     namesLabels: vector with the name of labels this dataset                                   #
#     FolderDS: the folder of the specific dataset                                               #
#     FolderCVTR: train folder                                                                   #
#     FolderCVTS: test folder                                                                    # 
#  Return                                                                                        #
#
#                                                                                                #
##################################################################################################
partition <- function(id_part, ds, dataset_name, number_folds, namesLabels,
                      FolderDS, FolderCVTR, FolderCVTS){

  retorno = list()

  # get information about the bell partitions
  info <- infoPartitions(id_part,dataset_name, FolderDS)
  
  # create specific folder for partition
  FolderPartition = paste(FolderDS, "/Partition-", info$numberOfPartition, sep="")
  if(dir.exists(FolderPartition)==FALSE){
    dir.create(FolderPartition)
  }
  
  # start build partitions
  # from fold 1 to last fold
  f = 1
  buildBellPartitions <- foreach(f = 1:number_folds) %dopar% {
    
    cat("\nFold: ", f)   
    
    ############################################################################################################
    # cat("\nLOAD LIBRARY \n")
    library("stringr")
    library("AggregateR")    
    library("plyr")
    library("dplyr")
    library("mldr")
    library("utiml")
    library("foreign")
    
    ############################################################################################################
    # cat("\nSET WORKSPACE \n")
    sistema = c(Sys.info())
    FolderRoot = ""
    if (sistema[1] == "Linux"){
      FolderRoot = paste("/home/", sistema[7], "/ExhaustivePartitions", sep="")
      setwd(FolderRoot)
    } else {
      FolderRoot = paste("C:/Users/", sistema[7], "/ExhaustivePartitions", sep="")
      setwd(FolderRoot)
    }
    
    ############################################################################################################
    # configuring folders
    FolderUtils = paste(FolderRoot, "/utils", sep="")
    FolderScripts = paste(FolderRoot, "/R/", sep="")
    setwd(FolderScripts)
    source("utils.R")
    diretorios = directories()
    
    ############################################################################################################
    # configuring folders
    FolderSplit = paste(FolderPartition, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){
      dir.create(FolderSplit)
    }
    
    ############################################################################################################
    # convert csv to arff
    converteArff <- function(arg1, arg2, arg3, FolderUtils){  
      str = paste("java -jar ", FolderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
      a = system(str)
      if(as.numeric(a)==0){
        cat("\nConverted\n")
      } else {
        stop()
      }
      cat("\n\n")  
    }
    
    # get information bell partitions
    info = infoPartitions(id_part,dataset_name, FolderDS)
    
    # from group 1 to last group of partition
    g = 1
    while(g<=info$numberGroupsOfPartition){
      
      ####################################################################################
      library("foreign")
      
      
      ####################################################################################
      # configuring names to use
      nome_particao = paste("partition_", info$numberOfPartition, sep="")
      nome_grupo = paste("group_", g, sep="")
      cat("\n\tPartition: ", nome_particao)
      cat("\n\tGroup: ", nome_grupo)
      
      
      ####################################################################################
      # configuring names for train and test files originals
      nomeTR = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
      nomeTS = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
      
      
      ####################################################################################
      # create folder to specific group
      nome_grupo_2 = paste("Group-", g, sep="")
      FolderGroup = paste(FolderSplit , "/", nome_grupo_2, sep="")
      if(dir.exists(FolderGroup)==FALSE){
        dir.create(FolderGroup)  
      } 
      
      
      ####################################################################################
      # get the labels of this group
      specificPartition = info$specificPartition
      specificGroup = specificPartition %>% filter(., specificPartition$group == g)
      
      
      ####################################################################################
      # total label in this group
      totalLabelsThisGr = nrow(specificGroup)
      
      
      ####################################################################################
      cat("\n\t\tTRAIN: Creating File\n")
      setwd(FolderCVTR)
      nomeTr2 = paste(FolderCVTR, "/", nomeTR, sep="")
      arquivoTR = data.frame(read.csv(nomeTr2))
      atributosTR = arquivoTR[ds$AttStart:ds$AttEnd]
      classesTR = select(arquivoTR, specificGroup$labels)
      thisGroupTR = cbind(atributosTR, classesTR)
      nrow(thisGroupTR)
      
      ####################################################################################
      #cat("\n\t\tTRAIN: Save CSV\n")
      nomeCsTr = paste(FolderGroup, "/grupo_Tr_", g, ".csv", sep="")
      nomeArTr = paste(FolderGroup, "/grupo_Tr_", g, ".arff", sep="")
      setwd(FolderGroup)
      write.csv(thisGroupTR, nomeCsTr, row.names = FALSE)
      
      ####################################################################################
      #cat("\n\t\tTargets")
      inicio = ds$LabelStart
      fim = ncol(thisGroupTR)
      ifr = data.frame(inicio, fim)
      setwd(FolderGroup)
      write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)
      
      ####################################################################################
      #cat("\n\t\tTRAIN: Convert CSV to ARFF\n")
      setwd(FolderGroup)
      arg1Tr = nomeCsTr
      arg2Tr = nomeArTr
      arg3Tr = paste(inicio, "-", fim, sep="")
      converteArff(arg1Tr, arg2Tr, arg3Tr, FolderUtils)
      
      ####################################################################################
      # verifying the arff file
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", nomeArTr, sep="")
      b = system(str0)
      if(as.numeric(b)==0){
        cat("\nSed Ok\n")
      } else {
        stop()
      }
      
      ####################################################################################
      cat("\n\t\tTEST: Creating File\n")
      setwd(FolderCVTS)
      nomeTs2 = paste(FolderCVTS, "/", nomeTS, sep="")
      arquivoTS = data.frame(read.csv(nomeTs2))
      atributosTS = arquivoTS[ds$AttStart:ds$AttEnd]
      classesTS = select(arquivoTS, specificGroup$labels)
      thisGroupTS = cbind(atributosTS, classesTS)
      nrow(thisGroupTS)
      
      ####################################################################################
      #cat("\n\t\tTEST: Save CSV\n")
      nomeCsTs = paste(FolderGroup, "/grupo_Ts_", g, ".csv", sep="")
      nomeArTs = paste(FolderGroup, "/grupo_Ts_", g, ".arff", sep="")
      setwd(FolderGroup)
      write.csv(thisGroupTS, nomeCsTs, row.names = FALSE)
      
      
      ####################################################################################
      #cat("\n\t\tTEST: Convert CSV to ARFF\n")
      setwd(FolderGroup)
      arg1Ts = nomeCsTs
      arg2Ts = nomeArTs
      arg3Ts = paste(inicio, "-", fim, sep="")
      converteArff(arg1Ts, arg2Ts, arg3Ts, FolderUtils)
      
      ####################################################################################
      # verifying the arff file
      str1 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", nomeArTs, sep="")
      d = system(str1)
      if(as.numeric(d)==0){
        cat("\nSed Ok\n")
      } else {
        stop()
      }
      
      ####################################################################################
      #cat("\n\t\tCreating .s file for Clus")
      setwd(FolderGroup)
      
      nome_config = paste("grupo_", g, ".s", sep="")
      sink(nome_config, type = "output")
      
      cat("[General]")          
      cat("\nCompatibility = MLJ08")
      
      cat("\n")
      cat("\n[Data]")
      
      nome_arquivo_2 = paste("grupo_Tr_", g, ".arff", sep="")
      cat(paste("\nFile = ", nome_arquivo_2, sep=""))
      
      nome_arquivo_3 = paste("grupo_Ts_", g, ".arff", sep="")
      cat(paste("\nTestSet = ", nome_arquivo_3, sep=""))
      
      cat("\n")
      cat("\n[Attributes]")
      cat("\nReduceMemoryNominalAttrs = yes")
      
      cat("\n")
      cat("\n[Attributes]")
      cat(paste("\nTarget = ", inicio, "-", fim, sep=""))
      cat("\nWeights = 1")
      
      cat("\n")
      cat("\n[Tree]")
      cat("\nHeuristic = VarianceReduction")
      cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
      
      cat("\n")
      cat("\n[Model]")
      cat("\nMinimalWeight = 5.0")
      
      cat("\n")
      cat("\n[Output]")
      cat("\nWritePredictions = {Test}")
      cat("\n")
      sink()
      
      ####################################################################################
      #cat("\n\t\tExecute CLUS")
      nome_config2 = paste(FolderGroup, "/", nome_config, sep="")
      setwd(FolderGroup)      
      str = paste("java -jar ", FolderUtils, "/Clus.jar ", nome_config2, sep="")
      e = system(str)
      if(as.numeric(e)==0){
        cat("\nClus Ok\n")
      } else {
        stop()
      }
      
      ####################################################################################
      #cat("\n\t\tOpen inicioFimRotulos.csv\n")
      targets = data.frame(read.csv("inicioFimRotulos.csv"))
      
      ####################################################################################
      #cat("\n\t\tOpen predictions\n")
      setwd(FolderGroup)
      library("foreign")
      namae2 = paste(FolderGroup, "/grupo_", g, ".test.pred.arff", sep="")
      predicoes = data.frame(read.arff(namae2))
      
      ####################################################################################
      #cat("\n\t\tSplit Y True and Y Predicts\n")
      
      if(targets$inicio == targets$fim){
        
        library("foreign")
        cat("\n\t\t\tOnly one label in this group\n")
        
        ####################################################################################
        #cat("\n\t\tSave Y_True\n")
        setwd(FolderGroup)
        classes = data.frame(predicoes[,1])
        names(classes) = colnames(predicoes)[1]
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        ####################################################################################
        #cat("\n\t\tSave Y_Predict\n")
        rot = paste("Pruned.p.", colnames(predicoes)[1], sep="")
        pred = data.frame(predicoes[,rot])
        names(pred) = colnames(predicoes)[1]
        setwd(FolderGroup)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        
        ####################################################################################
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        
        gc()
        
      } else {
        
        library("foreign")
        
        ####################################################################################
        #cat("\n\t\tMore than one label in this group")
        comeco = 1+(targets$fim - targets$inicio)
        
        ####################################################################################
        #cat("\n\t\tSave Y_true\n")
        classes = data.frame(predicoes[,1:comeco])
        setwd(FolderGroup)
        write.csv(classes, "y_true.csv", row.names = FALSE)
        
        ####################################################################################
        #cat("\n\t\tSave Y_Predict\n")
        rotulos = c(colnames(classes))
        n_r = length(rotulos)
        nomeColuna = c()
        
        # correct the labels
        t = 1
        while(t <= n_r){
          nomeColuna[t] = paste("Pruned.p.", rotulos[t], sep="")
          t = t + 1
          gc()
        }
        
        pred = data.frame(predicoes[nomeColuna])
        names(pred) = rotulos
        setwd(FolderGroup)
        write.csv(pred, "y_predict.csv", row.names = FALSE)
        
        gc()
      } # FIM DO ELSE
      
      # delete files
      nome1 = paste("grupo_Tr_", g, ".arff", sep="")
      nome2 = paste("grupo_Ts_", g, ".arff", sep="")
      nome3 = paste("grupo_Tr_", g, ".csv", sep="")
      nome4 = paste("grupo_Ts_", g, ".csv", sep="")
      nome5 = paste("grupo_", g, ".model", sep="")
      nome6 = paste("grupo_", g, ".s", sep="")
      
      setwd(FolderGroup)
      unlink(nome1, recursive = TRUE)
      unlink(nome2, recursive = TRUE)
      unlink(nome3, recursive = TRUE)
      unlink(nome4, recursive = TRUE)
      unlink(nome5, recursive = TRUE)
      unlink(nome6, recursive = TRUE)
      
      g = g + 1
      
      gc()
    } # fim do grupo
    
    gc()
  }
  
  # return
  retorno$id_part = id_part
  retorno$ds = ds
  retorno$dataset_name = dataset_name
  retorno$number_folds = number_folds
  retorno$namesLabels = namesLabels
  retorno$FolderDS = FolderDS
  retorno$FolderCVTR = FolderCVTR
  retorno$FolderCVTS = FolderCVTS
  retorno$infoPartition = info
  retorno$FolderPartition = FolderPartition
  
  return(retorno)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# Build and Test Partitions: END                                                                 #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
