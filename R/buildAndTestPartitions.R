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
#                                                                               
##################################################################################################
partition <- function(id_part, ds, dataset_name, number_folds, namesLabels,
                      FolderDS, FolderCVTR, FolderCVTS){
  
  #cat("\n FUNCTION PARTITION \n")
  
  retorno = list()
  
  #cat("\n\tTet info partitions:\n")
  info <- infoPartitions(id_part,dataset_name, FolderDS)
  #print(info)
  
  # criando a pasta específica da partição
  FolderPartition = paste(FolderDS, "/Partition-", info$numberOfPartition, sep="")
  #cat("\n\tFolder: ", FolderPartition, "\n")
  
  if(dir.exists(FolderPartition)==FALSE){
    #cat("\ncriando a pasta")
    dir.create(FolderPartition)
  }
  
  # start build partitions
  # do fold 1 até o último fold
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
    FolderUtils = paste(FolderRoot, "/utils", sep="")
    FolderScripts = paste(FolderRoot, "/R/", sep="")
    setwd(FolderScripts)
    source("utils.R")
    diretorios = directories()
    
    ############################################################################################################
    FolderSplit = paste(FolderPartition, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){
      dir.create(FolderSplit)
    }
    
    ############################################################################################################
    converteArff <- function(arg1, arg2, arg3){
      str = paste("java -jar ", FolderUtils, "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
      system(str)
      cat("\n\n")  
    }
    
    info = infoPartitions(id_part,dataset_name, FolderDS)
    
    g = 1
    while(g<=info$numberGroupsOfPartition){
      
      
      ####################################################################################
      library("foreign")
      
      ####################################################################################
      nome_particao = paste("partition_", info$numberOfPartition, sep="")
      nome_grupo = paste("group_", g, sep="")
      cat("\n\tPartition: ", nome_particao)
      cat("\n\tGroup: ", nome_grupo)
      
      
      ####################################################################################
      nomeTR = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
      nomeTS = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
      
      ####################################################################################
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
      # total de rótulos neste grupo
      totalLabelsThisGr = nrow(specificGroup)
      
      ####################################################################################
      cat("\n\t\tTRAIN: Creating File\n")
      setwd(FolderCVTR)
      nomeTr2 = paste(FolderCVTR, "/", nomeTR, sep="")
      arquivoTR = data.frame(read.csv(nomeTr2))
      atributosTR = arquivoTR[ds$AttStart:ds$AttEnd]
      rotulosTR = c(specificGroup$labels)
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
      arg1 = nomeCsTr
      arg2 = nomeArTr
      arg3 = paste(inicio, "-", fim, sep="")
      converteArff(arg1, arg2, arg3)
      
      ####################################################################################
      cat("\n\t\tTRAIN: Verify and correct {0} and {1}\n")
      #arquivo = paste(FolderGroup, "/", nomeArTr, sep="")
      str1 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", nomeArTr, sep="")
      cat("\n", str1,"\n")
      system(str1)
      
      ####################################################################################
      cat("\n\t\tTEST: Creating File\n")
      setwd(FolderCVTS)
      nomeTs2 = paste(FolderCVTS, "/", nomeTS, sep="")
      arquivoTS = data.frame(read.csv(nomeTs2))
      atributosTS = arquivoTS[ds$AttStart:ds$AttEnd]
      rotulosTS = c(specificGroup$labels)
      classesTS = select(arquivoTS, rotulosTS)
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
      arg1 = nomeCsTs
      arg2 = nomeArTs
      arg3 = paste(inicio, "-", fim, sep="")
      converteArff(arg1, arg2, arg3)
      
      ####################################################################################
      #cat("\n\t\tTEST: Verify and correct {0} and {1}\n")
      #arquivo = paste(FolderGroup, "/", nomeArTs, sep="")
      str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", nomeArTs, sep="")
      cat("\n", str0,"\n")
      system(str0)
      
      ####################################################################################
      #cat("\n\t\tCreating .s file for Clus")
      setwd(FolderGroup)
      
      nome_config = paste("grupo_", g, ".s", sep="")
      sink(nome_config, type = "output")
      
      cat("[General]")          
      #cat("\nVerbose = 10")
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
      system(str)
      
      ####################################################################################
      #cat("\n\t\tOpen inicioFimRotulos.csv\n")
      targets = data.frame(read.csv("inicioFimRotulos.csv"))
      
      ####################################################################################
      #cat("\n\t\tOpen predictions\n")
      setwd(FolderGroup)
      library("foreign")
      namae2 = paste(FolderGroup, "/grupo_", g, ".test.pred.arff", sep="")
      
      #if(file.exists(namae2)==TRUE){
      #  print(namae2)
      #  cat("\nexist")
      #} else {
      #  print(namae2)
      #  cat("\ndont exist")
      #}
      
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