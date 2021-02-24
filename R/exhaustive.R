cat("\n\n################################################################################################")
cat("\n# START EXECUTE EXHAUSTIVE                                                                       #")
cat("\n##################################################################################################\n\n") 

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
# Script 6 - Execute on Cluster/Server                                                           #
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
setwd(FolderScripts)


##################################################################################################
# ARGS COMMAND LINE                                                                              #
##################################################################################################
cat("\nArgs Command Line\n")
args <- commandArgs(TRUE)
cat(args, sep = "\n")


##################################################################################################
# LOAD MAIN.R                                                                                     #
##################################################################################################
FolderScripts = paste(FolderRoot, "/R/", sep="")
setwd(FolderScripts)
source("run.R") 


##################################################################################################
# GET THE DIRECTORIES                                                                            #
##################################################################################################
diretorios <- directories()


##################################################################################################
# Read the dataset file with the information for each dataset                                    #
##################################################################################################
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets.csv"))
n_dataset = nrow(datasets)


##################################################################################################
# Get the number of dataset                                                                      #
##################################################################################################
number_dataset <- as.numeric(args[1])


##################################################################################################
# Get the number of cores                                                                        #
##################################################################################################
number_cores <- as.numeric(args[2])


##################################################################################################
# Get the number of folds                                                                        #
##################################################################################################
number_folds <- as.numeric(args[3])


##################################################################################################
# Get the number of partition                                                                    #
# IF you make a LOOP, please, comment this line, because the id_part will past from loop         #
##################################################################################################
id_part <- as.numeric(args[4])


##################################################################################################
# Get the dataset name                                                                           # 
##################################################################################################
ds = datasets[number_dataset,]
dataset_name <- toString(ds$Name) 


##################################################################################################
# Get the number of bell partitions                                                              # 
##################################################################################################
Folder_ = paste(FolderRoot, "/BellPartitions/", dataset_name, sep="")
setwd(Folder_)
str_ = paste(dataset_name, "-groupsPerPartitions.csv", sep="")
bell = data.frame(read.csv(str_))
n = nrow(bell)


##################################################################################################
# EXECUTE                                                                                        # 
##################################################################################################
cat("\nPARTITION: ", id_part, "\n")

timeEP = system.time(res <- exhaustivePartitions(number_dataset, number_cores, number_folds, id_part))
cat("\n")

# Pasta para salvar resultados
Folder <- paste(diretorios$folderResults, "/", dataset_name, "/Exhaustive/Partition-",id_part, sep="")
setwd(Folder)

# salva no servidor
str1a <- paste(dataset_name, "-Partition-", id_part, "-RunTimeFinal.rds", sep="")
print(str1a)
cat("\n Save RDATA")
save(res, file = str1a)

str2a = paste(dataset_name, "-Partition-", id_part, "-Results.rds", sep="")
print(str2a)
cat("\n Save RDS")
save(res, file = str2a)
  
cat("\n Compress folders and files")
str3a <- paste("tar -zcvf ", dataset_name, "-Partition-", id_part, "-results.tar.gz " , Folder, sep="")
print(str3a)
system(str3a)

cat("\n Copy to folder Results")
str5a = paste("cp ", Folder, "/", dataset_name, "-Partition-", id_part, "-results.tar.gz " , diretorios$folderResults, sep="")
system(str5a)
  
#cat("\n Copy to google drive")
#origem = paste(diretorios$folderResults, "/", dataset_name, "-Partition-", id_part, "-results.tar.gz", sep="")
#cat("\nFROM: ", origem)  
#comando = paste("rclone copy ", origem, " YOUR_FOLDER_PATH", sep="")
#cat("\n", comando)
#system(comando)

cat("\n Delete Folder")
setwd(Folder)
str3 = paste("rm -r ", Folder)
system(str3)  

#use this only if you update to google drive
#setwd(diretorios$folderResults)
#unlink(paste(dataset_name, "-Partition-", id_part, "-results.tar.gz", sep=""),  recursive = TRUE)

gc()

cat("\n##################################################################################################")
cat("\n# END OF EXHAUSTIVE PARTITIONS. Thanks God!                                                       #") 
cat("\n##################################################################################################")
cat("\n\n\n\n") 


##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
