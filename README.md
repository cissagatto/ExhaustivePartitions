# ExhaustivePartitions
This code is part of my doctoral research. It's exhaustive experimentation of Bell Partitions.

## Scripts
This source code consists of a R project for R Studio and the following R scripts:

1. libraries
2. utils
3. buildAndTestPartitions
4. evaluation
5. run
6. exhaustive

## Folder Structure

ExhaustivePartitions

--BellPartitions

  ----[dataset_1]

  ----[dataset_2]

  ----[dataset_n]

-- datasets

  ---- folds

    ------ [dataset_1]
        
        ------ CrossValidation    
        
          ------ Tr        
          
          ------ Ts        

        ------ NamesLabels

  ---- originals

-- R

-- results

-- utils

## Important
This code is executed in 10-fold cross-validation (mandatory!). First you have to obtain the 10-fold cross-validation files using this code: https://github.com/cissagatto/CrossValidationMultiLabel (all the instructions to use the code are in the github). After that, put the results generated in the *folds* folder in this project. 

