# ExhaustivePartitions
This code is part of my doctoral research. It's exhaustive experimentation of Bell Partitions.

## Scripts
This source code consists of an R project for R Studio and the following R scripts:

1. libraries
2. utils
3. buildAndTestPartitions
4. evaluation
5. run
6. exhaustive

## Folder Structure
<img src="https://github.com/cissagatto/ExhaustivePartitions/blob/main/pastas.png" width="50">

## Preparing your experiment

### Step-1
This code is executed in 10-fold cross-validation (mandatory!). First, you have to obtain the 10-fold cross-validation files using this code: https://github.com/cissagatto/CrossValidationMultiLabel (all the instructions to use the code are in the Github). After that, put the results generated in the *folds* folder in this project. The folder structure generated by the code CrossValidation is used here.

### Step-2
After obtained the 10-Fold Cross-Validation and put in the correct folder, you need the Bell Partitions. The code to generated the Bell Partitions is available here https://github.com/cissagatto/BellPartitionsMultiLabel. Please, follow the instructions in GitHub. After generated the bell partitions, put them in the "BellPartitions" folder. The folder structure is maintained.



