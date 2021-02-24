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
This code is executed in X-fold cross-validation (mandatory!). First, you have to obtain the X-fold cross-validation files using this code: https://github.com/cissagatto/CrossValidationMultiLabel (all the instructions to use the code are in the Github). After that, put the results generated in the *folds* folder in this project. The folder structure generated by the code CrossValidation is used here.

### Step-2
After obtained the X-Fold Cross-Validation and put in the correct folder, you need the Bell Partitions. The code to generated the Bell Partitions is available here https://github.com/cissagatto/BellPartitionsMultiLabel. Please, follow the instructions in GitHub. After generated the bell partitions, put them in the "BellPartitions" folder. The folder structure is maintained.

### Step-3
Confirms if the folder UTILS contains the following files: Clus.jar, R_csv_2_arff.jar and weka.jar. Without these jars the code not runs. 

### Folder Path
Place a copy of this code in _"C:/Users/[username]/ExhaustivePartitions"_ or _"/home/[username]/ExhaustivePartitions"_. Our files are configured to obtain the paths of the folders from the root. You can change this in the code if you want.

### File "datasets.csv"
A file called "datasets.csv" must be in the *datasets* folder. This file is used to read informations about the datasets and they are used in the code. All 74 datasets available in cometa are in this file. If you want to use another dataset, please, add the following information about the dataset in the file:

_Id, Name, Domain, Labels, Instances, Attributes, Inputs, Labelsets, Single, Max freq, Card, Dens, MeanIR, Scumble, TCS, AttStart, AttEnd, LabelStart, LabelEnd_

The _"Id"_ of the dataset is a mandatory parameter (_n_dataset_) in the command line to run all code. The "LabelStart" and "LabelEnd" are used in a lot of internal functions. Please, make sure that these information are available before run the code.

## Software Requirements
This code was develop in R Studio Version 1.3.959 © 2009-2020, PBC, "Middlemist Red" (3a09be39, 2020-05-18) for Windows. The R language version was 4.0.1 (2020-06-06) with x86_64-w64-mingw32 plataform. Please make sure all the dependencies are installed (verify libraries.R). This code does not provide an installation of libraries.

## Hardware Requirements
This code may or may not be executed in parallel, however, it is highly recommended that you run it in parallel. The number of cores can be configured via the command line (_number_cores_). If *number_cores = 1* the code will run sequentially. In our experiments, we used ten cores. For reproducibility, we recommend that you also use ten cores.

Important: we used the CLUS classifier in this experiment. This implies generating all physical ARFF training, validating and testing files for each of the generated hybrid partitions. Our code generates the partitions first in memory and then saves them to the HD. However, to avoid memory problems, immediately after saving to HD, the files are validated (or tested) and then deleted. Even so, make sure you have enough space on your HD and RAM for this procedure.

# Run

```
Rscript exhaustive.R [number_dataset, number_cores, number_folds, id_part]
```

_number_dataset_: dataset number according to file "datasets.csv" in the folder root

_number_cores_: cores to parallell

_number_folds_: number of folds to cross-validation

_id_part_: you will find this information in "/BellPartitions/[dataset]/". The file used here is "[dataset_name]-groupsPerPartitions.csv". Example, the file flags-groupsPerPartitions.csv will appear like

| part, | totalGroups |
| 1,| 1 | 
| 2,| 2 | 
| 3,| 2 | 
| ...,| ... | 
| P,| G | 


Example:

```
Rscript exhaustive.R 24 10 10 3
```

This will compute all possibile parttions for dataset Flags, using 10 cores and 10-folds for cross-validation to process the partition 3.


## Acknowledgment
This study is financed in part by the Coordenação de Aperfeiçoamento de Pessoal de Nível Superior - Brasil (CAPES) - Finance Code 001

## Links

[Post Graduate Program in Computer Science](http://ppgcc.dc.ufscar.br/pt-br)

[Biomal](http://www.biomal.ufscar.br/)

[Computer Department](https://site.dc.ufscar.br/)

[CAPES](https://www.gov.br/capes/pt-br)

[Embarcados](https://www.embarcados.com.br/author/cissa/)

[Linkedin](https://www.linkedin.com/in/elainececiliagatto/)

[Linkedin](https://www.linkedin.com/company/27241216)

[Instagram](https://www.instagram.com/professoracissa/)

[Facebook](https://www.facebook.com/ProfessoraCissa/)

[Twitter](https://twitter.com/professoracissa)

# Thanks



