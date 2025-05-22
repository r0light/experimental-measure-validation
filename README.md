# An experimental validation of architectural measures for cloud-native quality evaluations

This is the accompanying repository to the paper titled "An experimental validation of architectural measures for cloud-native quality evaluations".
It is intended to make the experiments conducted in the context of the paper repeatable and it contains the data gathered while running the experiments for the paper.
(This repository, however, contains the experiment data only in aggregated form, the raw data is planned to be published on a platform like <https://zenodo.org/>)

## Running experiments

The `setup` folder contained in this repository contains the required code and artifacts for running the experiments. Please read the included `instructions.md` for information on how to run the experiments.

## Evaluating experiments

The `analysis` folder contains an [R](https://www.r-project.org/)[Studio](https://posit.co/products/open-source/rstudio/) project for processing and analyzing the data gathered from running the experiments.
The script `analysis/data-preparation.R` can be applied to the raw data, but needs to be adjusted to your specific experiment run.
The scripts `analysis/data-analysis.R` and `report.Rmd` can then be used for processing the output from the data preparation script. They contain roughly the same code, but  `report.Rmd` is used to create a more easily readable report from the data. The report from this script is also available as the gh-pages site to this repository.

## Experiment data

The folder `results` contain the aggregated results from the experiment runs for the paper. In addition, the architectural measures for the different architectural variations are included together with the architectural models. By importing the architectural models into the [Clounaq](https://clounaq.de) tool, the calculated architectural measures can be reproduced.

Result data containing runtime measures and design time architectural measures that belong together can be matched by matching the file name prefixes.
