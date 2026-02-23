---
author:
- Shannon Collins, Elinor Lichtenberg, Avery Pearson, Robert Whyle
authors:
- Shannon Collins, Elinor Lichtenberg, Avery Pearson, Robert Whyle
editor: visual
title: TPWD Species Checklist Workflow_readMe
toc-title: Table of contents
---

## Invert_occurrence_records.R

The .R document provides a workflow for compiling and cleaning species occurrence records for territories at various scales. Updates to the database along with the associated flag file are saved after each major step in the workflow. Databases, flag files, reports, and intermediate files are all saved in the below folder structure.

## Point of contact:

Elinor M. Lichtenberg, [emlichtenberg\@gmail.com](mailto:emlichtenberg@gmail.com){.email}

## Data setup (Folders = **Bold**; Environmental variables = *italicized*)

### User-specified folder (*RootPath)*

This folder is specified by the user and serves as the "root path" for Lichtenberg_invert_occurrence_records.R. In addition to the folders and data provided through the BeeBDC package, users should store there workflow script and any additional datasets they intend to include in occurrence database. After specifying this folder as the root path, BeeBDC::dirMaker() creates the folder structure below and provides environmental values in R that can be used to call particular folders. They are as follows:

#### Data_aquisition_workflow (*DataPath*)

This folder is created by the BeeBDC::dirMaker() function and contains the below folders, which are listed as environmental variables after running the dirMaker() function. It contains all data that is output from manipulating or cleaning occurrence records using the BeeBDC package. This folder and all sub-folders therein will populate as you proceed through the workflow.

"/Data_acquisition_workflow"

*DiscLifePath*

Relative path: "/BDC_repo/DiscoverLife_Data"

*OutPath*

"/Data_acquisition_workflow/Output"

*OutPath_Check*

"/Data_acquisition_workflow/Output/Check"

*OutPath_Figures*

"/Data_acquisition_workflow/Output/Figures"

*OutPath_Intermediate*

"/Data_acquisition_workflow/Output/Intermediate"

*OutPath_Report*

"/Data_acquisition_workflow/Output/Report"

*ScriptPath*

"/Data_acquisition_workflow/Output/Report"

#### Scripts

This folder should be created by the user and should contain the primary script used for cleaning species occurrence records (invert_occurrence_records.R). All other scripts are used as reference for the primary script.

#### BDC_repo

This folder contains repositories that need to be stored locally before starting the workflow

#### Additional Datasets

This folder contains the input data that will be combined to create the master occurrence database. If running through the BeeBDC vignette, input data for bee occurrence records can be downloaded from the supplementary material of Dorey (2023)

## References

This workflow was adapted from Dorey (2023).

Dorey, J. B., Fischer, E. E., Chesshire, P. R., Nava-Bolaños, A., O'Reilly, R. L., Bossert, S., Collins, S. M., Lichtenberg, E. M., Tucker, E. M., Smith-Pardo, A., Falcon-Brindis, A., Guevara, D. A., Ribeiro, B., de Pedro, D., Pickering, J., Hung, K. L. J., Parys, K. A., McCabe, L. M., Rogan, M. S., ... Cobb, N. S. (2023). A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data 2023 10:1, 10(1), 1--17. <https://doi.org/10.1038/s41597-023-02626-w>
