---
author:
- Shannon Collins, Elinor Lichtenberg, Avery Pearson, Mitchell Webb, Robert Whyle
authors:
- Shannon Collins, Elinor Lichtenberg, Avery Pearson, Mitchell Webb, Robert Whyle
editor: visual
title: Inverts occurrence records readMe
toc-title: Table of contents
---

## Invert_occurrence_records.R

The .R document provides a workflow for compiling and cleaning species occurrence records for territories at various scales. Updates to the database along with the associated flag file are saved after each major step in the workflow. Databases, flag files, reports, and intermediate files are all saved in the below folder structure.

## Point of contact:

Elinor M. Lichtenberg, [emlichtenberg\@gmail.com](mailto:emlichtenberg@gmail.com){.email}</br>

## Data setup (Folders = **Bold**; Environmental variables = *italicized*):

#### **User-specified folder** (*RootPath*)

-   This folder is specified by the user and serves as the "root path" for Invert_occurrence_records.R. It should house the workflow script, Data and Scripts directories, and all other folders created/provided throughout the workflow. After specifying this folder as the root path, our workflow, in addition to BeeBDC::dirMaker(), creates the folder structure below and provides environmental values in R that can be used to call particular folders. They are as follows:

#### **/Data** (*DataPath)*

-   This folder is used by the BeeBDC::dirMaker() function and contains the below folders, which are listed as environmental variables after running through the Setup portion of this workflow. It contains all data that is output from manipulating or cleaning occurrence records using the BeeBDC package. This folder and all sub-folders therein will populate as you proceed through the workflow.

#### **/Data/User_data** (*DiscLifePath)*

-   This path, while not used directly in this script, defaults to the User_data directory

#### **/Data/Output** (*OutPath)*

#### **/Data/Output/Check** (*OutPath_Check)*

#### **/Data/Output/Figures** (*OutPath_Figures)*

#### **/Data/Output/Intermediate** (*OutPath_Intermediate)*

#### **/Data/Output/Report** (*OutPath_Report)*

#### **/Data/User_data** (*UserDataPath*)

-   This folder serves to house all local user files, including custom datasets, downloaded datasets, shapefiles, taxonomy files, etc.

#### **/Scripts** (*ScriptPath*)

-   This folder is found directly within the project root and contains additional scripts used by this workflow.

## References

This workflow was adapted from Dorey (2023).

Dorey, J. B., Fischer, E. E., Chesshire, P. R., Nava-Bolaños, A., O'Reilly, R. L., Bossert, S., Collins, S. M., Lichtenberg, E. M., Tucker, E. M., Smith-Pardo, A., Falcon-Brindis, A., Guevara, D. A., Ribeiro, B., de Pedro, D., Pickering, J., Hung, K. L. J., Parys, K. A., McCabe, L. M., Rogan, M. S., ... Cobb, N. S. (2023). A globally synthesised and flagged bee occurrence dataset and cleaning workflow. Scientific Data 2023 10:1, 10(1), 1--17. <https://doi.org/10.1038/s41597-023-02626-w>
