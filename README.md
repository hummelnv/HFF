# HFF
Data and scripts used in "Statistical analysis of small faults in rotated blocks of crust near the Husavik-Flatey fault, northern Iceland"



The data in this repository are separated into files according to source publication. Measurements of dikes and lavas are from Jancin et al. (1985), Young et al. (1985), and Fjäder et al. (1994), measurements of faults with slickenside striae from Bergerat et al. (2000) and Garcia and Dhont (2005), and paleomagnetic measurements from Horst et al. (2018) and Titus et al. (2018). The file "ourData.csv" contains measurements collected for this study. 

The script “loadStructures2018.R” loads data from the csv files into R so that the data can be processed and plotted in other scripts. The folder “icelandData” in the GitHub project contains data in files with names that can be read into R by “loadStructures2018”. To download and view data, download the geologyGeometry library for R (http://www.joshuadavis.us/software/index.html), place the "icelandScripts" folder into the geologyGeometry folder on your computer, change the paths at the beginning of "loadStructures2018", and run the script. This will load data into R data frames. The data in "icelandData" are redundant with the data in csv files outside of the folder. Note that some of the measurements included in this GitHub repository (e.g. of large faults without slip sense indicators) are not shown in figures in the manuscript. 

R scripts contain code to run regressions and produce plots shown in figures in the manuscript. 

DOI: https://zenodo.org/badge/latestdoi/392162782

