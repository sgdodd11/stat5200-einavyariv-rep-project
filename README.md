# Replication of Einav & Yariv (2006)

## Package
To execute the full project, run the shell script `master.sh`. If the project is downloaded locally, run the following in a terminal after replacing `/path to/` with the location of the folder on your device:

```
bash "/path to/Einav Yariv replication/master.sh"
```

Executing the shell script runs `dta_to_csv.R`, `replication.R` and `replication_extra.R` in order, and saves all figures and tables to `output/`. It requires R with the `data.table`, `ggplot2` and `foreign` packages.


There are two relevant code files to replicate the exhibits; one to replicate the main results (`replication.R`) and one to replicate the additional Table 5 result (`replication_extra.R`). To execute these files individually, the working directory should be set to the `code` folder of this replication project; all paths inside the files are relative to the `code` folder. 

To execute specific files, the order of execution is: 
1. Run `dta_to_csv.R`, to convert the downloaded `.dta` files to `.csv` files. 
2. Run `replication.R`
3. Run `replication_extra.R`. 

The original data download is from https://www.openicpsr.org/openicpsr/project/125621/version/V1/view, and the files are stored in `data/125621-V1`. Execute `dta_to_csv.R`, with working directory set to `code`, to convert these files from `.dta` to `.csv` files. The output `.csv` files are then stored in the `raw` folder. 

Output automatically copies files to a local folder that is synced with Overleaf; this needs a device- and account-specific path. To produce output only in the output folder of this project, comment out the overleaf path specification and lines where tables and figures are stored to the overleaf directory. Also, this was on macOS which required "quartz" for me to output the figures as pdfs; I am not sure if this will produce problems on a Windows device. The file may also break without installing the Overleaf font LM Roman 10, as I used this to make the figures consistent with the outputs I'll have in the final project. As I said above, those lines can just be commented out if they are causing problems (comment out all lines with Overleaf exports, delete font-specifying lines and allow R to implement the default.)


## Replicated Exhibits: Figure 1 & Table 1; Figure 4 & Table 4
These figures all focus on tenure status as the outcome. Exhibits 1 use the data from economics; Exhibits 4 use the data from psychology. I am replicating these figures as establishing the lack of a significant relationship between last names and collaboration patterns will eventually be used as descriptive evidence of a difference resulting from the credit attributability in fields. The goal will be to assess whether this differs by gender across fields, and this work will set the foundation for future work I hope to do that extends to longer term collaboration patterns and outcomes. In this replication, I am using their outcomes. In the final project, collaboration outcomes will be focal. Because of this, I have also replicated Table 5 from the paper, which begins to look at collaboration patterns. 

## Notes on data and specifications in the paper. 
The replication package for this paper includes the data but not replication files with code; here I describe the specifications as they are described in the paper. I should get very close to their results; their dataset is cleaned and has few columns.

Indeed, I have been able to pretty much exactly replicate Figures 1 & 4 and Tables 1 & 4. I have had some trouble with Table 5, which I will describe below. 

### Data
 The data cover the top 35 economics and psychology departments in the US and include:
- names (surname order is numerical A:Z --> 1:26)
- tenure status (at 3 tiers: untenured, tenured and emeritus)
- nationality, race, religion (to use as controls related to surname initial prevalence)
- whether they are fellows of the Econometric Society
- year of PhD completion (so time in field can be measured using 2006 as the year of observation)

### Outcomes
Tenure status is the outcome of focus for the two main figures and two main tables I will replicate. The additional Table 5 looks at average position in the alphabet of coauthors on papers for different numbers of coauthors and for different credit attribution conventions. 

## Figure 1 
Figure 1 plots the cumulative distribution of last name initial (A:Z --> 1:26) separately for tenured and untenured economists. If there is no alphabetical bias in tenure, the two CDFs should track each other closely; if earlier initials are more likely to receive tenure, the tenured CDF should sit above the untenured CDF. I make one figure for each rank set of departments (top five, top ten, top 20 and top 35), mirroring the samples used in Table 1. People with missing tenure status are dropped, and each line is extended flat at one out to Z so the two CDFs can be compared across the full alphabet.


## Table 1
Table 1 contains basic linear probability regressions of binary tenure outcomes on last name alphabetical index. Regressions are run restricting to different rank sets of departments (top five, top ten, top 20 and top 35—there); each set includes two regressions, the first of which includes no controls, and the second of which includes all controls mentioned above: (American) nationality, origin of the name (Jewish, Indian and other Asian). The authors report "experimenting" with additional control variables, including department fixed effects and cumulative number of publications, as well as with a sample restriction over year of receiving PhD. 


## Figure 4 
Figure 4 is the psychology analog of Figure 1: the cumulative distribution of last name initial for tenured and untenured psychologists, again for each rank set of departments (top five, top ten, top 20 and top 35). It is built exactly as Figure 1 is, using the psychology data instead of the economics data. This is the comparison of interest: if credit is attributed differently across fields, the alphabetical pattern in economics (where authors are listed alphabetically) should not show up in psychology (where author order is not alphabetical).

## Table 4
Table 4 is the psychology analog of Table 1: basic linear probability regressions of binary tenure outcomes on last name alphabetical index, run on the same rank sets of departments (top five, top ten, top 20 and top 35), with one regression with no controls and one with controls for each set. The only difference from Table 1 is that there is no American nationality control, as nationality is blank for all psychologists; the controls are the origin of the name (Jewish, Indian and other Asian). As in Table 1, emeritus faculty are excluded from the regressions. 


## Table 5
Table 5 is the additional exhibit that begins to look at collaboration patterns, and it uses the publications data (`Surnames_pubs_data.csv`) rather than the faculty data; the unit of observation is a paper. For papers grouped by number of authors (one to five, then all multiauthored papers), the table reports the number and share of papers by alphabetization practice, the mean position in the alphabet of last name initials (A:Z --> 1:26) averaged across all authors on a paper (so, a higher mean, e.g. 17, indicates most of the authors on the paper have surnames with initials that are late in the alphabet), and the standard deviation of this paper-level mean. Multiauthored papers are also split into those listing authors alphabetically and those that do not. If alphabetical ordering is the convention for credit attribution, multiauthored papers should have lower mean initials than single-authored papers (since authors with early initials benefit from alphabetical ordering, while those with late initials get buried under the et al and may receive less credit for the collaboration), and non-alphabetical papers should have higher mean initials than alphabetical papers with the same number of authors. Following the paper, I mark means that are significantly lower than for single-authored papers with ** and non-alphabetical means that are significantly higher than for alphabetical papers with the same number of authors with ^^; both are at the 5 percent level. There are no ** cells in my replication; this differs from the table in the paper.

I have not been able to match the paper's Table 5 exactly. As mentioned above, some of the results have not reappeared. I believe this is some result of the alphabetization process. As I was reconstructing the table, there were decisions to be made about how to alphabetize (e.g., La Porta, on L or P). The earliest rows matched, but later rows did not. I tried a few ordering schemes (meaning some ordered on L, some ordered on P, and I did this for the different types of names that produced this type of conflict), but all of these made the early rows mismatched without making the later rows much better. 