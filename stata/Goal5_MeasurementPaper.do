/*TITLE:        Goal 5 - Measurement Paper Analyses_htkr_t1t2t3t4_c2
  INVESTIGATOR: Megan McClelland (Lead)
  SAMPLE:       HTKR Grant (IES Goal 5)- Cohort 2: t1, t2, t3, t4
  PURPOSE:      To run basic descriptives and analyses of the RLPL Paper
  Edited Date:  5/12/22
  Edited by:    Christopher R Gonzales
*/

********************************************************************************
*       Analyses for Goal 5 - HTKS-R Measurement Paper                         *
********************************************************************************


****extra stata modules****

*note: only uncomment if these needed to be installed on the current workstation

/*
*fre - frequency tables
ssc install fre

*pcorrmatt - pairwise correlations controlling for multiple variables
ssc install pcorrmat
*/

/*
*stata2mplus - generate mplus input and data file with variable descriptions
ssc install stata2mplus
*/

****Data file location****

*full merged data set
clear
cd "..\data\merged"
use alldata_t1t2t3t4_c2.dta

********************************************************************************
*       (1) Prep Datafiles for M-plus Item Level EFA Analysis                  *
********************************************************************************

/*
note: Appened individual datafiles and export to .csv to create htkr-longformat 
data for overall analyses in m-plus, and great individual datafiles for each 
time point.
*/
 
****Cleaning individual time point data files for long format***

***time 1***

*datafile location*
cd "..\data\t1\coh2\"
clear
use alldata_t1c2.dta

*drop unused variables
keep id htko1_1 - htkr30_1
drop htkra0_1 htkrc0_1 htkre0_1

*rename variables to remove post-script
renvars htk*, postdrop(2)
sort id
gen wave =1
gen coh =2
save htkr_t1c2, replace

***time 2***

*datafile location*
cd "..\data\t2\coh2\"
clear
use alldata_t2c2.dta

*drop unused variables
keep id htko1_2 - htkr30_2
*drop prompt questions
drop htkra0_2 htkrc0_2 htkre0_2

*rename variables to remove post-script
renvars htk*, postdrop(2)
sort id
gen wave =2
gen coh =2
save htkr_t2c2, replace

***time 3***

*datafile location*
cd "..\data\t3\coh2\"
clear
use alldata_t3c2.dta

*drop unused variables
keep id htko1_3 - htkr30_3
*drop prompt questions
drop htkra0_3 htkrc0_3 htkre0_3

*rename variables to remove post-script
renvars htk*, postdrop(2)
sort id
gen wave =3
gen coh =2
save htkr_t3c2, replace

***time 4***

*datafile location*
cd "..\data\t4\coh2\"
clear
use alldata_t4c2.dta

*drop unused variables
keep id htko1_4 - htkr30_4
*drop prompt questions
drop htkra0_4 htkrc0_4 htkre0_4

*rename variables to remove post-script
renvars htk*, postdrop(2)
sort id
gen wave =4
gen coh =2
save htkr_t4c2, replace

*******append dataset into long form*****
clear
use htkr_t1c2.dta
append using htkr_t2c2.dta, gen(merge_1)
append using htkr_t3c2.dta, gen(merge_2)
append using htkr_t4c2.dta, gen(merge_3)

fre merge*
drop merge*

sort id

order id wave coh htko* htkra* htkrb* htkr1-htkr10 htkrc* htkrd* htkr11-htkr20 htkre* ///
htkrf* htkr21-htkr30

save htkr_t1t2t3t4.dta, replace

******Recode missing values for use in mplus*****

*recode missing data to be . 

recode htk* (.a=.) (.m=.) (.r=.)

*recode contiuation stops to be 0

recode htk* (.n=0)

save htkr_t1t2t3t4.dta, replace

*recode missing for mplus codes and export to .csv
recode htk* (.=-999)

export delimited using "htkr_t1t2t3t4.csv", novarnames nolabel replace
clear


****Check Frequency of Response Categories for opposites and practice items****

*frequencies of categories for opposites and practice items*
use htkr_t1t2t3t4.dta
fre htko* htkra* htkrb* htkrc* htkrd* htkre* htkrf*


******recode opposites items for no self-correct**************
clear
use htkr_t1t2t3t4.dta

recode htko* (1=0)

fre htko*

****export file to .csv for mplus******
recode htk* (.=-999)
export delimited using "htkr_t1t2t3t4_selfcor2.csv", novarnames nolabel replace
clear


******recode opposites items for no self-correct**************
clear
use htkr_t1t2t3t4.dta

recode htko* (1=0)

fre htko*

****export file to .csv for mplus******
recode htk* (.=-999)
export delimited using "htkr_t1t2t3t4_selfcor2.csv", novarnames nolabel replace
clear



****************Construct Validity of HTKS-R*******************

recode htkrtf_* dngtsm_* dcbrsm_* wjwmw_* (.m=.)(.r=.)(.a=.)(.n=.)

sem (htkrtf_1 <- agemos_1 gender cspan_1 dngtsm_1 dcbrsm_1 wjwmw_1 agemos_1 gender cspan_1), standardize method(mlmv)

sem (htkrtf_2 <- agemos_1 gender cspan_1 dngtsm_2 dcbrsm_2 wjwmw_2 ), standardize method(mlmv)

sem (htkrtf_3 <- agemos_1 gender cspan_1 dngtsm_3 dcbrsm_3 wjwmw_3 ), standardize method(mlmv)

sem (htkrtf_4 <- agemos_1 gender cspan_1 dngtsm_4 dcbrsm_4 wjwmw_4 ), standardize method(mlmv)

*******no missing data handling*****
reg htkrtf_1 dngtsm_1 dcbrsm_1 wjwmw_1 agemos_1 gender cspan_1, beta

reg htkrtf_2 dngtsm_2 dcbrsm_2 wjwmw_2 agemos_2 gender cspan_1, beta

reg htkrtf_3 dngtsm_3 dcbrsm_3 wjwmw_3 agemos_3 gender cspan_1, beta 

reg htkrtf_4 dngtsm_4 dcbrsm_4 wjwmw_4 agemos_4 gender cspan_1, beta 


******Calculating scores for Sub Parts of HTKS****

**time1
gen htksp0_1 = htkop1_1 + htkos1_1
gen htksp1_1 = htkrs1_1 + htkrp1_1
gen htksp2_1 = htkrs2_1 + htkrp2_1
gen htksp3_1 = htkrs3_1 + htkrp3_1

recode htksp1_1 (.=0) if htksp0_1<.
recode htksp2_1 (.=0) if htksp0_1<.
recode htksp3_1 (.=0) if htksp0_1<.


pwcorr htksp0_1-htksp3_1, sig
sum htksp0_1 - htksp3_1

***Time2
gen htksp0_2 = htkop1_2 + htkos1_2
gen htksp1_2 = htkrs1_2 + htkrp1_2
gen htksp2_2 = htkrs2_2 + htkrp2_2
gen htksp3_2 = htkrs3_2 + htkrp3_2

recode htksp1_2 (.=0) if htksp0_2<.
recode htksp2_2 (.=0) if htksp0_2<.
recode htksp3_2 (.=0) if htksp0_2<.

pwcorr htksp0_2-htksp3_2, sig
sum htksp0_2 - htksp3_2

*Time3

gen htksp0_3 = htkop1_3 + htkos1_3
gen htksp1_3 = htkrs1_3 + htkrp1_3
gen htksp2_3 = htkrs2_3 + htkrp2_3
gen htksp3_3 = htkrs3_3 + htkrp3_3

recode htksp1_3 (.=0) if htksp0_3<.
recode htksp2_3 (.=0) if htksp0_3<.
recode htksp3_3 (.=0) if htksp0_3<.

pwcorr htksp0_3-htksp3_3, sig
sum htksp0_3 - htksp3_3
*Time 4

gen htksp0_4 = htkop1_4 + htkos1_4
gen htksp1_4 = htkrs1_4 + htkrp1_4
gen htksp2_4 = htkrs2_4 + htkrp2_4
gen htksp3_4 = htkrs3_4 + htkrp3_4

recode htksp1_4 (.=0) if htksp0_4<.
recode htksp2_4 (.=0) if htksp0_4<.
recode htksp3_4 (.=0) if htksp0_4<.

pwcorr htksp0_4-htksp3_4, sig
sum htksp0_4 - htksp3_4

