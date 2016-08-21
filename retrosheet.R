
#might like to vary taking extra base by baseand out. 

#Tango estimate (low because correcting for his sim not having outs made on bases)

#EVENT RUNNERON  0Out  1Out  2OUt
#1B    1B         .25   .25   .25
#1B   2B          .3    .5    .75
#2B   1B          .15   .25   .5
#OUT  1B          .05   .05   0
#OUT  2B          .25   .25   0
#OUT  3B          .5    .5  0

#out on bases rate 3.7%

#should just start with some roughly calibrated SWAGs mocked in and get the loop working before doubling back to calibrate.

#############################
#for gbout, could be 
##fielders choice (lead runner out, batter reaches first), 
##double play (lead runner + batter out), 
##or productive out (batter out, runners advance) 

#for fbout can't be fielder's choice. 
# but could be like k (runners stay where they are)
#Eventually, function of fb distance, OF arm, runner speed, game situation


#requires chadwick to be installed
#http://www.pitchbypitch.com/2013/11/29/installing-chadwick-software-on-mac/

setwd("~/Box Sync/cms/RProjects/bbsim/")

# if you need new data, run this block
# library(devtools)
# source_gist(8892981)  # reads in parse.retrosheet2.pbp 
# parse.retrosheet2.pbp(2015)

library(readr)
fields=read_csv("download.folder/unzipped/fields.csv")
all2015=read_csv("download.folder/unzipped/all2015.csv", col_names = fields$Header)

library(dplyr)

#GBOut with 2 outs ends inning
# Given GB out with runner on 1st <2outs, what is chance of double play vs. FC vs. productive out
gbOutInDpSitch = nrow(all2015 %>% filter(
  OUTS_CT<2 & BASE1_RUN_ID !="" & BATTEDBALL_CD=='G' & EVENT_CD %in% c(2,18,19) & SH_FL==FALSE))
gbDps = nrow(all2015 %>% filter(DP_FL==TRUE & BATTEDBALL_CD=='G'& EVENT_CD %in% c(2,18,19) & SH_FL==FALSE))

#so given that there's a DP situation and a GB caught(or errored) by an infielder, the chance of converting the DP is


first2OutDpOnGbOut = nrow(all2015 %>% filter(
  OUTS_CT<2 & BATTEDBALL_CD=='G'& EVENT_CD %in% c(2,18,19) & DP_FL==TRUE & SH_FL==FALSE & BASE1_RUN_ID !="" & RUN1_DEST_ID==0) )
dp1OnGbOut = first2OutDpOnGbOut / gbOutInDpSitch
#45%

first2SecondOnGbOut = nrow(all2015 %>% filter(
  OUTS_CT<2 & BATTEDBALL_CD=='G'& EVENT_CD %in% c(2,18,19) & SH_FL==FALSE & BASE1_RUN_ID !="" & RUN1_DEST_ID==2) )
prod1OnGbOut = first2SecondOnGbOut/gbOutInDpSitch
#26%

first2OutOnGbOutBatterReaches = nrow(all2015 %>% filter(
  OUTS_CT<2 & BATTEDBALL_CD=='G'& EVENT_CD %in% c(2,18,19) & SH_FL==FALSE & BASE1_RUN_ID !="" & RUN1_DEST_ID==0 & BAT_DEST_ID==1) )
fc1OnGbOut = first2OutOnGbOutBatterReaches / gbOutInDpSitch
#26%

#3% of the time, something weird happens to runner on first. (2-base error, etc..) Ignore and just call this 50/25/25
#Bottom line, if gbOut with runner on first & <2 out, go to dp 50%, go to +1 out & man on second 25%, go to +1 out & man on first 25%





#maybe treat errors separately?


