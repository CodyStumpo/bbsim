
batterCharacteristics = data.frame(name = character(), 
                                   bats=factor(levels = c('L','R','S')), 
                                   takeXB=double(),
                                   stringsAsFactors = FALSE)

#B-R has GO/AO, XB%, GDP%, ROE, by batter

batterCharacteristics[nrow(batterCharacteristics)+1,] <- c('Utley', 'L', 0.55)
batterCharacteristics[nrow(batterCharacteristics)+1,] <- c('Seager', 'L', 0.4)
batterCharacteristics[nrow(batterCharacteristics)+1,] <- c('Turner', 'R', 0.38)
batterCharacteristics[nrow(batterCharacteristics)+1,] <- c('Gonzalez', 'L', 0.25)
batterCharacteristics[nrow(batterCharacteristics)+1,] <- c('Puig', 'R', 0.5)
batterCharacteristics[nrow(batterCharacteristics)+1,] <- c('Crawford', 'L', 0.45)
batterCharacteristics[nrow(batterCharacteristics)+1,] <- c('Pederson', 'L', 0.38)
batterCharacteristics[nrow(batterCharacteristics)+1,] <- c('Ellis', 'R', 0.25)
batterCharacteristics$takeXB=as.numeric(batterCharacteristics$takeXB)

#this is percent successfully take extra base. Need also percent tried and get thrown out (few times per year)

# then as part of daily projection vs. opponent, takeoff rate and sb success rate
# many possibilities on how to do GDP

batterOutcomes = data.frame(sngl=double(), dbl=double(), trpl=double(), 
                     HR=double(), BB=double(), HBP=double(), K=double(), GBout=double(), FBout=double()
                     )

boInsert = function( sngl, dbl, trpl, HR, BB, HBP, K, PA, goaoRatio)  {
  x= c(sngl, dbl, trpl, HR, BB, HBP, K, 0,0)/PA 
  bipOuts=1-sum(x)
  x[8]=bipOuts*goaoRatio/(1+goaoRatio)
  x[9]=bipOuts*1/(1+goaoRatio)
  x
}

                    
# these are disjoint probabilities per plate appearance, summing to 1 per batter. 
for (i in 1:8) batterOutcomes[i,] = rep(1/9,9)
rownames(batterOutcomes) <- batterCharacteristics$name

batterOutcomes["Utley",]= boInsert(50, 18, 2, 7, 26, 7, 53,360,.9)
batterOutcomes["Seager",]=boInsert(86, 32, 3, 17, 34, 4, 109, 567,1.4)
batterOutcomes["Turner",]=boInsert(88,31,1,13,39,9,90,525,1.25)
batterOutcomes["Gonzalez",]=boInsert(98,32,0,24,53,3,111,630,0.9)
batterOutcomes["Puig",]=boInsert(89,29,6,21,53,8,117,581,1.15)
batterOutcomes["Crawford",]=boInsert(64,17,3,7,19,3,63,370, 1.06)
batterOutcomes["Pederson",]=boInsert(68,19,2,25,84,6,174,609,.97)
batterOutcomes["Ellis",]=boInsert(30,8,1,5,27,2,42,224,1.07)
batterOutcomes["genericPitcher",]=boInsert(470,87,5,23,133,13,1878,5054, 2.06)
batterCharacteristics[nrow(batterCharacteristics)+1,] <- c('genericPitcher', 'R', 0.3)
batterCharacteristics$takeXB=as.numeric(batterCharacteristics$takeXB)
#2015 NL pitchers 1221 GO / 417 + 175 FO + LO


batterOutcomeRanges=data.frame(t(apply(batterOutcomes, 1, cumsum)))


#2015 NL rates
TBF=91923
WP =820/TBF
BK = 79/TBF
PB = 147/TBF
DP = 5471 / TBF # want this denominator to be balls in play in DP situations 

bkWpPb = BK + WP + PB

H=.253*91923  #League BA = .253
BIP = H/.302 #League BABIP = .302

E = 1423 / BIP # want this denominator to be balls in play
#If GBOut or FBOut, chance of Reaching on error (some of these errors are not about reaching, but just extra base - ignore)
XB = .4 #40% of time, baserunners take the extra base.

grounders = 29225 
gbHits = 6887
gdp = 1840
gdpPerGBOut = gdp / (grounders-gbHits)
# gdpPerOpp = 10-15% where opp is man on first & <2 outs

#productive out
#when a fly ball, grounder or bunt advances a runner with nobody out; 
#when a pitcher bunts to advance a runner with one out (maximizing the effectiveness of the pitcher's at-bat), 
#or when a grounder or fly ball scores a run with one out.
