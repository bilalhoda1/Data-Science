##########################################
# GSS data
GSS.data <- read.csv("GSSdata.txt", header = TRUE, sep = ",", dec = ".")

#I need to recode various values. I'd wager there's a more efficient way to do this task, but I'm pretty quick with my text editor, so I just do it this way. If you want to find the better way, I'd suggest looking at a package called dplyr

GSS.data$SATJOB[GSS.data$SATJOB==0 | GSS.data$SATJOB > 7] <- NA
GSS.data$RACE[GSS.data$RACE==0 | GSS.data$RACE > 7] <- NA
GSS.data$MARITAL[GSS.data$MARITAL==0 | GSS.data$MARITAL > 7] <- NA
GSS.data$JOBLOSE[GSS.data$JOBLOSE==0 | GSS.data$JOBLOSE > 7] <- NA
GSS.data$JOBFIND[GSS.data$JOBFIND==0 | GSS.data$JOBFIND > 7] <- NA
GSS.data$WKVSFAM[GSS.data$WKVSFAM==0 | GSS.data$WKVSFAM > 7] <- NA
GSS.data$FAMVSWK[GSS.data$FAMVSWK==0 | GSS.data$FAMVSWK > 7] <- NA
GSS.data$LEARNNEW[GSS.data$LEARNNEW==0 | GSS.data$LEARNNEW > 7] <- NA
GSS.data$WORKFAST[GSS.data$WORKFAST==0 | GSS.data$WORKFAST > 7] <- NA
GSS.data$WORKDIFF[GSS.data$WORKDIFF==0 | GSS.data$WORKDIFF > 7] <- NA
GSS.data$LOTOFSAY[GSS.data$LOTOFSAY==0 | GSS.data$LOTOFSAY > 7] <- NA
GSS.data$OVERWORK[GSS.data$OVERWORK==0 | GSS.data$OVERWORK > 7] <- NA
GSS.data$KNOWWHAT[GSS.data$KNOWWHAT==0 | GSS.data$KNOWWHAT > 7] <- NA
GSS.data$MYSKILLS[GSS.data$MYSKILLS==0 | GSS.data$MYSKILLS > 7] <- NA
GSS.data$RESPECT[GSS.data$RESPECT==0 | GSS.data$RESPECT > 7] <- NA
GSS.data$TRUSTMAN[GSS.data$TRUSTMAN==0 | GSS.data$TRUSTMAN > 7] <- NA
GSS.data$SAFEHLTH[GSS.data$SAFEHLTH==0 | GSS.data$SAFEHLTH > 7] <- NA
GSS.data$PROUDEMP[GSS.data$PROUDEMP==0 | GSS.data$PROUDEMP > 7] <- NA
GSS.data$PRODCTIV[GSS.data$PRODCTIV==0 | GSS.data$PRODCTIV > 7] <- NA
GSS.data$WKSMOOTH[GSS.data$WKSMOOTH==0 | GSS.data$WKSMOOTH > 7] <- NA
GSS.data$PARTTEAM[GSS.data$PARTTEAM==0 | GSS.data$PARTTEAM > 7] <- NA
GSS.data$WKDECIDE[GSS.data$WKDECIDE==0 | GSS.data$WKDECIDE > 7] <- NA
GSS.data$SETTHNGS[GSS.data$SETTHNGS==0 | GSS.data$SETTHNGS > 7] <- NA
GSS.data$PROMTEOK[GSS.data$PROMTEOK==0 | GSS.data$PROMTEOK > 7] <- NA
GSS.data$OPDEVEL[GSS.data$OPDEVEL==0 | GSS.data$OPDEVEL > 7] <- NA
GSS.data$HAVEINFO[GSS.data$HAVEINFO==0 | GSS.data$HAVEINFO > 7] <- NA
GSS.data$WKFREEDM[GSS.data$WKFREEDM==0 | GSS.data$WKFREEDM > 7] <- NA
GSS.data$FRINGEOK[GSS.data$FRINGEOK==0 | GSS.data$FRINGEOK > 7] <- NA
GSS.data$SUPCARES[GSS.data$SUPCARES==0 | GSS.data$SUPCARES > 7] <- NA
GSS.data$CONDEMND[GSS.data$CONDEMND==0 | GSS.data$CONDEMND > 7] <- NA
GSS.data$JOBSECOK[GSS.data$JOBSECOK==0 | GSS.data$JOBSECOK > 7] <- NA
GSS.data$CHNGTME[GSS.data$CHNGTME==0 | GSS.data$CHNGTME > 7] <- NA
GSS.data$SATJOB[GSS.data$SATJOB==0 | GSS.data$SATJOB > 7] <- NA
GSS.data$SATJOB[GSS.data$SATJOB==0 | GSS.data$SATJOB > 7] <- NA
GSS.data$AGE[GSS.data$AGE > 97] <- NA
GSS.data$EDUC[GSS.data$EDUC==0] <- NA
GSS.data$PRESTG80[GSS.data$PRESTG80==0] <- NA
GSS.data$SEI[GSS.data$SEI==-1 | GSS.data$SEI > 97] <- NA
GSS.data$WORDSUM[GSS.data$WORDSUM==-1 | GSS.data$WORDSUM==99] <- NA


# generate new variables
# especially change variables to have an intuitive labels & directions
GSS.data$JobSat <- 5-GSS.data$SATJOB
GSS.data$FindNew <- 4-GSS.data$JOBFIND
GSS.data$SchedCon <- 5-GSS.data$CHNGTME
GSS.data$WorkFam <- 5-GSS.data$WKVSFAM
GSS.data$FamWork <- 5-GSS.data$FAMVSWK
GSS.data$JobLearn <- 5-GSS.data$LEARNNEW
GSS.data$WorkFast <- 5-GSS.data$WORKFAST
GSS.data$WorkVary <- 5-GSS.data$WORKDIFF
GSS.data$Respect <- 5-GSS.data$RESPECT
GSS.data$TrustMgr <- 5-GSS.data$TRUSTMAN
GSS.data$JobPride <- 5-GSS.data$PROUDEMP
GSS.data$NoPromo <- 5-GSS.data$PROMTEOK

GSS.data$LowAut <- GSS.data$LOTOFSAY
GSS.data$QuanOK <- GSS.data$OVERWORK
GSS.data$RoleAmb <- GSS.data$KNOWWHAT
GSS.data$WasteSkill <- GSS.data$MYSKILLS
GSS.data$Unsafe <- GSS.data$SAFEHLTH
GSS.data$Unproduct <- GSS.data$PRODCTIV
GSS.data$Conflict <- GSS.data$WKSMOOTH
GSS.data$NoDM <- GSS.data$WKDECIDE
GSS.data$LowCont <- GSS.data$SETTHNGS
GSS.data$NoDevel <- GSS.data$OPDEVEL
GSS.data$NoInfo <- GSS.data$HAVEINFO
GSS.data$NoFree <- GSS.data$WKFREEDM
GSS.data$BadBen <- GSS.data$FRINGEOK
GSS.data$SuperBad <- GSS.data$SUPCARES
GSS.data$DemConf <- GSS.data$CONDEMND
GSS.data$LowSecur <- GSS.data$JOBSECOK
GSS.data$Female <- GSS.data$SEX-1

# this would be treated as ordinal/interval in stats, which would be WRONG.
# need categorical variable (specific choice determined by research question).
GSS.data$Married[GSS.data$MARITAL > 1] <- 0

# in the original: 1 = yes on team, 2 = no.
# above changes it to 1 = yes and 0 = no.
GSS.data$WrkTeam <- abs(2-GSS.data$PARTTEAM)

# this would often NOT be a good idea. have a good reason.
GSS.data$FamSiz <- GSS.data$RESPNUM
GSS.data$FamSiz[GSS.data$MARITAL == 99] <- NA
GSS.data$FamSiz[GSS.data$MARITAL > 6] <- 6

# 5 is "leaving labor force," which i choose to treat as missing.
GSS.data$JobSecur <- GSS.data$JOBLOSE
GSS.data$JobSecur[GSS.data$JobSecur == 5] <- NA

# data cleaned.
#################################################
