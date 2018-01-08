# ###################################################
# Author: Ashesh Rambachan
# Last Updated: 12/5/2017
# This script is a helper script for data_clean.R. 
# It cleans the FU1 activities covariates
# ###################################################

# Hours spent on extracurriculars
# Base case = multiple answers
F1exhr0 = ifelse(NELS$F1S42 == 0, 1, 0);
F1exhr1 = ifelse(NELS$F1S42 == 1, 1, 0);
F1exhr2 = ifelse(NELS$F1S42 == 2, 1, 0);
F1exhr3 = ifelse(NELS$F1S42 == 3, 1, 0);
F1exhr4 = ifelse(NELS$F1S42 == 4, 1, 0);
F1exhr5 = ifelse(NELS$F1S42 == 5, 1, 0);
F1exhrm = ifelse(NELS$F1S42 == 98, 1, 0); # missing indicator
F1exhrnc = ifelse(NELS$F1S42 == 99, 1, 0);
NELS <- cbind(NELS, F1exhr0, F1exhr1, F1exhr2, F1exhr3, 
              F1exhr4, F1exhr5, F1exhrm, F1exhrnc);
rm(F1exhr0, F1exhr1, F1exhr2, F1exhr3, 
   F1exhr4, F1exhr5, F1exhrm, F1exhrnc);
NELS <- subset(NELS, select = -c(F1S42));

# Baseball/Softball
# Base case = multiple answers; 
F1baseball1 = ifelse(NELS$F1S41AA == 1, 1, 0);
F1baseball2 = ifelse(NELS$F1S41AA == 2, 1, 0);
F1baseball3 = ifelse(NELS$F1S41AA == 3, 1, 0);
F1baseball4 = ifelse(NELS$F1S41AA == 4, 1, 0);
F1baseball5 = ifelse(NELS$F1S41AA == 5, 1, 0);
F1baseball6 = ifelse(NELS$F1S41AA == 6, 1, 0);
F1baseballm = ifelse(NELS$F1S41AA == 98, 1, 0); # missing indicator
F1baseballnc = ifelse(NELS$F1S41AA == 99, 1, 0);
NELS <- cbind(NELS, F1baseball1, F1baseball2, F1baseball3, F1baseball4,
              F1baseball5, F1baseball6, F1baseballm, F1baseballnc);
rm(F1baseball1, F1baseball2, F1baseball3, F1baseball4,
   F1baseball5, F1baseball6, F1baseballm, F1baseballnc);

# Basketball
# Base case = multiple answers; 
F1basketball1 = ifelse(NELS$F1S41AB == 1, 1, 0);
F1basketball2 = ifelse(NELS$F1S41AB == 2, 1, 0);
F1basketball3 = ifelse(NELS$F1S41AB == 3, 1, 0);
F1basketball4 = ifelse(NELS$F1S41AB == 4, 1, 0);
F1basketball5 = ifelse(NELS$F1S41AB == 5, 1, 0);
F1basketball6 = ifelse(NELS$F1S41AB == 6, 1, 0);
F1basketballm = ifelse(NELS$F1S41AB == 98, 1, 0); # missing indicator
F1basketballnc = ifelse(NELS$F1S41AB == 99, 1, 0);
NELS <- cbind(NELS, F1basketball1, F1basketball2, F1basketball3, F1basketball4,
              F1basketball5, F1basketball6, F1basketballm, F1basketballnc);
rm(F1basketball1, F1basketball2, F1basketball3, F1basketball4,
   F1basketball5, F1basketball6, F1basketballm, F1basketballnc);

# Football
# Base case = multiple answers; 
F1football1 = ifelse(NELS$F1S41AC == 1, 1, 0);
F1football2 = ifelse(NELS$F1S41AC == 2, 1, 0);
F1football3 = ifelse(NELS$F1S41AC == 3, 1, 0);
F1football4 = ifelse(NELS$F1S41AC == 4, 1, 0);
F1football5 = ifelse(NELS$F1S41AC == 5, 1, 0);
F1football6 = ifelse(NELS$F1S41AC == 6, 1, 0);
F1footballm = ifelse(NELS$F1S41AC == 98, 1, 0); # missing indicator
F1footballnc = ifelse(NELS$F1S41AC == 99, 1, 0);
NELS <- cbind(NELS, F1football1, F1football2, F1football3, F1football4,
              F1football5, F1football6, F1footballm, F1footballnc);
rm(F1football1, F1football2, F1football3, F1football4,
   F1football5, F1football6, F1footballm, F1footballnc);

# Soccer
# Base case = multiple answers; 
F1soccer1 = ifelse(NELS$F1S41AD == 1, 1, 0);
F1soccer2 = ifelse(NELS$F1S41AD == 2, 1, 0);
F1soccer3 = ifelse(NELS$F1S41AD == 3, 1, 0);
F1soccer4 = ifelse(NELS$F1S41AD == 4, 1, 0);
F1soccer5 = ifelse(NELS$F1S41AD == 5, 1, 0);
F1soccer6 = ifelse(NELS$F1S41AD == 6, 1, 0);
F1soccerm = ifelse(NELS$F1S41AD == 98, 1, 0); # missing indicator
F1soccernc = ifelse(NELS$F1S41AD == 99, 1, 0);
NELS <- cbind(NELS, F1soccer1, F1soccer2, F1soccer3, F1soccer4,
              F1soccer5, F1soccer6, F1soccerm, F1soccernc);
rm(F1soccer1, F1soccer2, F1soccer3, F1soccer4,
   F1soccer5, F1soccer6, F1soccerm, F1soccernc);

# Swim team
# Base case = multiple answers; 
F1swim1 = ifelse(NELS$F1S41AE == 1, 1, 0);
F1swim2 = ifelse(NELS$F1S41AE == 2, 1, 0);
F1swim3 = ifelse(NELS$F1S41AE == 3, 1, 0);
F1swim4 = ifelse(NELS$F1S41AE == 4, 1, 0);
F1swim5 = ifelse(NELS$F1S41AE == 5, 1, 0);
F1swim6 = ifelse(NELS$F1S41AE == 6, 1, 0);
F1swimm = ifelse(NELS$F1S41AE == 98, 1, 0); # missing indicator
F1swimnc = ifelse(NELS$F1S41AE == 99, 1, 0);
NELS <- cbind(NELS, F1swim1, F1swim2, F1swim3, F1swim4,
              F1swim5, F1swim6, F1swimm, F1swimnc);
rm(F1swim1, F1swim2, F1swim3, F1swim4,
   F1swim5, F1swim6, F1swimm, F1swimnc);

# Other team sport
# Base case = multiple answers; 
F1other1 = ifelse(NELS$F1S41AF == 1, 1, 0);
F1other2 = ifelse(NELS$F1S41AF == 2, 1, 0);
F1other3 = ifelse(NELS$F1S41AF == 3, 1, 0);
F1other4 = ifelse(NELS$F1S41AF == 4, 1, 0);
F1other5 = ifelse(NELS$F1S41AF == 5, 1, 0);
F1other6 = ifelse(NELS$F1S41AF == 6, 1, 0);
F1otherm = ifelse(NELS$F1S41AF == 98, 1, 0); # missing indicator
F1othernc = ifelse(NELS$F1S41AF == 99, 1, 0);
NELS <- cbind(NELS, F1other1, F1other2, F1other3, F1other4,
              F1other5, F1other6, F1otherm, F1othernc);
rm(F1other1, F1other2, F1other3, F1other4,
   F1other5, F1other6, F1otherm, F1othernc);

# Individual sport
# Base case = multiple answers; 
F1ind1 = ifelse(NELS$F1S41AG == 1, 1, 0);
F1ind2 = ifelse(NELS$F1S41AG == 2, 1, 0);
F1ind3 = ifelse(NELS$F1S41AG == 3, 1, 0);
F1ind4 = ifelse(NELS$F1S41AG == 4, 1, 0);
F1ind5 = ifelse(NELS$F1S41AG == 5, 1, 0);
F1ind6 = ifelse(NELS$F1S41AG == 6, 1, 0);
F1indm = ifelse(NELS$F1S41AG == 98, 1, 0); # missing indicator
F1indnc = ifelse(NELS$F1S41AG == 99, 1, 0);
NELS <- cbind(NELS, F1ind1, F1ind2, F1ind3, F1ind4,
              F1ind5, F1ind6, F1indm, F1indnc);
rm(F1ind1, F1ind2, F1ind3, F1ind4,
   F1ind5, F1ind6, F1indm, F1indnc);

# Cheerleading
# Base case = multiple answers; 
F1cheer1 = ifelse(NELS$F1S41AH == 1, 1, 0);
F1cheer2 = ifelse(NELS$F1S41AH == 2, 1, 0);
F1cheer3 = ifelse(NELS$F1S41AH == 3, 1, 0);
F1cheer4 = ifelse(NELS$F1S41AH == 4, 1, 0);
F1cheer5 = ifelse(NELS$F1S41AH == 5, 1, 0);
F1cheer6 = ifelse(NELS$F1S41AH == 6, 1, 0);
F1cheerm = ifelse(NELS$F1S41AH == 98, 1, 0); # missing indicator
F1cheernc = ifelse(NELS$F1S41AH == 99, 1, 0);
NELS <- cbind(NELS, F1cheer1, F1cheer2, F1cheer3, F1cheer4,
              F1cheer5, F1cheer6, F1cheerm, F1cheernc);
rm(F1cheer1, F1cheer2, F1cheer3, F1cheer4,
   F1cheer5, F1cheer6, F1cheerm, F1cheernc);

# drill team
# Base case = multiple answers; 
F1drill1 = ifelse(NELS$F1S41AI == 1, 1, 0);
F1drill2 = ifelse(NELS$F1S41AI == 2, 1, 0);
F1drill3 = ifelse(NELS$F1S41AI == 3, 1, 0);
F1drill4 = ifelse(NELS$F1S41AI == 4, 1, 0);
F1drill5 = ifelse(NELS$F1S41AI == 5, 1, 0);
F1drill6 = ifelse(NELS$F1S41AI == 6, 1, 0);
F1drillm = ifelse(NELS$F1S41AI == 98, 1, 0); # missing indicator
F1drillnc = ifelse(NELS$F1S41AI == 99, 1, 0);
NELS <- cbind(NELS, F1drill1, F1drill2, F1drill3, F1drill4,
              F1drill5, F1drill6, F1drillm, F1drillnc);
rm(F1drill1, F1drill2, F1drill3, F1drill4,
   F1drill5, F1drill6, F1drillm, F1drillnc);

NELS <- subset(NELS, select = -c(F1S41AA, F1S41AB, F1S41AC, F1S41AD,
                                 F1S41AE, F1S41AF, F1S41AG, F1S41AH, 
                                 F1S41AI));

# Band
# Base case = school does not offer
F1band2 = ifelse(NELS$F1S41BA == 2, 1, 0);
F1band3 = ifelse(NELS$F1S41BA == 3, 1, 0);
F1band4 = ifelse(NELS$F1S41BA == 4, 1, 0);
F1bandm = ifelse(NELS$F1S41BA == 8, 1, 0); # missing indicator
F1bandnc = ifelse(NELS$F1S41BA == 9, 1, 0); # legit skip
NELS <- cbind(NELS, F1band2, F1band3, F1band4, F1bandm, F1bandnc);
rm(F1band2, F1band3, F1band4, F1bandm, F1bandnc);

# Plays/Musicals
# Base case = school does not offer; 
F1play2 = ifelse(NELS$F1S41BB == 2, 1, 0);
F1play3 = ifelse(NELS$F1S41BB == 3, 1, 0);
F1play4 = ifelse(NELS$F1S41BB == 4, 1, 0);
F1playm = ifelse(NELS$F1S41BB == 8, 1, 0); # missing indicator
F1plaync = ifelse(NELS$F1S41BB == 9, 1, 0); # legit skip
NELS <- cbind(NELS, F1play2, F1play3, F1play4, F1playm, F1plaync);
rm(F1play2, F1play3, F1play4, F1playm, F1plaync);

# Student government
# Base case = school does not offer
F1govt2 = ifelse(NELS$F1S41BC == 2, 1, 0);
F1govt3 = ifelse(NELS$F1S41BC == 3, 1, 0);
F1govt4 = ifelse(NELS$F1S41BC == 4, 1, 0);
F1govt6 = ifelse(NELS$F1S41BC == 6, 1, 0);
F1govtm = ifelse(NELS$F1S41BC == 8, 1, 0); # missing indicator
F1govtnc = ifelse(NELS$F1S41BC == 9, 1, 0); # legit skip
NELS <- cbind(NELS, F1govt2, F1govt3, F1govt4, F1govt6, F1govtm, F1govtnc);
rm(F1govt2, F1govt3, F1govt4, F1govt6, F1govtm, F1govtnc);

# Academic honor society
# Base case = school does not offer
F1honor2 = ifelse(NELS$F1S41BD == 2, 1, 0);
F1honor3 = ifelse(NELS$F1S41BD == 3, 1, 0);
F1honor4 = ifelse(NELS$F1S41BD == 4, 1, 0);
F1honor6 = ifelse(NELS$F1S41BD == 6, 1, 0);
F1honorm = ifelse(NELS$F1S41BD == 8, 1, 0); # missing indicator
F1honornc = ifelse(NELS$F1S41BD == 9, 1, 0); # legit skip
NELS <- cbind(NELS, F1honor2, F1honor3, F1honor4, F1honor6, F1honorm, F1honornc);
rm(F1honor2, F1honor3, F1honor4, F1honor6, F1honorm, F1honornc);

# Yearbook/Newspaper
# Base case = school does not offer
F1news2 = ifelse(NELS$F1S41BE == 2, 1, 0);
F1news3 = ifelse(NELS$F1S41BE == 3, 1, 0);
F1news4 = ifelse(NELS$F1S41BE == 4, 1, 0);
F1news6 = ifelse(NELS$F1S41BE == 6, 1, 0);
F1newsm = ifelse(NELS$F1S41BE == 8, 1, 0); # missing indicator
F1newsnc = ifelse(NELS$F1S41BE == 9, 1, 0); # legit skip
NELS <- cbind(NELS, F1news2, F1news3, F1news4, F1news6, F1newsm, F1newsnc);
rm(F1news2, F1news3, F1news4, F1news6, F1newsm, F1newsnc);

# School services clubs
# Base case = school does not offer
F1serv2 = ifelse(NELS$F1S41BF == 2, 1, 0);
F1serv3 = ifelse(NELS$F1S41BF == 3, 1, 0);
F1serv4 = ifelse(NELS$F1S41BF == 4, 1, 0);
F1serv6 = ifelse(NELS$F1S41BF == 6, 1, 0);
F1servm = ifelse(NELS$F1S41BF == 8, 1, 0); # missing indicator
F1servnc = ifelse(NELS$F1S41BF == 9, 1, 0); # legit skip
NELS <- cbind(NELS, F1serv2, F1serv3, F1serv4, F1serv6, F1servm, F1servnc);
rm(F1serv2, F1serv3, F1serv4, F1serv6, F1servm, F1servnc);

# Academic clubs
# Base case = school does not offer
F1acad2 = ifelse(NELS$F1S41BG == 2, 1, 0);
F1acad3 = ifelse(NELS$F1S41BG == 3, 1, 0);
F1acad4 = ifelse(NELS$F1S41BG == 4, 1, 0);
F1acad6 = ifelse(NELS$F1S41BG == 6, 1, 0);
F1acadm = ifelse(NELS$F1S41BG == 8, 1, 0); # missing indicator
F1acadnc = ifelse(NELS$F1S41BG == 9, 1, 0); # legit skip
NELS <- cbind(NELS, F1acad2, F1acad3, F1acad4, F1acad6, F1acadm, F1acadnc);
rm(F1acad2, F1acad3, F1acad4, F1acad6, F1acadm, F1acadnc);

# Hobby clubs
# Base case = school does not offer
F1hobby2 = ifelse(NELS$F1S41BH == 2, 1, 0);
F1hobby3 = ifelse(NELS$F1S41BH == 3, 1, 0);
F1hobby4 = ifelse(NELS$F1S41BH == 4, 1, 0);
F1hobby6 = ifelse(NELS$F1S41BH == 6, 1, 0);
F1hobbym = ifelse(NELS$F1S41BH == 8, 1, 0); # missing indicator
F1hobbync = ifelse(NELS$F1S41BH == 9, 1, 0); # legit skip
NELS <- cbind(NELS, F1hobby2, F1hobby3, F1hobby4, F1hobby6, F1hobbym, F1hobbync);
rm(F1hobby2, F1hobby3, F1hobby4, F1hobby6, F1hobbym, F1hobbync);

# FTA, FHA, FFA
# Base case = school does not offer, multiple responses
F1FTA2 = ifelse(NELS$F1S41BI == 2, 1, 0);
F1FTA3 = ifelse(NELS$F1S41BI == 3, 1, 0);
F1FTA4 = ifelse(NELS$F1S41BI == 4, 1, 0);
F1FTA6 = ifelse(NELS$F1S41BI == 6, 1, 0);
F1FTAm = ifelse(NELS$F1S41BI == 8, 1, 0); # missing indicator
F1FTAnc = ifelse(NELS$F1S41BI == 9, 1, 0); # legit skip
NELS <- cbind(NELS, F1FTA2, F1FTA3, F1FTA4, F1FTA6, F1FTAm, F1FTAnc);
rm(F1FTA2, F1FTA3, F1FTA4, F1FTA6, F1FTAm, F1FTAnc);

NELS <- subset(NELS, select = -c(F1S41BA, F1S41BB, F1S41BC, F1S41BD,
                                 F1S41BE, F1S41BF, F1S41BG, F1S41BH, 
                                 F1S41BI));