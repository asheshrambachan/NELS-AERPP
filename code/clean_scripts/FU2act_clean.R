# Time spent on extracurriculars per week
# Base case = multiple responses
F2exhr0 = ifelse(NELS$F2S31 == 0, 1, 0);
F2exhr1 = ifelse(NELS$F2S31 == 1, 1, 0);
F2exhr2 = ifelse(NELS$F2S31 == 2, 1, 0);
F2exhr3 = ifelse(NELS$F2S31 == 3, 1, 0);
F2exhr4 = ifelse(NELS$F2S31 == 4, 1, 0);
F2exhr5 = ifelse(NELS$F2S31 == 5, 1, 0);
F2exhr6 = ifelse(NELS$F2S31 == 6, 1, 0);
F2exhr7 = ifelse(NELS$F2S31 == 7, 1, 0);
F2exhrm = ifelse(NELS$F2S31 == 98, 1, 0); # missing indicator
F2exhrnc = ifelse(NELS$F2S31 == 99, 1, 0);
NELS <- cbind(NELS, F2exhr0, F2exhr1, F2exhr2, F2exhr3, F2exhr4,
              F2exhr5, F2exhr6, F2exhr7, F2exhrm, F2exhrnc);
rm(F2exhr0, F2exhr1, F2exhr2, F2exhr3, F2exhr4,
   F2exhr5, F2exhr6, F2exhr7, F2exhrm, F2exhrnc);
NELS <- subset(NELS, select = -c(F2S31));

# Team sport
# Base case: School doesn't have
F2team2 = ifelse(NELS$F2S30AA == 2, 1, 0);
F2team3 = ifelse(NELS$F2S30AA == 3, 1, 0);
F2team4 = ifelse(NELS$F2S30AA == 4, 1, 0);
F2team5 = ifelse(NELS$F2S30AA == 5, 1, 0);
F2team6 = ifelse(NELS$F2S30AA == 6, 1, 0);
F2teamm = ifelse(NELS$F2S30AA == 8, 1, 0); # missing indicator
F2teamnc = ifelse(NELS$F2S30AA == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2team2, F2team3, F2team4, F2team5, F2team6, F2teamm, F2teamnc)
rm(F2team2, F2team3, F2team4, F2team5, F2team6, F2teamm, F2teamnc);

# Individual sport
# Base case: School doesn't have
F2ind2 = ifelse(NELS$F2S30AB == 2, 1, 0);
F2ind3 = ifelse(NELS$F2S30AB == 3, 1, 0);
F2ind4 = ifelse(NELS$F2S30AB == 4, 1, 0);
F2ind5 = ifelse(NELS$F2S30AB == 5, 1, 0);
F2indm = ifelse(NELS$F2S30AB == 8, 1, 0); # missing indicator
F2indnc = ifelse(NELS$F2S30AB == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2ind2, F2ind3, F2ind4, F2ind5, F2indm, F2indnc)
rm(F2ind2, F2ind3, F2ind4, F2ind5, F2indm, F2indnc);

# Cheer
# Base case: School doesn't have
F2cheer2 = ifelse(NELS$F2S30AC == 2, 1, 0);
F2cheer3 = ifelse(NELS$F2S30AC == 3, 1, 0);
F2cheer4 = ifelse(NELS$F2S30AC == 4, 1, 0);
F2cheer5 = ifelse(NELS$F2S30AC == 5, 1, 0);
F2cheer6 = ifelse(NELS$F2S30AC == 6, 1, 0);
F2cheerm = ifelse(NELS$F2S30AC == 8, 1, 0); # missing indicator
F2cheernc = ifelse(NELS$F2S30AC == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2cheer2, F2cheer3, F2cheer4, F2cheer5, F2cheer6, F2cheerm, F2cheernc)
rm(F2cheer2, F2cheer3, F2cheer4, F2cheer5, F2cheer6, F2cheerm, F2cheernc);

NELS <- subset(NELS, select = -c(F2S30AA, F2S30AB, F2S30AC));

# School music group
# Base case: School doesn't have
F2music2 = ifelse(NELS$F2S30BA == 2, 1, 0);
F2music3 = ifelse(NELS$F2S30BA == 3, 1, 0);
F2music4 = ifelse(NELS$F2S30BA == 4, 1, 0);
F2musicm = ifelse(NELS$F2S30BA == 8, 1, 0); # missing indicator
F2musicnc = ifelse(NELS$F2S30BA == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2music2, F2music3, F2music4, F2musicm, F2musicnc)
rm(F2music2, F2music3, F2music4, F2musicm, F2musicnc);

# Plays/Musicals
# Base case: School doesn't have
F2play2 = ifelse(NELS$F2S30BB == 2, 1, 0);
F2play3 = ifelse(NELS$F2S30BB == 3, 1, 0);
F2play4 = ifelse(NELS$F2S30BB == 4, 1, 0);
F2play6 = ifelse(NELS$F2S30BB == 6, 1, 0);
F2playm = ifelse(NELS$F2S30BB == 8, 1, 0); # missing indicator
F2plaync = ifelse(NELS$F2S30BB == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2play2, F2play3, F2play4, F2play6, F2playm, F2plaync)
rm(F2play2, F2play3, F2play4, F2play6, F2playm, F2plaync);

# Student government
# Base case: School doesn't have
F2govt2 = ifelse(NELS$F2S30BC == 2, 1, 0);
F2govt3 = ifelse(NELS$F2S30BC == 3, 1, 0);
F2govt4 = ifelse(NELS$F2S30BC == 4, 1, 0);
F2govt6 = ifelse(NELS$F2S30BC == 6, 1, 0);
F2govtm = ifelse(NELS$F2S30BC == 8, 1, 0); # missing indicator
F2govtnc = ifelse(NELS$F2S30BC == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2govt2, F2govt3, F2govt4, F2govt6, F2govtm, F2govtnc)
rm(F2govt2, F2govt3, F2govt4, F2govt6, F2govtm, F2govtnc);

# Academic honor society
# Base case: School doesn't have
F2honor2 = ifelse(NELS$F2S30BD == 2, 1, 0);
F2honor3 = ifelse(NELS$F2S30BD == 3, 1, 0);
F2honor4 = ifelse(NELS$F2S30BD == 4, 1, 0);
F2honor6 = ifelse(NELS$F2S30BD == 6, 1, 0);
F2honorm = ifelse(NELS$F2S30BD == 8, 1, 0); # missing indicator
F2honornc = ifelse(NELS$F2S30BD == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2honor2, F2honor3, F2honor4, F2honor6, F2honorm, F2honornc);
rm(F2honor2, F2honor3, F2honor4, F2honor6, F2honorm, F2honornc);

# Yearbook/Newspaper
# Base case: School doesn't have
F2news2 = ifelse(NELS$F2S30BE == 2, 1, 0);
F2news3 = ifelse(NELS$F2S30BE == 3, 1, 0);
F2news4 = ifelse(NELS$F2S30BE == 4, 1, 0);
F2newsm = ifelse(NELS$F2S30BE == 8, 1, 0); # missing indicator
F2newsnc = ifelse(NELS$F2S30BE == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2news2, F2news3, F2news4, F2newsm, F2newsnc)
rm(F2news2, F2news3, F2news4, F2newsm, F2newsnc);

# School service club
# Base case: School doesn't have
F2serv2 = ifelse(NELS$F2S30BF == 2, 1, 0);
F2serv3 = ifelse(NELS$F2S30BF == 3, 1, 0);
F2serv4 = ifelse(NELS$F2S30BF == 4, 1, 0);
F2serv6 = ifelse(NELS$F2S30BF == 6, 1, 0);
F2servm = ifelse(NELS$F2S30BF == 8, 1, 0); # missing indicator
F2servnc = ifelse(NELS$F2S30BF == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2serv2, F2serv3, F2serv4, F2serv6, F2servm, F2servnc)
rm(F2serv2, F2serv3, F2serv4, F2serv6, F2servm, F2servnc);

# Academic club
# Base case: School doesn't have
F2acad2 = ifelse(NELS$F2S30BG == 2, 1, 0);
F2acad3 = ifelse(NELS$F2S30BG == 3, 1, 0);
F2acad4 = ifelse(NELS$F2S30BG == 4, 1, 0);
F2acad6 = ifelse(NELS$F2S30BG == 6, 1, 0);
F2acadm = ifelse(NELS$F2S30BG == 8, 1, 0); # missing indicator
F2acadnc = ifelse(NELS$F2S30BG == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2acad2, F2acad3, F2acad4, F2acad6, F2acadm, F2acadnc)
rm(F2acad2, F2acad3, F2acad4, F2acad6, F2acadm, F2acadnc);

# Hobby Clubs
# Base case: School doesn't have
F2hobby2 = ifelse(NELS$F2S30BH == 2, 1, 0);
F2hobby3 = ifelse(NELS$F2S30BH == 3, 1, 0);
F2hobby4 = ifelse(NELS$F2S30BH == 4, 1, 0);
F2hobby6 = ifelse(NELS$F2S30BH == 6, 1, 0);
F2hobbym = ifelse(NELS$F2S30BH == 8, 1, 0); # missing indicator
F2hobbync = ifelse(NELS$F2S30BH == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2hobby2, F2hobby3, F2hobby4, F2hobby6, F2hobbym, F2hobbync)
rm(F2hobby2, F2hobby3, F2hobby4, F2hobby6, F2hobbym, F2hobbync);

# FTA, FHA, FFA
# Base case: School doesn't have
F2FTA2 = ifelse(NELS$F2S30BI == 2, 1, 0);
F2FTA3 = ifelse(NELS$F2S30BI == 3, 1, 0);
F2FTA4 = ifelse(NELS$F2S30BI == 4, 1, 0);
F2FTA6 = ifelse(NELS$F2S30BI == 6, 1, 0);
F2FTAm = ifelse(NELS$F2S30BI == 8, 1, 0); # missing indicator
F2FTAnc = ifelse(NELS$F2S30BI == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2FTA2, F2FTA3, F2FTA4, F2FTA6, F2FTAm, F2FTAnc)
rm(F2FTA2, F2FTA3, F2FTA4, F2FTA6, F2FTAm, F2FTAnc);

# Intramural team sport
# Base case: School doesn't have
F2ITM2 = ifelse(NELS$F2S30BJ == 2, 1, 0);
F2ITM3 = ifelse(NELS$F2S30BJ == 3, 1, 0);
F2ITM4 = ifelse(NELS$F2S30BJ == 4, 1, 0);
F2ITM6 = ifelse(NELS$F2S30BJ == 6, 1, 0);
F2ITMm = ifelse(NELS$F2S30BJ == 8, 1, 0); # missing indicator
F2ITMnc = ifelse(NELS$F2S30BJ == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2ITM2, F2ITM3, F2ITM4, F2ITM6, F2ITMm, F2ITMnc)
rm(F2ITM2, F2ITM3, F2ITM4, F2ITM6, F2ITMm, F2ITMnc);

# Intramural individual sport
# Base case: School doesnt have
F2IIM2 = ifelse(NELS$F2S30BK == 2, 1, 0);
F2IIM3 = ifelse(NELS$F2S30BK == 3, 1, 0);
F2IIM4 = ifelse(NELS$F2S30BK == 4, 1, 0);
F2IIM6 = ifelse(NELS$F2S30BK == 6, 1, 0);
F2IIMm = ifelse(NELS$F2S30BK == 8, 1, 0); # missing indicator
F2IIMnc = ifelse(NELS$F2S30BK == 9, 1, 0); # legit skip indicator
NELS <- cbind(NELS, F2IIM2, F2IIM3, F2IIM4, F2IIM6, F2IIMm, F2IIMnc)
rm(F2IIM2, F2IIM3, F2IIM4, F2IIM6, F2IIMm, F2IIMnc);

NELS <- subset(NELS, select = -c(F2S30BA, F2S30BB, F2S30BC, F2S30BD, F2S30BE,
                                 F2S30BF, F2S30BG, F2S30BH, F2S30BI, F2S30BJ, 
                                 F2S30BK));

# Volunteer with youth group
# Base case: legit skip
NELS <- NELS[NELS$F2S39A != 6, ] # drop multiple answers
F2VYG1 = ifelse(NELS$F2S39A == 1, 1, 0);
F2VYG2 = ifelse(NELS$F2S39A == 2, 1, 0);
F2VYG8 = ifelse(NELS$F2S39A == 8, 1, 0);
NELS <- cbind(NELS, F2VYG1, F2VYG2, F2VYG8);
rm(F2VYG1, F2VYG2, F2VYG8);

# Volunteer with service group
# Base case: legit skip
NELS <- NELS[NELS$F2S39B != 6, ] # drop multiple answers
F2VSG1 = ifelse(NELS$F2S39B == 1, 1, 0);
F2VSG2 = ifelse(NELS$F2S39B == 2, 1, 0);
F2VSG8 = ifelse(NELS$F2S39B == 8, 1, 0);
NELS <- cbind(NELS, F2VSG1, F2VSG2, F2VSG8);
rm(F2VSG1, F2VSG2, F2VSG8);

# Volunteer with political group
# Base case: Legit skip
F2VPG1 = ifelse(NELS$F2S39C == 1, 1, 0);
F2VPG2 = ifelse(NELS$F2S39C == 2, 1, 0);
F2VPG8 = ifelse(NELS$F2S39C == 8, 1, 0);
NELS <- cbind(NELS, F2VPG1, F2VPG2, F2VPG8);
rm(F2VPG1, F2VPG2, F2VPG8);

# Volunteer with church group
# Base case: legit skip
NELS <- NELS[NELS$F2S39D != 6, ] # drop multiple answers
F2VCHG1 = ifelse(NELS$F2S39D == 1, 1, 0);
F2VCHG2 = ifelse(NELS$F2S39D == 2, 1, 0);
F2VCHG8 = ifelse(NELS$F2S39D == 8, 1, 0);
NELS <- cbind(NELS, F2VCHG1, F2VCHG2, F2VCHG8);
rm(F2VCHG1, F2VCHG2, F2VCHG8);

# Volunteer with community group
# Base case: legit skip
NELS <- NELS[NELS$F2S39E != 6, ] # drop multiple answers
F2VCOG1 = ifelse(NELS$F2S39E == 1, 1, 0);
F2VCOG2 = ifelse(NELS$F2S39E == 2, 1, 0);
F2VCOG8 = ifelse(NELS$F2S39E == 8, 1, 0);
NELS <- cbind(NELS, F2VCOG1, F2VCOG2, F2VCOG8);
rm(F2VCOG1, F2VCOG2, F2VCOG8);

# Volunteer in hospital
# Base case: legit skip
NELS <- NELS[NELS$F2S39F != 6, ] # drop multiple answers
F2HOS1 = ifelse(NELS$F2S39F == 1, 1, 0);
F2HOS2 = ifelse(NELS$F2S39F == 2, 1, 0);
F2HOS8 = ifelse(NELS$F2S39F == 8, 1, 0);
NELS <- cbind(NELS, F2HOS1, F2HOS2, F2HOS8);
rm(F2HOS1, F2HOS2, F2HOS8);

# Volunteer in education 
# Base case: legit skip
NELS <- NELS[NELS$F2S39G != 6, ] # drop multiple answers
F2EDU1 = ifelse(NELS$F2S39G == 1, 1, 0);
F2EDU2 = ifelse(NELS$F2S39G == 2, 1, 0);
F2EDU8 = ifelse(NELS$F2S39G == 8, 1, 0);
NELS <- cbind(NELS, F2EDU1, F2EDU2, F2EDU8);
rm(F2EDU1, F2EDU2, F2EDU8);

# Volunteer on environmental issue
# Base case: legit skip
NELS <- NELS[NELS$F2S39H != 6, ] # drop multiple answers
F2ENV1 = ifelse(NELS$F2S39H == 1, 1, 0);
F2ENV2 = ifelse(NELS$F2S39H == 2, 1, 0);
F2ENV8 = ifelse(NELS$F2S39H == 8, 1, 0);
NELS <- cbind(NELS, F2ENV1, F2ENV2, F2ENV8);
rm(F2ENV1, F2ENV2, F2ENV8);

NELS <- subset(NELS, select = -c(F2S39A, F2S39B, F2S39C, F2S39D, 
                                 F2S39E, F2S39F, F2S39G, F2S39H));
