# Load the PROM files
# There are two phases 'd1' and 'd1' with responses at
# - Baseline (00)
# - 1 month (01)
# - 3 months (03)
# - 6 months (06)
# - 12 months (12)
d1_00 = read.csv('../Data/Phase 1 baseline.csv')
d1_01 = read.csv('../Data/Phase 1 Month 1.csv')
d1_03 = read.csv('../Data/Phase 1 Month 3.csv')
d1_06 = read.csv('../Data/Phase 1 Month 6.csv')
d1_12 = read.csv('../Data/Phase 1 Month 12.csv')
d2_00 = read.csv('../Data/Phase 2 baseline.csv')
d2_01 = read.csv('../Data/Phase 2 Month 1.csv')
d2_03 = read.csv('../Data/Phase 2 Month 3.csv')
d2_06 = read.csv('../Data/Phase 2 Month 6.csv')
d2_12 = read.csv('../Data/Phase 2 Month 12.csv')

# Three files have 'Study_ID' column names with problems, rename these to the standard
names(d2_01)[1] = "Study_ID" 
names(d2_03)[1] = "Study_ID"
names(d2_06)[1] = "Study_ID"


# Reshape to wide format by the question
d1_00w = reshape(d1_00, idvar="Study_ID", 
                 timevar="Question_ID", direction="wide")
d1_01w = reshape(d1_01, idvar="Study_ID", 
                 timevar="Question_ID", direction="wide")
d1_03w = reshape(d1_03, idvar="Study_ID", 
                 timevar="Question_ID", direction="wide")
d1_06w = reshape(d1_06, idvar="Study_ID", 
                 timevar="Question_ID", direction="wide")
d1_12w = reshape(d1_12, idvar="Study_ID", 
                 timevar="Question_ID", direction="wide")
d2_00w = reshape(d2_00, idvar="Study_ID", 
                 timevar="Question_ID", direction="wide")
d2_01w = reshape(d2_01, idvar="Study_ID", 
                 timevar="Question_ID", direction="wide")
d2_03w = reshape(d2_03, idvar="Study_ID", 
                 timevar="Question_ID", direction="wide")
d2_06w = reshape(d2_06, idvar="Study_ID", 
                 timevar="Question_ID", direction="wide")
d2_12w = reshape(d2_12, idvar="Study_ID", 
                 timevar="Question_ID", direction="wide")

# Extract the required variables from each phase and time point
# Each time point and each phase had it's own question ID
# This makes it a long manual process
d1_00 = data.frame(
  Phase=1, # Study phase
  Time=0, # Time of the study
  Study_ID=d1_00w$Study_ID, # Patient ID
  DoS=d1_00w$Surgery_date.18657, # Date of surgery
  A23=d1_00w$Answer.18657, # Response to EPIC question 23
  A26=d1_00w$Answer.18658, # Response to EPIC question 26
  A27=d1_00w$Answer.18659, # Response to EPIC question 27
  A28=d1_00w$Answer.18660, # Response to EPIC question 28
  A34=d1_00w$Answer.18665, # Response to EPIC question 34
  A57=d1_00w$Answer.18672, # Response to EPIC question 57
  A58=d1_00w$Answer.18673, # Response to EPIC question 58
  A59=d1_00w$Answer.18674, # Response to EPIC question 59
  A60=d1_00w$Answer.18675, # Response to EPIC question 60
  A64=d1_00w$Answer.18676, # Response to EPIC question 64
  A68=d1_00w$Answer.18677, # Response to EPIC question 68
  Assist=d1_00w$Answer.18684, # Binary whether medications or devices for erections have been used
  Pill=d1_00w$Answer.18685, # Response to if pills were used
  Muse=d1_00w$Answer.18686, # Response to if muse was used
  PIT=d1_00w$Answer.18687, # Response to if PIT was used
  Vac=d1_00w$Answer.18688, # Response to if vacuum was used
  Other=d1_00w$Answer.18689, # Response to if other was used
  Race=d1_00w$Answer.18696, # Patient reported race
  Marital=d1_00w$Answer.18697, # Patient reported marital status
  Diabetes=d1_00w$Answer.18702, # Patient reported diabetes
  HeartD=d1_00w$Answer.18698 # Patient reported heart disease
)
d1_01 = data.frame(
  Phase=1,
  Time=1,
  Study_ID=d1_01w$Study_ID,
  DoS=d1_01w$Surgery_date.18902,
  A23=d1_01w$Answer.18902,
  A26=d1_01w$Answer.18903,
  A27=d1_01w$Answer.18904,
  A28=d1_01w$Answer.18905,
  A34=d1_01w$Answer.18910,
  A57=d1_01w$Answer.18917,
  A58=d1_01w$Answer.18918,
  A59=d1_01w$Answer.18919,
  A60=d1_01w$Answer.18920,
  A64=d1_01w$Answer.18921,
  A68=d1_01w$Answer.18922,
  Assist=d1_01w$Answer.18929,
  Pill=d1_01w$Answer.18930,
  Muse=d1_01w$Answer.18931,
  PIT=d1_01w$Answer.18932,
  Vac=d1_01w$Answer.18933,
  Other=d1_01w$Answer.18934,
  Race=NA,
  Marital=NA,
  Diabetes=NA,
  HeartD=NA
)
d1_03 = data.frame(
  Phase=1,
  Time=3,
  Study_ID=d1_03w$Study_ID,
  DoS=d1_03w$Surgery_date.18844,
  A23=d1_03w$Answer.18844,
  A26=d1_03w$Answer.18845,
  A27=d1_03w$Answer.18846,
  A28=d1_03w$Answer.18847,
  A34=d1_03w$Answer.18852,
  A57=d1_03w$Answer.18859,
  A58=d1_03w$Answer.18860,
  A59=d1_03w$Answer.18861,
  A60=d1_03w$Answer.18862,
  A64=d1_03w$Answer.18863,
  A68=d1_03w$Answer.18864,
  Assist=d1_03w$Answer.18871,
  Pill=d1_03w$Answer.18872,
  Muse=d1_03w$Answer.18873,
  PIT=d1_03w$Answer.18874,
  Vac=d1_03w$Answer.18875,
  Other=d1_03w$Answer.18876,
  Race=NA,
  Marital=NA,
  Diabetes=NA,
  HeartD=NA
)
d1_06 = data.frame(
  Phase=1,
  Time=6,
  Study_ID=d1_06w$Study_ID,
  DoS=d1_06w$Surgery_date.18786,
  A23=d1_06w$Answer.18786,
  A26=d1_06w$Answer.18787,
  A27=d1_06w$Answer.18788,
  A28=d1_06w$Answer.18789,
  A34=d1_06w$Answer.18794,
  A57=d1_06w$Answer.18801,
  A58=d1_06w$Answer.18802,
  A59=d1_06w$Answer.18803,
  A60=d1_06w$Answer.18804,
  A64=d1_06w$Answer.18805,
  A68=d1_06w$Answer.18806,
  Assist=d1_06w$Answer.18813,
  Pill=d1_06w$Answer.18814,
  Muse=d1_06w$Answer.18815,
  PIT=d1_06w$Answer.18816,
  Vac=d1_06w$Answer.18817,
  Other=d1_06w$Answer.18818,
  Race=NA,
  Marital=NA,
  Diabetes=NA,
  HeartD=NA
)
d1_12 = data.frame(
  Phase=1,
  Time=12,
  Study_ID=d1_12w$Study_ID,
  DoS=d1_12w$Surgery_date.18728,
  A23=d1_12w$Answer.18728,
  A26=d1_12w$Answer.18729,
  A27=d1_12w$Answer.18730,
  A28=d1_12w$Answer.18731,
  A34=d1_12w$Answer.18736,
  A57=d1_12w$Answer.18743,
  A58=d1_12w$Answer.18744,
  A59=d1_12w$Answer.18745,
  A60=d1_12w$Answer.18746,
  A64=d1_12w$Answer.18747,
  A68=d1_12w$Answer.18748,
  Assist=d1_12w$Answer.18755,
  Pill=d1_12w$Answer.18756,
  Muse=d1_12w$Answer.18757,
  PIT=d1_12w$Answer.18758,
  Vac=d1_12w$Answer.18759,
  Other=d1_12w$Answer.18760,
  Race=NA,
  Marital=NA,
  Diabetes=NA,
  HeartD=NA
)
d2_00 = data.frame(
  Phase=2,
  Time=0,
  Study_ID=d2_00w$Study_ID,
  DoS=d2_00w$Surgery_date.22159,
  A23=d2_00w$Answer.22159,
  A26=d2_00w$Answer.22160,
  A27=d2_00w$Answer.22161,
  A28=d2_00w$Answer.22162,
  A34=d2_00w$Answer.22167,
  A57=d2_00w$Answer.22174,
  A58=d2_00w$Answer.22175,
  A59=d2_00w$Answer.22176,
  A60=d2_00w$Answer.22177,
  A64=d2_00w$Answer.22178,
  A68=d2_00w$Answer.22179,
  Assist=d2_00w$Answer.22186,
  Pill=d2_00w$Answer.22187,
  Muse=d2_00w$Answer.22188,
  PIT=d2_00w$Answer.22189,
  Vac=d2_00w$Answer.22190,
  Other=d2_00w$Answer.22191,
  Race=d2_00w$Answer.22198,
  Marital=d2_00w$Answer.22199,
  Diabetes=d2_00w$Answer.22205,
  HeartD=d2_00w$Answer.22200
)
d2_01 = data.frame(
  Phase=2,
  Time=1,
  Study_ID=d2_01w$Study_ID,
  DoS=d2_01w$Surgery_date.22232,
  A23=d2_01w$Answer.22232,
  A26=d2_01w$Answer.22233,
  A27=d2_01w$Answer.22234,
  A28=d2_01w$Answer.22235,
  A34=d2_01w$Answer.22240,
  A57=d2_01w$Answer.22247,
  A58=d2_01w$Answer.22248,
  A59=d2_01w$Answer.22249,
  A60=d2_01w$Answer.22250,
  A64=d2_01w$Answer.22251,
  A68=d2_01w$Answer.22252,
  Assist=d2_01w$Answer.22259,
  Pill=d2_01w$Answer.22260,
  Muse=d2_01w$Answer.22261,
  PIT=d2_01w$Answer.22262,
  Vac=d2_01w$Answer.22263,
  Other=d2_01w$Answer.22264,
  Race=NA,
  Marital=NA,
  Diabetes=NA,
  HeartD=NA
)
d2_03 = data.frame(
  Phase=2,
  Time=3,
  Study_ID=d2_03w$Study_ID,
  DoS=d2_03w$Surgery_date.22378,
  A23=d2_03w$Answer.22378,
  A26=d2_03w$Answer.22379,
  A27=d2_03w$Answer.22380,
  A28=d2_03w$Answer.22381,
  A34=d2_03w$Answer.22386,
  A57=d2_03w$Answer.22393,
  A58=d2_03w$Answer.22394,
  A59=d2_03w$Answer.22395,
  A60=d2_03w$Answer.22396,
  A64=d2_03w$Answer.22397,
  A68=d2_03w$Answer.22398,
  Assist=d2_03w$Answer.22405,
  Pill=d2_03w$Answer.22406,
  Muse=d2_03w$Answer.22407,
  PIT=d2_03w$Answer.22408,
  Vac=d2_03w$Answer.22409,
  Other=d2_03w$Answer.22410,
  Race=NA,
  Marital=NA,
  Diabetes=NA,
  HeartD=NA
  
)
d2_06 = data.frame(
  Phase=2,
  Time=6,
  Study_ID=d2_06w$Study_ID,
  DoS=d2_06w$Surgery_date.22451,
  A23=d2_06w$Answer.22451,
  A26=d2_06w$Answer.22452,
  A27=d2_06w$Answer.22453,
  A28=d2_06w$Answer.22454,
  A34=d2_06w$Answer.22459,
  A57=d2_06w$Answer.22466,
  A58=d2_06w$Answer.22467,
  A59=d2_06w$Answer.22468,
  A60=d2_06w$Answer.22469,
  A64=d2_06w$Answer.22470,
  A68=d2_06w$Answer.22471,
  Assist=d2_06w$Answer.22478,
  Pill=d2_06w$Answer.22479,
  Muse=d2_06w$Answer.22480,
  PIT=d2_06w$Answer.22481,
  Vac=d2_06w$Answer.22482,
  Other=d2_06w$Answer.22483,
  Race=NA,
  Marital=NA,
  Diabetes=NA,
  HeartD=NA
  
)
d2_12 = data.frame(
  Phase=2,
  Time=12,
  Study_ID=d2_12w$Study_ID,
  DoS=d2_12w$Surgery_date.22524,
  A23=d2_12w$Answer.22524,
  A26=d2_12w$Answer.22525,
  A27=d2_12w$Answer.22526,
  A28=d2_12w$Answer.22527,
  A34=d2_12w$Answer.22532,
  A57=d2_12w$Answer.22539,
  A58=d2_12w$Answer.22540,
  A59=d2_12w$Answer.22541,
  A60=d2_12w$Answer.22542,
  A64=d2_12w$Answer.22543,
  A68=d2_12w$Answer.22544,
  Assist=d2_12w$Answer.22551,
  Pill=d2_12w$Answer.22552,
  Muse=d2_12w$Answer.22553,
  PIT=d2_12w$Answer.22554,
  Vac=d2_12w$Answer.22555,
  Other=d2_12w$Answer.22556,
  Race=NA,
  Marital=NA,
  Diabetes=NA,
  HeartD=NA
)

# Bind all PROM files together into long format by time
d = rbind(
  d1_00,
  d1_01,
  d1_03,
  d1_06,
  d1_12,
  d2_00,
  d2_01,
  d2_03,
  d2_06,
  d2_12
)

# Load the consent file and demographic file to link the participant
# IDs and ensure they gave consent
Consent <- read.csv("../Data/Consent.csv")
Demographics <- read.csv("../Data/Patient Demographics.csv")
merged = merge(Consent, Demographics, by="Patient_No")
consented = data.frame(merged$Study_ID, merged$Birth_Date)
colnames(consented) = c("Study_ID", "DoB")

# Merge the consented participants with the data we have
d = merge(consented, d, by="Study_ID")
# We are now doubly sure that all included patients had given consent

# Read in the PROMs and prepare some columns for coded PROMs
# 'AX' contains the response to question 'X' in words
# 'CX' contains the EPIC coded value for response 'X'
d['C23']=NA
d['C26']=NA
d['C27']=NA
d['C28']=NA
d['C57']=NA
d['C58']=NA
d['C59']=NA
d['C60']=NA
d['C64']=NA
d['C68']=NA

# Convert PROMs to their EPIC-26 value
# This is another long manual task
# There will be a better way of doing this, but I didn't find it
d$C23[d$A23=='More than once a week'&!is.na(d$A23)] = 0
d$C23[d$A23=='About once a week'&!is.na(d$A23)] = 25
d$C23[d$A23=='More than once a day'&!is.na(d$A23)] = 50
d$C23[d$A23=='About once a day'&!is.na(d$A23)] = 75
d$C23[d$A23=='Rarely or never'&!is.na(d$A23)] = 100

d$C26[d$A26=='No urinary control whatsoever'&!is.na(d$A26)] = 0
d$C26[d$A26=='Frequent dribbling'&!is.na(d$A26)] = 33
d$C26[d$A26=='Occasional dribbling'&!is.na(d$A26)] = 67
d$C26[d$A26=='Total control'&!is.na(d$A26)] = 100

d$C27[d$A27=='3 or more pads per day'&!is.na(d$A27)] = 0
d$C27[d$A27=='2 pads per day'&!is.na(d$A27)] = 33
d$C27[d$A27=='1 pad per day'&!is.na(d$A27)] = 67
d$C27[d$A27=='None'&!is.na(d$A27)] = 100

d$C28[d$A28=='Big Problem'&!is.na(d$A28)] = 0
d$C28[d$A28=='Moderate Problem'&!is.na(d$A28)] = 25
d$C28[d$A28=='Small Problem'&!is.na(d$A28)] = 50
d$C28[d$A28=='Very Small Problem'&!is.na(d$A28)] = 75
d$C28[d$A28=='No Problem'&!is.na(d$A28)] = 100

d$C57[d$A57=='Very Poor to None'&!is.na(d$A57)] = 0
d$C57[d$A57=='Very Poor'&!is.na(d$A57)] = 0
d$C57[d$A57=='Poor'&!is.na(d$A57)] = 25
d$C57[d$A57=='Fair'&!is.na(d$A57)] = 50
d$C57[d$A57=='Good'&!is.na(d$A57)] = 75
d$C57[d$A57=='Very Good'&!is.na(d$A57)] = 100

d$C58[d$A58=='Very Poor to None'&!is.na(d$A58)] = 0
d$C58[d$A58=='Very Poor'&!is.na(d$A58)] = 0
d$C58[d$A58=='Poor'&!is.na(d$A58)] = 25
d$C58[d$A58=='Fair'&!is.na(d$A58)] = 50
d$C58[d$A58=='Good'&!is.na(d$A58)] = 75
d$C58[d$A58=='Very Good'&!is.na(d$A58)] = 100

d$C59[d$A59=='None at all'&!is.na(d$A59)] = 0
d$C59[d$A59=='Not firm enough for any sexual activity'&!is.na(d$A59)] = 33
d$C59[d$A59=='Firm enough for masturbation and foreplay only'&!is.na(d$A59)] = 67
d$C59[d$A59=='Firm enough for intercourse'&!is.na(d$A59)] = 100

d$C60[d$A60=='I NEVER had an erection when I wanted one'&!is.na(d$A60)] = 0
d$C60[d$A60=='I had an erection LESS THAN HALF the time I wanted one'&!is.na(d$A60)] = 25
d$C60[d$A60=='I had an erection ABOUT HALF the time I wanted one'&!is.na(d$A60)] = 50
d$C60[d$A60=='I had an erection MORE THAN HALF the time I wanted one'&!is.na(d$A60)] = 75
d$C60[d$A60=='I had an erection WHENEVER I wanted one'&!is.na(d$A60)] = 100

d$C64[d$A64=='Very poor'&!is.na(d$A64)] = 0
d$C64[d$A64=='Poor'&!is.na(d$A64)] = 25
d$C64[d$A64=='Fair'&!is.na(d$A64)] = 50
d$C64[d$A64=='Good'&!is.na(d$A64)] = 75
d$C64[d$A64=='Very good'&!is.na(d$A64)] = 100

d$C68[d$A68=='Big problem'&!is.na(d$A68)] = 0
d$C68[d$A68=='Moderate problem'&!is.na(d$A68)] = 25
d$C68[d$A68=='Small problem'&!is.na(d$A68)] = 50
d$C68[d$A68=='Very small problem'&!is.na(d$A68)] = 75
d$C68[d$A68=='No problem'&!is.na(d$A68)] = 100

# Calculate the EPIC-26 score and find the number missing
Uscore = rowMeans(d[c("C23","C26","C27","C28")], na.rm=T)
Sscore = rowMeans(d[c("C57", "C58", "C59", "C60", "C64", "C68")], na.rm=T)
Umiss = rowSums(is.na(d[c("C23","C26","C27","C28")]))
Smiss = rowSums(is.na(d[c("C57", "C58", "C59", "C60", "C64", "C68")]))

# Urinary score cannot have any missing
# Sexual score can have 1 missing
Uscore[Umiss>0]=NA
Sscore[Smiss>1]=NA
d['Umiss']=Umiss
d['Smiss']=Smiss
d['Uscore']=Uscore
d['Sscore']=Sscore

# Convert questions to factors, this will help in building tables later
d$A23 = factor(d$A23, levels = c("Rarely or never", "About once a week", "More than once a week", "About once a day", "More than once a day"))
d$A26 = factor(d$A26, levels = c("Total control", "Occasional dribbling", "Frequent dribbling", "No urinary control whatsoever"))
d$A27 = factor(d$A27, levels = c("None", "1 pad per day", "2 pads per day", "3 or more pads per day"))
d$A28 = factor(d$A28, levels = c("No Problem", "Very Small Problem", "Small Problem", "Moderate Problem", "Big Problem"))
d$A34 = factor(d$A34, levels = c("No problem", "Very small problem", "Small problem", "Moderate problem", "Big problem")) 
# Notice that some are capitalised and some are not. This caused a lot of confusion.
d$A57 = factor(d$A57, levels = c("Very Good", "Good", "Fair", "Poor", "Very Poor", "Very Poor to None"))
d$A58 = factor(d$A58, levels = c("Very Good", "Good", "Fair", "Poor", "Very Poor", "Very Poor to None"))
d$A59 = factor(d$A59, levels = c("Firm enough for intercourse", "Firm enough for masturbation and foreplay only", "Not firm enough for any sexual activity", "None at all"))
d$A60 = factor(d$A60, levels = c("I had an erection WHENEVER I wanted one", "I had an erection MORE THAN HALF the time I wanted one", "I had an erection ABOUT HALF the time I wanted one", "I had an erection LESS THAN HALF the time I wanted one", "I NEVER had an erection when I wanted one"))
d$A64 = factor(d$A64, levels = c("Very good", "Good", "Fair", "Poor", "Very poor"))
d$A68 = factor(d$A68, levels = c("No problem", "Very small problem", "Small problem", "Moderate problem", "Big problem"))

# For medication and device usage, the same responses are always used
Use = c(
  "Have not tried it",
  "It helped and I use it always",
  "It helped and I use it sometimes",
  "It helped, but I am not using it now",
  "Tried it, but it was not helpful"
)
d$Pill = factor(d$Pill, Use)
d$Muse = factor(d$Muse, Use)
d$PIT = factor(d$PIT, Use)
d$Vac = factor(d$Vac, Use)
d$Other = factor(d$Other, Use)
d$Assist = factor(d$Assist, levels=c("No", "Yes"))



l=d

# Load the clinical data and the variables we need
baus = read.csv("../Data/BAUS.csv", header=T)
baus_w = baus[c("Study_ID","Question", "Answer")]
baus_w = reshape(baus_w, idvar="Study_ID", timevar="Question", direction="wide")
baus_w = data.frame(
  "Study_ID"=baus_w[1],
  "PSA"=as.numeric(unlist(baus_w[4])),
  "GS"=unlist(baus_w[7]),
  "NSA"=unlist(baus_w[11]),
  "Lymph"=unlist(baus_w[10]),
  "Stage"=unlist(baus_w[5]),
  "Approach"=unlist(baus_w[9]),
  "DAmico"=unlist(baus_w[8])
)
baus_w["Clinical_data"]=1 # To indicate if clinical data was available
baus_w$GS = factor(baus_w$GS)
baus_w$NSA = factor(baus_w$NSA, 
                    levels=c(
                      "None", "Unilateral", "Bilateral"
                    ))
baus_w$Lymph = factor(baus_w$Lymph)
baus_w$DAmico[baus_w$DAmico=="high"]="High"
baus_w$DAmico = factor(baus_w$DAmico,
                       levels=c(
                         "Low", "Intermediate", "High"
                       ))
# Load PROM data and convert to wide
w = d[order(d$Time),]
w = reshape(w, timevar="Time", idvar="Study_ID", direction="wide")

# Load date of births from clinical data for those missing from PROMs
baus_dob = data.frame("Study_ID"=baus$Study_ID, "DoBB"=baus$Birth_Date)
baus_dob = baus_dob[!duplicated(baus_dob),]
baus_w = merge(baus_w, baus_dob, by="Study_ID")
w = merge(w, baus_w, by="Study_ID", all.x=T)

# Coalesce date of births and surgeries
# DoB and DoS was recorded at each time point, so some will be missing
# This allows us to look at each time point to find if at least of
# the columns has the information, even if it is mising in others
dob = coalesce(w$DoB.0, w$DoB.1, w$DoB.3, w$DoB.6, w$DoB.12)
dos = coalesce(w$DoS.0, w$DoS.1, w$DoS.3, w$DoS.6, w$DoS.12)

# Convert NULL to NA
dos[dos=="NULL"]=NA

# There are two formats used for date which can cause problems
dos1 = as.Date(dos, format="%d/%m/%Y")
dos2 = as.Date(dos, format="%Y-%m-%d")
dos = coalesce(dos1, dos2)

# Add these converted dates to the dataset and check they make sense 
w["DoB"] = as.Date(dob)
w["DoS"] = as.Date(dos)
w$DoB[as.Date(w$DoB)<as.Date("1900-01-01")]=NA
w$DoB[as.Date(w$DoB)>as.Date("2000-01-01")]=NA
w$DoS[as.Date(w$DoS)<as.Date("2015-01-01")]=NA

# Marker for clinical varibales
w$Clinical_data[is.na(w$Clinical_data)]=0

# Calculate the age at surgery
# Age is calculated in days and then divided by 365.25 (to account for leap years)
w["Age"] = as.numeric((as.Date(w$DoS)-as.Date(w$DoB))/365.25)

# Remove the columns we no longer need
w = subset(w, select = -c(
  DoB.0,DoB.1,DoB.3,DoB.6,DoB.12,DoBB,
  DoS.0,DoS.1,DoS.3,DoS.6,DoS.12,
  Phase.0,Phase.1,Phase.3,Phase.6,Phase.12 
))

# Remove those with no PROM data
rem = w[rowSums(is.na(w))>=175, ]
w = w[which(!w$Study_ID %in% rem$Study_ID),]
l = l[which(!l$Study_ID %in% rem$Study_ID),]


save(l, w, file="data.RData")
