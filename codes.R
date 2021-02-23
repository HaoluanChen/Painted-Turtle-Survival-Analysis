library(readxl)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(GGally)
library(marked)
library(mice)


## Data Cleaning
data1 <- data %>% 
  select("notch", "Year", 'age',
         "sex", "date", "Julian date", "site", "gravid", "nest",
         "nestdate", "secnest", "nest2dat", "firstcap", "mass", "cl",
         "pl", "midcl", "midpl", "lclaw", "rclaw", "cw", "ch", "dead") %>% 
  filter(site == "Arowhon Rd." | site == "Sims Creek" | site == "Maiden Lake"
         | site ==  "Road to WHP" | site == "Blanding's Bog" 
         | site == "Wolf Howl Pond" | site == "Wolf Howl Pond E." 
         | site == "Road to Wrose" | site == "West Rose" 
         | site == "March Hare Lake" | site == "Wolf Howl Pond e.") %>% 
  mutate(site = ifelse(site == "Wolf Howl Pond e.","Wolf Howl Pond E.", site ))

data1 <- data1 %>% 
  mutate(mature = ifelse(sex == "Male" & pl > 9, TRUE, FALSE),
         mature = ifelse(sex == "Female" & pl > 12, TRUE, FALSE),
         mass = ifelse(mass == -47, NA, mass),
         firstcapyear = ifelse(firstcap == TRUE, Year,0))
#data1 is cleaned data


## Capture History 
turtles <- data %>% select(notch, Year) %>% filter(Year >= 1991)
turtles %>% distinct(Year)
history <-  turtles %>% mutate(capture = 1) %>% 
  complete(notch, Year) %>% 
  mutate(capture = !is.na(capture),
         capture = ifelse(capture == T, 1, 0)) %>% 
  group_by(notch) %>% 
  summarise(ch = paste(capture,collapse = ""))

## Data Manupliation and Transformation
datacov <- data1 %>% select(notch, Year, sex, midpl, dead) # select wanted variables
datawithcapyear <- data1 %>% select(notch, firstcapyear) %>% filter(firstcapyear != 0)

data2 <- full_join(datacov, history, by = "notch") #joining wanted variables and capture history 

# cleaning and transforming midpl variable
data2 <- data2 %>% filter(!is.na(midpl)) 
midpl <- data2 %>% group_by(notch) %>% spread(Year, midpl)
midpl <- full_join(midpl, datawithcapyear, by = "notch") %>% filter(!is.na(firstcapyear))


# imputation
year = 1991
column = 5
for(c in column:31) {
  for (i in 1:nrow(midpl)){
    if (midpl[i,32] > year){ 
      midpl[i, c] = 0 
    }
    # impute by assumping each year turtles' midpl grow by 0.15 
    else if (is.na(midpl[i,c])){
      if (year > 1991 && midpl[i,32] < year){
        midpl[i,c] = midpl[i, c-1] + 0.15
      }
    }
    
    
    # impute by previous observation
    # if(year > 1991 && is.na(testdata[i,c])){
    #   testdata[i,c] = testdata[1, c-1]
    # }
  }
  year = year + 1 
  
}
midpl

# renaming the columns
midpl <- rename(midpl, c("midpl1" = "1991", "midpl2" = "1992", "midpl3" = "1993", "midpl4" = "1994", "midpl5" = "1995", "midpl6" = "1996", "midpl7" = "1997", "midpl8" = "1998", "midpl9" = "1999", "midpl10" = "2000", "midpl11" = "2001", "midpl12" = "2002", "midpl13" = "2003", "midpl14" = "2004", "midpl15" = "2005", "midpl16" = "2006", "midpl17" = "2007", "midpl18" = "2008", "midpl19" = "2009", "midpl20" = "2010", "midpl21" = "2011", "midpl22" = "2012", "midpl23" = "2013", "midpl24" = "2014", "midpl25" = "2015","midpl26" = "2016", "midpl27" = "2017"))

midpl <- na.omit(midpl)


# cleaning and transforming mass variable

mass <- data1 %>% select(notch, mass, Year) %>% filter(!is.na(mass), Year >= 1991) %>%
  group_by(notch) %>% spread(Year, mass)
mass <- full_join(mass, datawithcapyear) %>% filter(!is.na(firstcapyear)) 

year = 1991
column = 2
for(c in column:28) {
  for (i in 1:nrow(mass)){
    if (mass[i,29] > year){ 
      mass[i, c] = 0 
    }
    # impute by assumping each year turtles' mass grow by 8.9
    else if (is.na(mass[i,c])){
      if (year > 1991 && mass[i,29] < year){
        mass[i,c] = mass[i, c-1] + 8.9
      }
    }
    
    # impute by previous observation
    # if(year > 1991 && is.na(mass[i,c])){
    #   mass[i,c] = mass[1, c-1]
    # }
  }
  year = year + 1 
  
}

# renaming the columns 
massdata <- rename(mass, c("mass1" = "1991", "mass2" = "1992", "mass3" = "1993", "mass4" = "1994", "mass5" = "1995", "mass6" = "1996", "mass7" = "1997", "mass8" = "1998", "mass9" = "1999", "mass10" = "2000", "mass11" = "2001", "mass12" = "2002", "mass13" = "2003", "mass14" = "2004", "mass15" = "2005", "mass16" = "2006", "mass17" = "2007", "mass18" = "2008", "mass19" = "2009", "mass20" = "2010", "mass21" = "2011", "mass22" = "2012", "mass23" = "2013", "mass24" = "2014", "mass25" = "2015","mass26" = "2016", "mass27" = "2017"))
massdata <- na.omit(massdata)

# Joing cleaned and transformed mass data and midpl data
cjsdata <- inner_join(midpl,massdata, by = "notch")
cjsdata


## Model 1 
model=crm(cjsdata)

# Process data
turtles.proc = process.data(cjsdata)
turtles.proc

# Create design data with static and time varying covariates
design.Phi=list(static=c("sex"))
design.p=list(static=c("sex")) 

design.parameters=list(Phi=design.Phi,p=design.p)
ddl=make.design.data(turtles.proc,parameters=design.parameters) 
names(ddl$Phi)
names(ddl$p)

Phi.sfw=list(formula=~sex)
p.ast=list(formula=~sex)
model=crm(turtles.proc,ddl,hessian=TRUE,model.parameters=list(Phi=Phi.sfw,p=p.ast))
model$results
