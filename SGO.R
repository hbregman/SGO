###Read in CSV
SGO <- read.csv("/Users/halliebregman/Desktop/SGO2.csv")
###Remove identifying info
SGO <- SGO[c(-1:-9)]
names(SGO)

head(SGO[37:48], 50)
table(SGO[48])

###Rename shorter names
names(SGO) <- c("Hear_about", "Hear_about_other", "Events_Attended", "Inconvenient_locations", 
                "Too_early", "Too_late", "Too_expensive", "X.4", "Too_busy", "Not_attended_other", 
                "Satisfaction", "Recommend", "Recommend_Open_ended", "raffle_prizes", "speakers", 
                "food", "networking", "drinks", "recruiting", "Membership", "Not_member_why_not", "Too_expensive_member", "Dont_like_benefits",
                "Dont_want_to_attend_more_than_one_event_month", "Dont_like_events", "Dont_like_raffle", "Not_member_other", 
                "Connection", "Pay_SGO", "Pay_Special_Events", "Pay_Prof_Dev", "Future_open", "Future_topics", "Improve",
                "Do_well", "Geek_out", "SGO_monthly", "Book_club", "Food/wine", "Happy_hours", "Walking_tours", "Educational_workshops",
                "Movies", "Content_specific", "Karaoke",
                "Speed_recruit", "Job_fairs", "Mentorship", "Men", "Number_of_eevnts_attend", "Coding", "Sciences", "Marketing_Sales", 
                "UX/Design", "Entrepreneurship", "Women_in_tech", "Prof_Dev", "Personal_growth", "Downtown_Boston", "Cambridge", "Somerville", 
                "Brookline", "SouthBoston", "Newton", "Waltham", "Worchester", "Other", "Car", "Bus", "Uber", "Train/CommuterRail", "Subway",
                "Cab", "Ferry", "Other_transport", "Wine", "Beer", "Liquor", "Soda", "Juice", "Water", "Other_drink", "Gluten-free", "Nuts", "Dairy",
                "Vegetarian", "Vegan", "Kosher", "No_Allergies", "Other_allgeries", "Market_Research", "Software", "Hardware", "Advertising", "SaaS",
                "Consumer", "Healthcare", "Biotech", "Student", "Other_industry", "Marketing", "Data_Science", "Product", "Sales", "Front_End", "Hardware", "Back_end",
                "Founder_CEO", "Finance_Accounting", "HR", "UX", "Student2", "Other_role", "Current_work", "Company_Name",
                "Currently_looking", "Hiring", "SGO_Hiring", "Age", "Neighborhood", "Neighborhood_Other", "american_indian/alaskan_native",
                "Asian/pacific_islander", "Black/African_american", "Hispanic", "White/Caucasian", "Prefer_not_to_answer", "Multiple/Other", "Income")

###Set color scheme
colors <- c("darkred","red", "tomato", "coral", "violetred", "pink", "violetred4")

par(mfrow=c(3,1))
# Simple Horizontal Bar Plot with Added Labels 
barplot(table(SGO$Hear_about), main="Heard About SGO", horiz=TRUE,
        names.arg=c("BostInno", "Evenbrite", "Friend/Colleague", "Other"), col=colors, xlim=c(0, 100))

###Crosstabs for Referral source and attendance
barplot(table(SGO$Hear_about, SGO$Events_Attended), col=colors
        , legend = rownames(table(SGO$Hear_about, SGO$Events_Attended)),
        ylim=c(0, 100), main="Heard about SGO vs. Attendance")

###Crosstabs for Satisfaction by Referral Source Scaled to 100% stacked bars
barplot(prop.table(table(SGO$Satisfaction[which(SGO$Events_Attended=="One" 
             | SGO$Events_Attended=="More than one")], 
              SGO$Hear_about[which(SGO$Events_Attended=="One" 
            | SGO$Events_Attended=="More than one")]), 2) * 100, 
        col=colors, legend = rownames(table(SGO$Satisfaction, SGO$Hear_about)),
        ylim=c(0, 100), main="For those who have attended at least one event, Satisfaction by Referral Source")

###Crosstabs for Satisfaction by Referral Source Scaled to 100% stacked bars
par(mfrow=c(2,1))
barplot(table(SGO$Satisfaction[which(SGO$Software=="Software")]), main="Satisfaction by Industry", horiz=TRUE,
        names.arg=rownames(SGO$Satisfaction), col=colors, xlim=c(0, 100))
barplot(table(SGO$Satisfaction[which(SGO$Market_Research=="Market Research")]), main="Satisfaction by Industry", horiz=TRUE,
        names.arg=rownames(SGO$Satisfaction), col=colors, xlim=c(0, 100))

###variables of interest: industry, position, looking, company name, job fairs, locations###
###Convert to dummies for plotting
SGO$Marketing <- model.matrix( ~ Marketing - 1, data=SGO )[,2]
SGO$Data_Science <- model.matrix( ~ Data_Science - 1, data=SGO )[,2]
SGO$Product <- model.matrix( ~ Product - 1, data=SGO )[,2]
SGO$Sales <- model.matrix( ~ Sales - 1, data=SGO )[,2]
SGO$Front_End <- model.matrix( ~ Front_End - 1, data=SGO )[,2]
SGO$Hardware <- model.matrix( ~ Hardware - 1, data=SGO )[,2]
SGO$Back_end <- model.matrix( ~ Back_end - 1, data=SGO )[,2]
SGO$Founder_CEO <- model.matrix( ~ Founder_CEO - 1, data=SGO )[,2]
SGO$Finance_Accounting <- model.matrix( ~ Finance_Accounting - 1, data=SGO )[,2]
SGO$HR <- model.matrix( ~ HR - 1, data=SGO )[,2]
SGO$UX <- model.matrix( ~ UX - 1, data=SGO )[,2]
SGO$Student2 <- model.matrix( ~ Student2 - 1, data=SGO )[,2]

###String together list of jobs
jobs <- list(marketing = SGO$Marketing, data_science = SGO$Data_Science, product= SGO$Product, sales= SGO$Sales, 
             front_end=SGO$Front_End, hardware=SGO$Hardware, back_end=SGO$Back_end, founder_ceo=SGO$Founder_CEO, 
             finance_accounting = SGO$Finance_Accounting, hr=SGO$HR, UX=SGO$UX, student=SGO$Student2)
job_roles <- unlist(lapply(jobs, mean, USE.NAMES=TRUE))

###Crosstabs for Job Roles
par(mar=c(3, 3, 3, 3))
barplot(job_roles, main="Job Roles", horiz=TRUE,
        names.arg=c("Marketing", "Data Science", "Product", "Sales", "Front End", "Hardware", "Back End", "Founder/CEO", "Finance/Accounting", 
                    "HR", "UX", "Student"), col=colors, xlim=c(0, .2))

###Convert to dummies for plotting
SGO$Market_Research <- model.matrix( ~ Market_Research - 1, data=SGO )[,2]
SGO$Software <- model.matrix( ~ Software - 1, data=SGO )[,2]
SGO$Hardware <- model.matrix( ~ Hardware - 1, data=SGO )[,1]
SGO$Advertising <- model.matrix( ~ Advertising - 1, data=SGO )[,2]
SGO$SaaS <- model.matrix( ~ SaaS - 1, data=SGO )[,2]
SGO$Consumer <- model.matrix( ~ Consumer - 1, data=SGO )[,2]
SGO$Healthcare <- model.matrix( ~ Healthcare - 1, data=SGO )[,2]
SGO$Biotech <- model.matrix( ~ Biotech - 1, data=SGO )[,2]
SGO$Student <- model.matrix( ~ Student - 1, data=SGO )[,2]

###String together list of industries
industries <- list(Market_Research = SGO$Market_Research, Software = SGO$Software, Hardware= SGO$Hardware, Advertising= SGO$Advertising, 
             SaaS=SGO$SaaS, Consumer=SGO$Consumer, Healthcare=SGO$Healthcare, Biotech=SGO$Biotech, 
             Student = SGO$Student)
industries <- unlist(lapply(industries, mean, USE.NAMES=TRUE))

###Crosstabs for Industry
par(mfrow=c(1,1))
par(mar=c(3, 3, 3, 3))
barplot(industries, main="Job Roles", horiz=TRUE,
        names.arg=c("Market Research", "Software", "Hardware", "Advertising", "SaaS", "Consumer", "Healthcare", 
                    "Biotech", "Student"), col=colors, xlim=c(0, .3))

###Crosstabs for Hiring
barplot(table(SGO$Currently_looking[which(SGO$Currently_looking!="")]), main="Looking for a job", horiz=TRUE,
        names.arg=rownames(SGO$Currently_looking[which(SGO$Currently_looking!="")]), col=colors, xlim=c(0, 100))

###Industry by Hiring
par(mfrow=c(4, 2))
barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Software[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Software[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="Software Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Healthcare[which(SGO$Currently_looking!="")]), 2) * 100, col=colors,
        ylim=c(0, 100), main="Healthcare Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Market_Research[which(SGO$Currently_looking!="")]), 2) * 100, col=colors,
        ylim=c(0, 100), main="Market_Research Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$SaaS[which(SGO$Currently_looking!="")]), 2) * 100, col=colors,
        ylim=c(0, 100), main="SaaS Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Consumer[which(SGO$Currently_looking!="")]), 2) * 100, col=colors,
        ylim=c(0, 100), main="Consumer Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Advertising[which(SGO$Currently_looking!="")]), 2) * 100, col=colors,
        ylim=c(0, 100), main="Advertising Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Biotech[which(SGO$Currently_looking!="")]), 2) * 100, col=colors,
        ylim=c(0, 100), main="Biotech Currently_looking")

###Job Role by Hiring
par(mfrow=c(4, 3))
barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Marketing[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Marketing[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="Marketing Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Data_Science[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Data_Science[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="Data_Science Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Product[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Product[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="Product Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Sales[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Sales[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="Sales Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Front_End[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Front_End[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="Front_End Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Hardware[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Hardware[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="Hardware Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Back_end[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Back_end[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="Back_end Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Founder_CEO[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Founder_CEO[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="Founder_CEO Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Finance_Accounting[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Finance_Accounting[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="Finance_Accounting Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$HR[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$HR[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="HR Currently_looking")

barplot(prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$UX[which(SGO$Currently_looking!="")]), 2) * 100, col=colors
        , legend = rownames(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$UX[which(SGO$Currently_looking!="")])),
        ylim=c(0, 100), main="UX Currently_looking")

###Tables
prop.table(table(SGO$Hear_about))
prop.table(table(SGO$Events_Attended))
prop.table(table(SGO$Inconvenient_locations))
prop.table(table(SGO$Satisfaction))
prop.table(table(SGO$Job_fairs))
prop.table(table(SGO$Speed_recruit))
prop.table(table(SGO$Currently_looking[SGO$Currently_looking!=""]))
prop.table(table(SGO$Hiring))
prop.table(table(SGO$SGO_Hiring))
prop.table(table(SGO$Marketing))
prop.table(table(SGO$Data_Science))
prop.table(table(SGO$Product))
prop.table(table(SGO$Sales))
prop.table(table(SGO$Front_End))
prop.table(table(SGO$Hardware))
prop.table(table(SGO$Back_end))
prop.table(table(SGO$Founder_CEO))
prop.table(table(SGO$Finance_Accounting))
prop.table(table(SGO$HR))
prop.table(table(SGO$UX))
prop.table(table(SGO$Student2))
prop.table(table(SGO$Software))
prop.table(table(SGO$Hardware))
prop.table(table(SGO$Market_Research))
prop.table(table(SGO$Advertising))
prop.table(table(SGO$SaaS))
prop.table(table(SGO$Consumer))
prop.table(table(SGO$Healthcare))
prop.table(table(SGO$Biotech))
prop.table(table(SGO$Student))

prop.table(table(SGO$Age[SGO$Age!=""]))
prop.table(table(SGO$Income[SGO$Income!=""]))
prop.table(table(SGO$Neighborhood[SGO$Neighborhood!=""]))

prop.table(table(SGO$Hear_about, SGO$Events_Attended), 1)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Events_Attended[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Events_Attended[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Speed_recruit, SGO$Job_fairs), 1)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Software[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Hardware[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Market_Research[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Advertising[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$SaaS[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Consumer[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Healthcare[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Biotech[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Student[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Marketing[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Data_Science[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Product[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Sales[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$UX[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$HR[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Finance_Accounting[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Back_end[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Front_End[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Founder_CEO[which(SGO$Currently_looking!="")]), 2)
prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Student2[which(SGO$Currently_looking!="")]), 2)

prop.table(table(SGO$Currently_looking[which(SGO$Currently_looking!="")], SGO$Job_fairs[which(SGO$Currently_looking!="")]), 2)

prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Software[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Hardware[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Market_Research[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Advertising[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$SaaS[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Consumer[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Healthcare[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Biotech[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Student[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Marketing[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Data_Science[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Product[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Sales[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$UX[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$HR[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Finance_Accounting[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Back_end[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Front_End[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Founder_CEO[which(SGO$Job_fairs!="")]), 2)
prop.table(table(SGO$Job_fairs[which(SGO$Job_fairs!="")], SGO$Student2[which(SGO$Job_fairs!="")]), 2)


#Proportions for Internal 
prop.table(table(SGO$Hear_about[which(SGO$Hear_about!="")]))
prop.table(table(SGO$`White/Caucasian`))
prop.table(table(SGO$`Black/African_american`))
prop.table(table(SGO$`Hispanic`))
prop.table(table(SGO$`Multiple/Other`))
prop.table(table(SGO$`Asian/pacific_islander`))
prop.table(table(SGO$`american_indian/alaskan_native`))

table(SGO$Hear_about_other)
prop.table(table(SGO$Events_Attended[which(SGO$Events_Attended!="")]))
prop.table(table(SGO$Events_Attended[which(SGO$Events_Attended!="")], SGO$Hear_about[which(SGO$Events_Attended !="")]), 2)
prop.table(table(SGO$Satisfaction[which(SGO$Satisfaction!="")], SGO$Hear_about[which(SGO$Satisfaction !="")]), 2)
prop.table(table(SGO$`Inconvenient_locations`[which(SGO$Events_Attended=="None")]))
prop.table(table(SGO$`Too_early`[which(SGO$Events_Attended=="None")]))
prop.table(table(SGO$`Too_late`[which(SGO$Events_Attended=="None")]))
prop.table(table(SGO$`Too_expensive`[which(SGO$Events_Attended=="None")]))
prop.table(table(SGO$`Too_busy`[which(SGO$Events_Attended=="None")]))

names(SGO)
# Prepare Data
myvars <- c("Age", "Income", "UX", "Marketing", "Data_Science",
            "Software", "Healthcare", "Currently_looking")
newdata <- as.data.frame(SGO[myvars])

names(newdata)

#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(newdata$Marketing)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(newdata$Marketing == level, 1, 0)
}
for(level in unique(newdata$Age)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(newdata$Age == level, 1, 0)
}
for(level in unique(newdata$Income)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(newdata$Income == level, 1, 0)
}
for(level in unique(newdata$UX)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(newdata$UX == level, 1, 0)
}
for(level in unique(newdata$Software)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(newdata$Software == level, 1, 0)
}
for(level in unique(newdata$Healthcare)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(newdata$Healthcare == level, 1, 0)
}
for(level in unique(newdata$Currently_looking)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(newdata$Currently_looking == level, 1, 0)
}
for(level in unique(newdata$Data_Science)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(newdata$Data_Science == level, 1, 0)
}
names(newdata)
newdata <- newdata[names(newdata[9:32])]
c <- cor(newdata)
library(corrplot)
corrplot(c, tl.pos = "d")

#SGOC <- scale(newdata) # standardize variables
# Determine number of clusters
wss <- (nrow(newdata)-1)*sum(apply(newdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(newdata, 
                                     centers=i)$withinss)
par(mfrow=c(1, 1))
par(mar=c(4, 4, 4, 4))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
set.seed(101)
fit2 <- kmeans(newdata, 2) # 5 cluster solution
fit3 <- kmeans(newdata, 3) # 5 cluster solution
fit4 <- kmeans(newdata, 4) # 5 cluster solution
fit5 <- kmeans(newdata, 5) # 5 cluster solution
fit6 <- kmeans(newdata, 6) # 5 cluster solution

# get cluster means 
summs <- aggregate(newdata,by=list(fit3$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(SGO, fit3$cluster)
mydata <- data.frame(mydata, fit4$cluster)
head(mydata)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
par(mfrow=c(1, 1))
clusplot(newdata, fit2$cluster, color=TRUE, shade=TRUE, 
         labels=0, lines=0)
clusplot(newdata, fit3$cluster, color=TRUE, shade=TRUE, 
         labels=0, lines=0)
clusplot(newdata, fit4$cluster, color=TRUE, shade=TRUE, 
         labels=0, lines=0)
clusplot(newdata, fit5$cluster, color=TRUE, shade=TRUE, 
         labels=0, lines=0)
clusplot(newdata, fit6$cluster, color=TRUE, shade=TRUE, 
         labels=0, lines=0)

par(mfrow=c(1, 1))
plot(summs$Group.1, summs$dummy_Marketing, ylim=c(0, .45), type='l', col="red")
lines(summs$Group.1, summs$`dummy_User Experience`, col="blue")
lines(summs$Group.1, summs$`dummy_Data Science`, col="green")
lines(summs$Group.1, summs$dummy_Software, col="black")
lines(summs$Group.1, summs$dummy_Healthcare, col="orange")
legend("topright", inset=c(-0.6,-.5), c("Marketing", "UX", "Data Science", "Software", "Healthcare"), 
       pch=1, col=c("red", "blue", "green", "black", "orange"))

par(mar=c(5, 5, 5, 5))
par(xpd=TRUE)
plot(summs$Group.1, summs$`dummy_18 to 24`, ylim=c(0, .6), type='l', col="red")
lines(summs$Group.1, summs$`dummy_25 to 34`, col="blue")
lines(summs$Group.1, summs$`dummy_35 to 44`, col="green")
lines(summs$Group.1, summs$`dummy_45 to 54`, col="black")
lines(summs$Group.1, summs$`dummy_55 to 64`, col="orange")
legend("topright", inset=c(-0.5,0), c("18-24", "25-34", "35-44", "45-54", "55-64"), 
      pch=1, col=c("red", "blue", "green", "black", "orange"))

par(mar=c(5, 5, 5, 5))
par(xpd=TRUE)
plot(summs$Group.1, summs$dummy_No, ylim=c(0, 1), type='l', col="red")
lines(summs$Group.1, summs$`dummy_Yes; actively looking`, col="blue")
lines(summs$Group.1, summs$`dummy_Yes; casually looking`, col="green")
legend("topright", inset=c(-0.5,-.5), c("Not Looking", "Actively Looking", "Casually Looking"), 
       pch=1, col=c("red", "blue", "green"))

for(level in unique(SGO$Satisfaction)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(SGO$Satisfaction == level, 1, 0)
}
for(level in unique(SGO$Recommend)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(SGO$Recommend == level, 1, 0)
}
for(level in unique(SGO$Events_Attended)){
  newdata[paste("dummy", level, sep = "_")] <- ifelse(SGO$Events_Attended == level, 1, 0)
}
for(level in unique(SGO$Membership)){
  newdata[paste("Membership", level, sep = "_")] <- ifelse(SGO$Membership == level, 1, 0)
}
for(level in unique(SGO$Mentorship)){
  newdata[paste("Mentorship", level, sep = "_")] <- ifelse(SGO$Mentorship == level, 1, 0)
}
for(level in unique(SGO$Content_specific)){
  newdata[paste("Content_specific", level, sep = "_")] <- ifelse(SGO$Content_specific == level, 1, 0)
}
for(level in unique(SGO$Educational_workshops)){
  newdata[paste("Educational_workshops", level, sep = "_")] <- ifelse(SGO$Educational_workshops == level, 1, 0)
}
for(level in unique(SGO$`Food/wine`)){
  newdata[paste("Food", level, sep = "_")] <- ifelse(SGO$`Food/wine` == level, 1, 0)
}
for(level in unique(SGO$Job_fairs)){
  newdata[paste("Job_fairs", level, sep = "_")] <- ifelse(SGO$Job_fairs == level, 1, 0)
}
for(level in unique(SGO$Happy_hours)){
  newdata[paste("Happy_hours", level, sep = "_")] <- ifelse(SGO$Happy_hours == level, 1, 0)
}
for(level in unique(SGO$Speed_recruit)){
  newdata[paste("Speed_recruit", level, sep = "_")] <- ifelse(SGO$Speed_recruit == level, 1, 0)
}
for(level in unique(SGO$Walking_tours)){
  newdata[paste("Walking_tours", level, sep = "_")] <- ifelse(SGO$Walking_tours == level, 1, 0)
}
for(level in unique(SGO$Movies)){
  newdata[paste("Movies", level, sep = "_")] <- ifelse(SGO$Movies == level, 1, 0)
}
for(level in unique(SGO$Book_club)){
  newdata[paste("Book_club", level, sep = "_")] <- ifelse(SGO$Book_club == level, 1, 0)
}
for(level in unique(SGO$Karaoke)){
  newdata[paste("Karaoke", level, sep = "_")] <- ifelse(SGO$Karaoke == level, 1, 0)
}
for(level in unique(SGO$Inconvenient_locations)){
  newdata[paste("Inconvenient_locations", level, sep = "_")] <- ifelse(SGO$Inconvenient_locations == level, 1, 0)
}
for(level in unique(SGO$Too_early)){
  newdata[paste("Too_early", level, sep = "_")] <- ifelse(SGO$Too_early == level, 1, 0)
}
for(level in unique(SGO$Too_late)){
  newdata[paste("Too_late", level, sep = "_")] <- ifelse(SGO$Too_late == level, 1, 0)
}
for(level in unique(SGO$Too_expensive)){
  newdata[paste("Too_expensive", level, sep = "_")] <- ifelse(SGO$Too_expensive == level, 1, 0)
}
for(level in unique(SGO$Too_busy)){
  newdata[paste("Too_busy", level, sep = "_")] <- ifelse(SGO$Too_busy == level, 1, 0)
}

names(newdata)

for (name in names(SGO)){
  for(level in levels(SGO[name])){
    newdata[paste("dummy", level, sep = "_")] <- ifelse((SGO[name]) == level, 1, 0)
  }
}

summs <- aggregate(newdata,by=list(fit3$cluster),FUN=mean)

par(mar=c(5, 5, 5, 5))
par(xpd=TRUE)
plot(summs$Group.1, summs$`Too_busy_I'm too busy`, ylim=c(0, .6), type='l', col="red")
lines(summs$Group.1, summs$`Too_late_The events are too late`, col="blue")
lines(summs$Group.1, summs$`Too_early_The events are too early`, col="green")
lines(summs$Group.1, summs$`Too_expensive_The events are too expensive`, col="black")
lines(summs$Group.1, summs$`Inconvenient_locations_The locations are inconvenient`, col="orange")
legend("topright", inset=c(-0.5,-.5), c("Too Busy", "Too Late", "Too Early", "Too Expensive", "Inconvenient"), 
       pch=1, col=c("red", "blue", "green", "black", "orange"))

par(mar=c(5, 5, 5, 5))
par(xpd=TRUE)
plot(summs$Group.1, summs$`dummy_Extremely satisfied`, ylim=c(0, .6), type='l', col="red")
lines(summs$Group.1, summs$`dummy_Very satisfied`, col="blue")
lines(summs$Group.1, summs$`dummy_Moderately satisfied`, col="green")
lines(summs$Group.1, summs$`dummy_Slightly satisfied`, col="black")
lines(summs$Group.1, summs$`dummy_Not at all satisfied`, col="orange")
legend("topright", inset=c(-0.5,-.5), c("Extremely", "Very", "Moderately", "Slightly", "Not at all"), 
       pch=1, col=c("red", "blue", "green", "black", "orange"))

par(mar=c(5, 5, 5, 5))
par(xpd=TRUE)
plot(summs$Group.1, summs$`dummy_Extremely likely - 10`, ylim=c(0, .6), type='l', col="red")
lines(summs$Group.1, summs$`dummy_9`, col="red")
lines(summs$Group.1, summs$`dummy_8`, col="blue")
lines(summs$Group.1, summs$`dummy_7`, col="blue")
lines(summs$Group.1, summs$`dummy_6`, col="orange")
lines(summs$Group.1, summs$`dummy_5`, col="orange")
lines(summs$Group.1, summs$`dummy_4`, col="orange")
lines(summs$Group.1, summs$`dummy_3`, col="orange")
lines(summs$Group.1, summs$`dummy_2`, col="orange")
lines(summs$Group.1, summs$`dummy_1`, col="orange")
legend("topright", inset=c(-0.5,-.5), c("Promoters", "Passives", "Detractors"), 
       pch=1, col=c("red", "blue", "orange"))

par(mar=c(5, 5, 5, 5))
par(xpd=TRUE)
plot(summs$Group.1, summs$`dummy_None`, ylim=c(0, .6), type='l', col="red")
lines(summs$Group.1, summs$`dummy_One`, col="blue")
lines(summs$Group.1, summs$`dummy_More than one`, col="green")
legend("topright", inset=c(-0.5,-.5), c("None", "One", "More than one"), 
       pch=1, col=c("red", "blue", "green"))

library(reshape2)
names(summs)
sumset <- summs[c("Group.1", "Walking_tours_Extremely interested", "Book_club_Extremely interested"
                  , "Karaoke_Extremely interested", "Movies_Extremely interested", "Educational_workshops_Extremely interested"
                  , "Food_Extremely interested", "Job_fairs_Extremely interested", "Happy_hours_Extremely interested"
                  , "Speed_recruit_Extremely interested", "Mentorship_Extremely interested", "Content_specific_Extremely interested")]
sumset

df <- melt(sumset ,  id.vars = 'Group.1', variable.name = 'series')

# plot on same grid, each series colored differently -- 
# good if the series have same scale
library(ggplot2)
dev.off()
ggplot(df, aes(Group.1,value)) + geom_line(aes(colour = as.factor(series)))


par(mar=c(5, 5, 5, 5))
par(xpd=TRUE)
plot(summs$Group.1, summs$`Walking_tours_Extremely interested`, ylim=c(0, .1), type='l', col="darkred")
lines(summs$Group.1, summs$`Book_club_Extremely interested`, col="red")
lines(summs$Group.1, summs$`Karaoke_Extremely interested`, col="tomato")
lines(summs$Group.1, summs$`Movies_Extremely interested`, col="coral")
lines(summs$Group.1, summs$`Educational_workshops_Extremely interested`, col="violetred")
lines(summs$Group.1, summs$`Food_Extremely interested`, col="pink")
lines(summs$Group.1, summs$`Job_fairs_Extremely interested`, col="violetred4")
lines(summs$Group.1, summs$`Happy_hours_Extremely interested`, col="violetred3")
lines(summs$Group.1, summs$`Speed_recruit_Extremely interested`, col="violetred2")
lines(summs$Group.1, summs$`Mentorship_Extremely interested`, col="red2")
lines(summs$Group.1, summs$`Content_specific_Extremely interested`, col="red3")

summs

table(SGO$Pay_SGO)
table(SGO$Pay_Special_Events)
table(SGO$Pay_Prof_Dev)

table(SGO$Pay_SGO, SGO$Events_Attended)
table(SGO$Membership)
table(SGO$Too_expensive_member[SGO$Membership=="No"])
table(SGO$Dont_like_benefits[SGO$Membership=="No"])
table(SGO$Dont_want_to_attend_more_than_one_event_month[SGO$Membership=="No"])
table(SGO$Dont_like_events[SGO$Membership=="No"])
table(SGO$Dont_like_raffle[SGO$Membership=="No"])

head(SGO$Future_open)
head(SGO$Future_topics)
head(SGO$Improve)
head(SGO$Geek_out)

SGO$Future_open  <- tolower(SGO$Future_open)
prof = grepl("professional development", SGO$Future_open)
SGO$Future_open[prof]
work = grepl("workshop", SGO$Future_open)
SGO$Future_open[work]


##Text mining open ended
library(tm)
library(SnowballC)
doc.vec <- VectorSource(SGO$Future_open)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)
tdx = TermDocumentMatrix(doc.corpus)
findFreqTerms(x = tdx, lowfreq=4, highfreq=Inf) #Find most frequent terms

###Find answers with most frequent terms
SGO$Future_open  <- tolower(SGO$Future_open)
prof = grepl("professional development", SGO$Future_open)
SGO$Future_open[prof]
work = grepl("workshop", SGO$Future_open)
SGO$Future_open[work]
spec = grepl("specific", SGO$Future_open)
SGO$Future_open[spec]
women = grepl("men", SGO$Future_open)
SGO$Future_open[women]
foc = grepl("focus", SGO$Future_open)
SGO$Future_open[foc]

doc.vec <- VectorSource(SGO$Future_topics)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)
tdx = TermDocumentMatrix(doc.corpus)
findFreqTerms(x = tdx, lowfreq=4, highfreq=Inf)

SGO$Future_topics  <- tolower(SGO$Future_topics)
prof = grepl("development", SGO$Future_topics)
SGO$Future_topics[prof]
work = grepl("tech", SGO$Future_topics)
SGO$Future_topics[work]
spec = grepl("career", SGO$Future_topics)
SGO$Future_topics[spec]

doc.vec <- VectorSource(SGO$Improve)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)
tdx = TermDocumentMatrix(doc.corpus)
findFreqTerms(x = tdx, lowfreq=3, highfreq=Inf)

SGO$Improve  <- tolower(SGO$Improve)
prof = grepl("speaker", SGO$Improve)
SGO$Improve[prof]
work = grepl("negot", SGO$Improve)
SGO$Improve[work]
spec = grepl("space", SGO$Improve)
SGO$Improve[spec]

doc.vec <- VectorSource(SGO$Do_well)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)
tdx = TermDocumentMatrix(doc.corpus)
findFreqTerms(x = tdx, lowfreq=4, highfreq=Inf)

SGO$Do_well  <- tolower(SGO$Do_well)
prof = grepl("community", SGO$Do_well)
SGO$Do_well[prof]
work = grepl("food", SGO$Do_well)
SGO$Do_well[work]
spec = grepl("speaker", SGO$Do_well)
SGO$Do_well[spec]
