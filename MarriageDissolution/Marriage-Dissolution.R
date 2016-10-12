# The unit of observation is the couple and the event of interest is divorce, with interview and widowhood treated as censoring events. We have three fixed covariates: education of the husband and two indicators of the couple's ethnicity: whether the husband is black and whether the couple is mixed. The variables are:
# 
# - id: a couple number.
# - heduc: education of the husband, coded
# - 0 = less than 12 years,
# - 1 = 12 to 15 years, and
# - 2 = 16 or more years.
# - heblack: coded 1 if the husband is black and 0 otherwise
# - mixed: coded 1 if the husband and wife have different ethnicity (defined as black or other), 0 otherwise.
# - years: duration of marriage, from the date of wedding to divorce or censoring (due to widowhood or interview).
# - div: the failure indicator, coded 1 for divorce and 0 for censoring.

names <- c("id","hEduc","hIsBlack","mixed","duration","div")
marriage_dissolution <- read.table("~/Downloads/marriage_dissolution.txt", quote="\"", comment.char="",col.names = names)
View(marriage_dissolution)
