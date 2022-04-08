library(readxl)
library(tidyverse)


# col = read_xlsx("H:/JenaSP6_2021/Coll family.xlsx",
#                 sheet = "Sheet1",
#                 na = "")
# 
# df_new = col %>% select(-contains("length"))
# 
# unc = as.data.frame(df_new)
# unc$Subplot = gsub("Treatment", "D", unc$Subplot)
# unc$PlotSub = paste(unc$Plot, unc$Subplot, sep = "")
# unc$PlotSub = gsub(", bottom only", "b", unc$PlotSub)
# unc$PlotSub = gsub(", top only", "t", unc$PlotSub)
# row.names(unc) = unc[,15]
# unc=unc[,-c(1,2,3,15)]
# 
# hum = 1-vegan::vegdist(unc, method = "jaccard", binary=TRUE)
# as.matrix(hum)[4:6,1:3]

########################## Protura, Pauropoda, Symphyla ########################

################################### Collembola #################################

raw = read_xlsx("H:/JenaSP6_2021/Coll family.xlsx",
                sheet = "Tabelle1",
                na = "")
df_new = raw %>% select(-contains("length"))
# mmmm = str_split(col[1,]$`length (mm)...5`, pattern = ";", simplify = TRUE)
# names(mmmm) = rownames(unc)
# df <- data.frame(matrix(unlist(mmmm), nrow=6, byrow=TRUE),stringsAsFactors=FALSE)

coll = col %>% rename_at(vars(starts_with('length')), funs(paste(colnames(df_new[,4:14]), .))) %>% 
  arrange(Plot, Subplot)
coll = as.data.frame(coll)

colll = coll %>% select(contains("length"))
colll = as.data.frame(colll)

for(i in colnames(df_new[4:14])) { 
  mmmm[,i] <- NA
}


for (i in 1:11) {
  mmmmm = as.data.frame(str_split(colll[,i], 
                                  pattern = ";|:|; |: |. |, ", 
                                  simplify = T))
  n=dim(mmmmm)[2]
  if (n >= 10) {
    mmmmm = mmmmm[,1:10]
  } else {
    m=10-n
    nam = c(paste0("col",1:m))
    mmmmm[ , nam] <- NA
  }
  mmmmm = as.matrix(mmmmm)
  new_vector <- c(t(mmmmm))
  mmmm[,2+i] <- new_vector
}

# write.csv(mmmm, file = "H:/aaaarrrgh.csv")


##################################### Acari ####################################