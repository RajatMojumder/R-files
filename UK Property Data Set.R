library(readr)
df_ret <- read.csv( "https://raw.githubusercontent.com/masud-alam/ECO274LAB/main/ret_final.csv")

df_ret1 <- df_ret[,-1]
k = ncol(df_ret1)
SS = matrix(NA,ncol=5,nrow=k)
rownames(SS) = colnames(df_ret1)
colnames(SS) = c("Obs","Minimum","Mean","Maximum","SD")
for (i in 1:k) {
  SS[i,] = 
    c(nrow(df_ret1),min(df_ret1[,i]),mean(df_ret1[,i]),max(df_ret1[,i]),sd(df_ret1[,i]))
}
round(SS,4)

View(SS)

library(stargazer)
stargazer(SS, type = "latex", title = "Dynamic 
Connectedness of UK Regional Property Returns:Summary Statistics", 
          out = "table1.tex")