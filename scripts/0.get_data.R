"data/data_rp.csv"<-system("mc cp s3/projet-formation/bonnes-pratiques/data/RPindividus_24.csv")

system("mc mirror s3/projet-formation/bonnes-pratiques/data/RPindividus_24.csv ~/RPindividus_24.csv")

install.packages("styler")