if(!require(rmarkdown)) install.packages("rmarkdown")
library(rmarkdown,lib.loc = "C:/Users/User/Documents/R/win-library/3.4")

<<<<<<< HEAD
render(input = "C:/Users/User/Documents/project/dabrp_classnote/class7sample/pdftesthangul.Rmd",
=======
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

render(input = "C:/Users/mrchypark/Documents/project/dabrp_classnote2/class7sample/pdftesthangul.Rmd",
>>>>>>> 3182c8cf688040aaa108eba54732cb1d214063a9
       encoding="UTF-8")
