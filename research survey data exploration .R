library('dplyr')
library('plotly')
library('reshape')
library(RColorBrewer)
data1 <- read.csv('/Users/yimanzhang/Desktop/101-innovations-research-tools-survey/survey_cleaned.csv',header=1)
dim(data1)
names(data1)
levels(data1$PUBYEAR)
data1$PUBYEAR <- data1$PUBYEAR %>% 
    relevel(ref='before 1991') 
f <- function(x){
  new_data <- x %>%
    melt(id=c('ID', "PUBYEAR")) %>%
    filter(value!=''&PUBYEAR!='') %>%
    group_by(PUBYEAR,variable) %>%
    summarise(n=n()) 
  p <- plot_ly(new_data, x=new_data$PUBYEAR,y=new_data$n,
               color=new_data$variable, type='bar',
               colors=brewer.pal(10,"Paired")) %>%
    layout(barmode='stack')
  return(p)
  
}
data1=droplevels(data1)
f(select(data1, ID, PUBYEAR, PHYS:ARTHUM))
f(select(data1, ID, PUBYEAR, RTOOL:ANALYZOTHCL))
f(select(data1, ID, PUBYEAR, WORD:WRITESPECCL),)
f(select(data1, ID, PUBYEAR, GITHUB:DATASPECCL))

