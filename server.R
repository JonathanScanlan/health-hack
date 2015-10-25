rm(list=ls())
gc()

library('shiny')
library('ggmap')
#TheRecords<-read.csv('http://52.89.201.245:3838/data/DataForAppV2.csv')
TheRecords<-read.csv('/data/DataForAppV2.csv')
ForBeta<-c(sample(which(paste0(TheRecords[,'facility_country'])=='United States'),50),sample(which(paste0(TheRecords[,'facility_country'])=='Germany'),50))
#TheRecords<-TheRecords[ForBeta,]

#This subsets the data to the most recent records
Dates<-strptime(paste0(TheRecords[,'last_touch_date']),format="%d/%m/%Y")
CurrentDate<-strptime(Sys.Date(),format="%Y-%m-%d")
Days<-difftime(CurrentDate,Dates)
TheRecords<-cbind(TheRecords,"DaysSince"=Days)
TheRecords<-TheRecords[order(TheRecords[,'DaysSince']),]
FirstRecs<-!duplicated(TheRecords[,c("name_of_laboratory","name_of_institution","facility_country",'last_touch_date','genes',"lon","lat")])
TheRecords<-TheRecords[FirstRecs,]
Genes<-strsplit(paste0(TheRecords[,'genes']),"\\|")
Dates<-strptime(paste0(TheRecords[,'last_touch_date']),format="%d/%m/%Y")
FirstRecs<-!duplicated(TheRecords[,c("name_of_laboratory","name_of_institution","facility_country","lon","lat")])
TheRecords<-TheRecords[,c("name_of_laboratory","name_of_institution","facility_country","lon","lat")]

##This revises all interaction dates with the latest interaction
Breakdown<-paste0(TheRecords[,"name_of_laboratory"],TheRecords[,"name_of_institution"],TheRecords[,"facility_country"])
for(i in unique(Breakdown)){
Dates[Breakdown==i]<-(Dates[Breakdown==i])[1]
}

##Version 2 to incorporate date
Review<-function(Y,D){
A<-do.call(cbind,lapply(Y,function(Z){
do.call('c',lapply(Genes,function(X){Z %in% X}))}))
A<-do.call('c',lapply(1:nrow(A),function(X){FALSE %in% A[X,]}))
D<-rev(sort(D))
Inside<-Dates <= D[1] & Dates >= D[2] & !A
Outside<-!(Dates <= D[1] & Dates >= D[2]) & !A
Binder<-list(Inside,Outside)
Binder<-lapply(Binder,function(Q){
TheRecords[Q,c("name_of_laboratory","name_of_institution","facility_country","lon","lat")]})
Binder<-lapply(1:2,function(Q){
if(nrow(Binder[[Q]])>0){
Binder[[Q]]<-cbind(Binder[[Q]],"InDates"=(Q-1))}
})
A<-do.call(rbind,Binder)
if(length(A)>1){
L<-c()
if(length(Binder[[1]])>0){
L<-c(L,'Within Date Range')
}
if(length(Binder[[2]])>0){
L<-c(L,'Outside Date Range')
}
A[,'InDates']<-as.factor(as.numeric(paste0(A[,'InDates'])))
levels(A[,'InDates'])<-L
A<-A[!duplicated(A[,-which(colnames(A)=="InDates")]),]
}else{A<-c()}
unique(A)}

G<-geocode('united states of america')
Countries<-list()
Countries[[1]]<-qmap(location=c(lon=(G[1,1]-1),lat=G[1,2]), zoom=4, color="bw")
Countries[[2]]<-qmap('germany', zoom=6, color="bw")

CountryNames<-c("United States","Germany")

genOutput<-function(Y,D,C){
SplitsVille<-strsplit(gsub(toupper(Y)," ",""),",")[[1]]
Table<-Review(Y,D)
Table<-Table[paste0(Table[,'facility_country'])==CountryNames[C],]
C<-Countries[[C]]
if(length(Table)>0){
C<-C+geom_point(data=Table,aes(x=lon,y=lat,color=InDates))+theme(legend.position="top")+
theme(legend.title=element_blank())+scale_colour_manual(values=c("Outside Date Range"="red","Within Date Range"="blue"))
Table<-Table[,-which(colnames(Table) %in% c('lon','lat','facility_country'))]
colnames(Table)<-c("Name","Institution","Last Contact")
}
list("Mapped"=C,"Table"=Table)}

Test<-strptime(c("2011-01-01","2015-12-10"),format="%Y-%m-%d")
Dates
LetsGo<-Review('MLH1',Test)

shinyServer(function(input, output) {

  # Fill in the spot we created for a plot
  output$Mapped <- renderPlot(genOutput(input$GenText,strptime(paste0(input$dates),format="%Y-%m-%d"),as.numeric(input$region))$Mapped,height = 400, width = 600)
  output$Labs<- renderTable({genOutput(input$GenText,strptime(paste0(input$dates),format="%Y-%m-%d"),as.numeric(input$region))$Table})

})

