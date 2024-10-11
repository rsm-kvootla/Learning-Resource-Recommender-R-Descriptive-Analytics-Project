ggplot(data, aes(x=Education, fill=dream.tech.job)) + geom_bar(stat="count")+theme_minimal()+theme(axis.text.x = element_text(angle=45, hjust=1))+scale_fill_brewer(palette="Set1")+labs(title ="dream tech job based on education level")+xlab("dream tech job")+ylab("freq")
ggplot(data, aes(x=dream.tech.job, fill=Gender)) + geom_bar(stat="count")+theme_minimal()+theme(axis.text.x = element_text(angle=45, hjust=1))+scale_fill_brewer(palette="Set1")+labs(title ="dream tech job based on gender")+xlab("dream tech job")+ylab("freq")
ggplot(data, aes(x=Specialisation, fill=dream.tech.job)) + geom_bar(stat="count")+theme_minimal()+theme(axis.text.x = element_text(angle=45, hjust=1))+scale_fill_brewer(palette="Set1")+labs(title ="dream tech job among different specializations")+xlab("specialization")+ylab("freq")
a<-table(data$`proficiency JavaScript/Java`)
a<-as.data.frame(a)
a$language<-c("JavaScript/Java")

b<-table(data$`proficiency R`)
b<-as.data.frame(b)
b$language<-c("R")

c<-table(data$`Proficiency Python`)
c<-as.data.frame(c)
c$language<-c("Python")

d<-table(data$`Proficiency C/C#/C++`)
d<-as.data.frame(d)
d$language<-c("C/C#/C++")

e<-table(data$`Proficiency HTML/CSS`)
e<-as.data.frame(e)
e$language<-c("HTML/CSS")

f<-table(data$`Proficiency Go`)
f<-as.data.frame(f)
f$language<-c("Go")

g<-table(data$`Proficiency SQL`)
g<-as.data.frame(g)
g$language<-c("SQL")

h<-table(data$`Proficiency Bash/shell`)
h<-as.data.frame(h)
h$language<-c("Bash/shell")

i<-table(data$`Proficiency Ruby`)
i<-as.data.frame(i)
i$language<-c("Ruby")

j<-table(data$`Proficiency Swift`)
j<-as.data.frame(j)
j$language<-c("Swift")

k<-table(data$`Proficiency Typescript`)
k<-as.data.frame(k)
k$language<-c("Typescript")
L<-rbind(a,b,c,d,e,f,j,g,h,i,k)
colnames(L)[1]<-"Proficiency"
ggplot(L, aes(x=language, y=Freq, fill=Proficiency)) + geom_bar(stat="identity",position="stack")+theme_minimal()+theme(axis.text.x = element_text(angle=45, hjust=1))+scale_fill_brewer(palette="Set1")+labs(title ="proficieny in programming among students")+xlab("languages")+ylab("freq")
