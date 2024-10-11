library(dplyr)
#specify number of rows
Nrows=500

#generate education levels
a<-sample(c("final high school","Pre-university","University undergraduate","graduate university","polytechnic"),Nrows,T,c(0.107,0.10,0.304,0.137,0.352))
data1<-as.data.frame(a)
colnames(data1)[1]<-"Education"

#generate gender
data3<-data1
for(i in 1:Nrows)
{education<-data3[i,]
if(education=="final high school")
{b<-sample(c("female","male"),1,T,c(0.488,0.522))
data3[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("female","male"),1,T,c(0.527,0.473))
data3[i,]<-b
} 
if(education=="University undergraduate")
{b<-sample(c("female","male"),1,T,c(0.496,0.504))
data3[i,]<-b
}
if(education=="graduate university")
{b<-sample(c("female","male"),1,T,c(0.469,0.531))
data3[i,]<-b
}
if(education=="polytechnic")
{b<-sample(c("female","male"),1,T,c(0.474,0.526))
data3[i,]<-b
}
}
colnames(data3)[1]<-"Gender"
#generate specialization or prospective specialization
data2<-data1
data4<-data3
for(i in 1:Nrows)
{education<-data2[i,]
gender<-data4[i,]
if(education=="final high school")
  {if(gender=="male")
    {b<-sample(c("business","engineering","computing"),1,T,c(1/3,1/3,1/3))
      data4[i,]<-b}
  else
    {b<-sample(c("business","engineering","computing"),1,T,c(1/3,1/3,1/3))
    data4[i,]<-b}
  }
if(education=="Pre-university")
{if(gender=="male")
{b<-sample(c("business","engineering","computing"),1,T,c(1/3,1/3,1/3))
data4[i,]<-b}
  else
  {b<-sample(c("business","engineering","computing"),1,T,c(1/3,1/3,1/3))
  data4[i,]<-b}
}
if(education=="University undergraduate")
  {if(gender=="male")
    {b<-sample(c("business","engineering","computing"),1,T,c(0.220,0.409,0.371))
      data4[i,]<-b}
  else
    {b<-sample(c("business","engineering","computing"),1,T,c(0.442,0.361,0.197))
      data4[i,]<-b}
  }
if(education=="graduate university")
  {if(gender=="male")
    {b<-sample(c("business","engineering","computing"),1,T,c(0.322,0.494,0.184))
      data4[i,]<-b}
  else
    {b<-sample(c("business","engineering","computing"),1,T,c(0.401,0.465,0.134))
      data4[i,]<-b}
  }
if(education=="polytechnic")
  {if(gender=="male")
    {b<-sample(c("business","engineering","computing"),1,T,c(0.198,0.568,0.234))
      data4[i,]<-b}
  else
    {b<-sample(c("business","engineering","computing"),1,T,c(0.557,0.292,0.151))
      data4[i,]<-b}
  }
}
colnames(data4)[1]<-"Specialisation"
#generate dream tech role
c<-sample(c("Telecommunications and Networks","Software and Applications","IT Services","Emerging Technologies","Business Analytics"),Nrows,T,c(0.120,0.153,0.168,0.224,0.336))
data5<-as.data.frame(c)
colnames(data5)[1]<-"dream.tech.job"
#generate time availability
data6<-data1
for(i in 1:Nrows)
{education<-data6[i,]
  if(education=="final high school")
     {b<-sample(c("0-15","15-30","30-45","45-60","60-75","75-90","90-105","105-120","more"),1,T,c(0.10,0.20,0.25,0.25,0.10,0.05,0.03,0.01,0.01))
      data6[i,]<-b
    }
  if(education=="Pre-university")
      {b<-sample(c("0-15","15-30","30-45","45-60","60-75","75-90","90-105","105-120","more"),1,T,c(0.10,0.20,0.25,0.25,0.10,0.05,0.03,0.01,0.01))
        data6[i,]<-b
    } 
  if(education=="University undergraduate")
      {b<-sample(c("0-15","15-30","30-45","45-60","60-75","75-90","90-105","105-120","more"),1,T,c(0.05,0.15,0.25,0.30,0.15,0.02,0.01,0.01,0.01))
        data6[i,]<-b
    }
  if(education=="graduate university")
      {b<-sample(c("0-15","15-30","30-45","45-60","60-75","75-90","90-105","105-120","more"),1,T,c(0.05,0.10,0.15,0.25,0.25,0.15,0.03,0.01,0.01))
        data6[i,]<-b
    }
  if(education=="polytechnic")
    {b<-sample(c("0-15","15-30","30-45","45-60","60-75","75-90","90-105","105-120","more"),1,T,c(0.05,0.15,0.25,0.30,0.15,0.02,0.01,0.01,0.01))
        data6[i,]<-b
    }
}
colnames(data6)[1]<-"free time"
#how many internships done in tech?
data7<-data1
for(i in 1:Nrows)
{education<-data7[i,]
if(education=="final high school")
{b<-sample(c("1","0"),1,T,c(0,1))
data7[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("2","1","0"),1,T,c(0.01,0.04,0.95))
data7[i,]<-b
} 
if(education=="University undergraduate")
{b<-sample(c("3","2","1","0"),1,T,c(0.04,0.15,0.25,0.56))
data7[i,]<-b
}
if(education=="graduate university")
{b<-sample(c("more","3","2","1","0"),1,T,c(0.15,0.3,0.30,0.20,0.05))
data7[i,]<-b
}
if(education=="polytechnic")
{b<-sample(c("3","2","1","0"),1,T,c(0.04,0.15,0.25,0.56))
data7[i,]<-b
}
}
colnames(data7)[1]<-"number of internships done"
#internship done?
data8<-data7
for(i in 1:Nrows)
{education<-data8[i,]
if(!education=="0")
{b<-"yes"
data8[i,]<-b}
else
  {b<-"no"
data8[i,]<-b}
}
colnames(data8)[1]<-"internships done"

#JavaScript/Java proficiency
data9<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data9[i,]
major<-data10[i,]
if(education=="final high school")
{b<-sample(c("yes","no"),1,T,c(0,1))
data9[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data9[i,]<-b
} 
if(education=="University undergraduate")
 {if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.1,0.9))
data9[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.6,0.4))
  data9[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.95,0.05))
  data9[i,]<-b}
}
if(education=="graduate university")
 {if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.2,0.8))
data9[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.97,0.03))
  data9[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.97,0.03))
  data9[i,]<-b}
}
if(education=="polytechnic")
 {if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.1,0.9))
data9[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.5,0.5))
  data9[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.8,0.2))
  data9[i,]<-b}
}
}
colnames(data9)[1]<-"proficiency JavaScript/Java"
#R
data11<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data11[i,]
major<-data10[i,]
if(education=="final high school")
{b<-sample(c("yes","no"),1,T,c(0.02,0.98))
data11[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("yes","no"),1,T,c(0.05,0.95))
data11[i,]<-b
} 
if(education=="University undergraduate")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.6,0.4))
data11[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.8,0.2))
  data11[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.90,0.1))
  data11[i,]<-b}
}
if(education=="graduate university")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.95,0.05))
data11[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.95,0.05))
  data11[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.95,0.05))
  data11[i,]<-b}
}
if(education=="polytechnic")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.5,0.5))
data11[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.7,0.3))
  data11[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.8,0.2))
  data11[i,]<-b}
}
}
colnames(data11)[1]<-"proficiency R"
#Python
data12<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data12[i,]
  major<-data10[i,]
  if(education=="final high school")
  {b<-sample(c("yes","no"),1,T,c(0.02,0.98))
  data12[i,]<-b
  }
  if(education=="Pre-university")
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data12[i,]<-b
  } 
  if(education=="University undergraduate")
  {if(major=="business")
  {b<-sample(c("yes","no"),1,T,c(0.6,0.4))
  data12[i,]<-b}
    if(major=="engineering")
    {b<-sample(c("yes","no"),1,T,c(0.8,0.2))
    data12[i,]<-b}
    else
    {b<-sample(c("yes","no"),1,T,c(0.90,0.1))
    data12[i,]<-b}
  }
  if(education=="graduate university")
  {if(major=="business")
  {b<-sample(c("yes","no"),1,T,c(0.95,0.05))
  data12[i,]<-b}
    if(major=="engineering")
    {b<-sample(c("yes","no"),1,T,c(0.95,0.05))
    data12[i,]<-b}
    else
    {b<-sample(c("yes","no"),1,T,c(0.95,0.05))
    data12[i,]<-b}
  }
  if(education=="polytechnic")
  {if(major=="business")
  {b<-sample(c("yes","no"),1,T,c(0.5,0.5))
  data12[i,]<-b}
    if(major=="engineering")
    {b<-sample(c("yes","no"),1,T,c(0.7,0.3))
    data12[i,]<-b}
    else
    {b<-sample(c("yes","no"),1,T,c(0.8,0.2))
    data12[i,]<-b}
  }
}
colnames(data12)[1]<-"Proficiency Python"
#C/ C++/c#
data13<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data13[i,]
major<-data10[i,]
if(education=="final high school")
{b<-sample(c("yes","no"),1,T,c(0,1))
data13[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data13[i,]<-b
} 
if(education=="University undergraduate")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.05,0.95))
data13[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.6,0.4))
  data13[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.7,0.3))
  data13[i,]<-b}
}
if(education=="graduate university")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.15,0.75))
data13[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.7,0.3))
  data13[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.9,0.1))
  data13[i,]<-b}
}
if(education=="polytechnic")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.1,0.9))
data13[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.5,0.5))
  data13[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.7,0.3))
  data13[i,]<-b}
}
}
colnames(data13)[1]<-"Proficiency C/C#/C++"
#HTML/CSS
data14<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data14[i,]
major<-data10[i,]
if(education=="final high school")
{b<-sample(c("yes","no"),1,T,c(0,1))
data14[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data14[i,]<-b
} 
if(education=="University undergraduate")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.05,0.95))
data14[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.6,0.4))
  data14[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.7,0.3))
  data14[i,]<-b}
}
if(education=="graduate university")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.15,0.75))
data14[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.7,0.3))
  data14[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.9,0.1))
  data14[i,]<-b}
}
if(education=="polytechnic")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.1,0.9))
data14[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.5,0.5))
  data14[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.7,0.3))
  data14[i,]<-b}
}
}
colnames(data14)[1]<-"Proficiency HTML/CSS"
#SQL
data15<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data15[i,]
major<-data10[i,]
if(education=="final high school")
{b<-sample(c("yes","no"),1,T,c(0,1))
data15[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data15[i,]<-b
} 
if(education=="University undergraduate")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.10,0.90))
data15[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.6,0.4))
  data15[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.7,0.3))
  data15[i,]<-b}
}
if(education=="graduate university")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.08,0.92))
data15[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.7,0.3))
  data15[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.85,0.15))
  data15[i,]<-b}
}
if(education=="polytechnic")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.05,0.95))
data15[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.5,0.5))
  data15[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.6,0.4))
  data15[i,]<-b}
}
}
colnames(data15)[1]<-"Proficiency SQL"
#Typescript
data16<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data16[i,]
major<-data10[i,]
if(education=="final high school")
{b<-sample(c("yes","no"),1,T,c(0,1))
data16[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data16[i,]<-b
} 
if(education=="University undergraduate")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.02,0.98))
data16[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.35,0.65))
  data16[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.35,0.65))
  data16[i,]<-b}
}
if(education=="graduate university")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.03,0.97))
data16[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.35,0.65))
  data16[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.35,0.65))
  data16[i,]<-b}
}
if(education=="polytechnic")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.02,0.98))
data16[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.3,0.7))
  data16[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.5,0.5))
  data16[i,]<-b}
}
}
colnames(data16)[1]<-"Proficiency Typescript"
#Bash/shell
data17<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data17[i,]
major<-data10[i,]
if(education=="final high school")
{b<-sample(c("yes","no"),1,T,c(0,1))
data17[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("yes","no"),1,T,c(0,1))
data17[i,]<-b
} 
if(education=="University undergraduate")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data17[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.3,0.7))
  data17[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.35,0.65))
  data17[i,]<-b}
}
if(education=="graduate university")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.04,0.96))
data17[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.35,0.65))
  data17[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.35,0.65))
  data17[i,]<-b}
}
if(education=="polytechnic")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.02,0.98))
data17[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.25,0.75))
  data17[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.25,0.75))
  data17[i,]<-b}
}
}
colnames(data17)[1]<-"Proficiency Bash/shell"
#Go
data18<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data18[i,]
major<-data10[i,]
if(education=="final high school")
{b<-sample(c("yes","no"),1,T,c(0,1))
data18[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("yes","no"),1,T,c(0,1))
data18[i,]<-b
} 
if(education=="University undergraduate")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data18[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.1,0.9))
  data18[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.1,0.9))
  data18[i,]<-b}
}
if(education=="graduate university")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data18[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.15,0.85))
  data18[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.15,0.85))
  data18[i,]<-b}
}
if(education=="polytechnic")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data18[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.15,0.85))
  data18[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.15,0.85))
  data18[i,]<-b}
}
}
colnames(data18)[1]<-"Proficiency Go"
#Ruby
data19<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data19[i,]
major<-data10[i,]
if(education=="final high school")
{b<-sample(c("yes","no"),1,T,c(0,1))
data19[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("yes","no"),1,T,c(0,1))
data19[i,]<-b
} 
if(education=="University undergraduate")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data19[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data19[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data19[i,]<-b}
}
if(education=="graduate university")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data19[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data19[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data19[i,]<-b}
}
if(education=="polytechnic")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data19[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data19[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data19[i,]<-b}
}
}
colnames(data19)[1]<-"Proficiency Ruby"
#Swift
data20<-data1
data10<-data4
for(i in 1:Nrows)
{education<-data20[i,]
major<-data10[i,]
if(education=="final high school")
{b<-sample(c("yes","no"),1,T,c(0,1))
data20[i,]<-b
}
if(education=="Pre-university")
{b<-sample(c("yes","no"),1,T,c(0,1))
data20[i,]<-b
} 
if(education=="University undergraduate")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data20[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data20[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data20[i,]<-b}
}
if(education=="graduate university")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data20[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data20[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data20[i,]<-b}
}
if(education=="polytechnic")
{if(major=="business")
{b<-sample(c("yes","no"),1,T,c(0.01,0.99))
data20[i,]<-b}
  if(major=="engineering")
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data20[i,]<-b}
  else
  {b<-sample(c("yes","no"),1,T,c(0.05,0.95))
  data20[i,]<-b}
}
}
colnames(data20)[1]<-"Proficiency Swift"

#merge data
data<-cbind(data1,data3,data4,data5,data6,data7,data8,data9,data11,data12,data13,data14,data15,data16,data17,data18,data19,data20)
print(data)