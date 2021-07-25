'.libPaths'()

#install the necessary libraries
install.packages("lubridate")
install.packages('plotly')
install.packages('tidyverse')

#load packages
library('lubridate') #to modify the date
library('plotly') #for graphing
library('tidyverse') #for data preprocessing

covid <- read.csv('https://raw.githubusercontent.com/makena-yvonne/Covid-19-Data-Analysis-using-plotly/main/Dataset/country_daywise.csv')

head(covid)

dim(covid)

covid$Date <- ymd(covid$Date)

head(covid)

covid <- arrange(covid, Date)

tail(covid)

daywise <- read.csv('https://raw.githubusercontent.com/makena-yvonne/Covid-19-Data-Analysis-using-plotly/main/Dataset/daywise.csv')

head(daywise)

daywise$Date <- ymd(daywise$Date)
daywise <- arrange(daywise, Date)

head(daywise)

#plot a scatter plot
plot_ly(daywise, x=~Date, y=~Confirmed, type='scatter', mode='lines')

#names of columns in the daywise data
names(daywise)

fig <- plot_ly(daywise, x=~Date)

fig <- fig %>% add_trace(y=~Confirmed, name='Confirmed', mode='lines', type='scatter')
fig <- fig %>% add_trace(y=~Recovered, name='Recovered', mode='lines', type='scatter')
fig <- fig %>% add_trace(y=~Deaths, name='Deaths', mode='lines', type='scatter')
fig

cnf <- '#893aa6'
dth <- '#ff2e63'
rec <- '#21bf73'
act <- '#fe9801'

fig <- plot_ly(daywise, x=~Date)

cnf.line <- list(color=cnf,width=4)
dth.line <- list(color=dth,width=4)
rec.line <- list(color=rec,width=4)

fig <- fig %>% add_trace(y=~Confirmed, name='Confirmed', mode='lines', type='scatter', line=cnf.line)
fig <- fig %>% add_trace(y=~Recovered, name='Recovered', mode='lines', type='scatter', line=rec.line)
fig <- fig %>% add_trace(y=~Deaths, name='Deaths', mode='lines', type='scatter', line=dth.line)

fig <- fig %>% layout(title='Total Worldwide Corona Cases',
                     xaxis=list(title='Date'),
                     yaxis=list(title='Total Cases'))
fig

fig <- plot_ly(daywise, x=~Date)

cnf.line <- list(color=cnf,width=4)
dth.line <- list(color=dth,width=4)
rec.line <- list(color=rec,width=4)

cnf.marker <- list(color=act, size=2, opacity=1,
                  line=list(color='#A5321a', width=4))

fig <- fig %>% add_trace(y=~Confirmed, name='Confirmed', mode='lines+markers', type='scatter', line=cnf.line, marker=cnf.marker)
fig <- fig %>% add_trace(y=~Recovered, name='Recovered', mode='lines', type='scatter', line=rec.line)
fig <- fig %>% add_trace(y=~Deaths, name='Deaths', mode='lines', type='scatter', line=dth.line)

fig <- fig %>% layout(title='Total Worldwide Corona Cases',
                     xaxis=list(title='Date'),
                     yaxis=list(title='Total Cases'))
fig

head(covid)

tail(covid)

latest <- covid %>% filter(Date==max(Date)) %>% arrange(desc(Confirmed))

top10 <- latest %>% slice(1:10)

top10

summary(top10)

plot_ly(top10, x= ~Country, y= ~Confirmed, type='bar', name='Confirmed Cases')

factor(top10$Country, levels=c(as.character(top10$Country)))

top10$Country <- factor(top10$Country, levels=c(as.character(top10$Country)))

plot_ly(top10, x=~Country, y=~Confirmed, type='bar', name='Confirmed Cases')

values <- as.character(top10$Confirmed)

plot_ly(top10, x=~Country, y=~Confirmed, type='bar', name='Confirmed Cases',
       text=values, textposition='auto', marker=list(color=cnf, line=list(color='magenta', width=0)))

rainbow(n=10)

plot_ly(top10, x=~Country, y=~Confirmed, type='bar', name='Confirmed Cases',
       text=values, textposition='auto', marker=list(color=rainbow(n=10)))

plot_ly(top10, x=~Country, y=~Confirmed, type='bar', name='Confirmed Cases',
       text=values, textposition='auto', marker=list(color=heat.colors(n=10)))

us <- covid%>% filter(Country=='US') %>% arrange(Date)

head(us)

tail(us)

fig1 <- plot_ly(us, x=~Date, y=~Confirmed, type='scatter', mode='lines', name='Confirmed Cases')
fig2 <- plot_ly(us, x=~Date, y=~Recovered, type='scatter', mode='lines', name='Recovered Cases')
fig3 <- plot_ly(us, x=~Date, y=~Deaths, type='scatter', mode='lines', name='Death Cases')

subplot(fig1, fig3, fig2, nrows=2, shareX=FALSE)

head(daywise)

fig1 <- plot_ly(daywise, x=~Date, y=~Confirmed, type='scatter', mode='lines', name='Confirmed Cases')
fig2 <- plot_ly(daywise, x=~Date, y=~Recovered, type='scatter', mode='lines', name='Recovered Cases')
fig3 <- plot_ly(daywise, x=~Date, y=~Deaths, type='scatter', mode='lines', name='Death Cases')

fig4 <- plot_ly(daywise, x=~Date, y=~New.Cases, type='scatter', mode='lines', name='New.Cases')

fig5 <- plot_ly(daywise, x=~Date, y=~Deaths...100.Cases, type='scatter', mode='lines', name='Deaths/100.Cases')
fig6 <- plot_ly(daywise, x=~Date, y=~No..of.Countries, type='scatter', mode='lines', name='No.Of.Countries')

subplot(fig1, fig3, fig2, fig4, fig5, fig6, nrows=3, shareX=FALSE)

fig1 <- plot_ly(daywise, x=~Date, y=~Confirmed, type='bar', name='Confirmed Cases', height=1000)
fig2 <- plot_ly(daywise, x=~Date, y=~Recovered, type='bar', name='Recovered Cases')
fig3 <- plot_ly(daywise, x=~Date, y=~Deaths, type='bar', name='Death Cases')

fig4 <- plot_ly(daywise, x=~Date, y=~New.Cases, type='bar', name='New.Cases')

fig5 <- plot_ly(daywise, x=~Date, y=~Deaths...100.Cases, type='bar', name='Deaths/100.Cases')
fig6 <- plot_ly(daywise, x=~Date, y=~No..of.Countries, type='bar', name='No.Of.Countries')

subplot(fig1, fig3, fig2, fig4, fig5, fig6, nrows=3, shareX=FALSE)

plot_ly(top10, x=~Confirmed, y=~Deaths, type='scatter', mode='markers',
        color=~Country, colors=heat.colors(n=10), size=~Confirmed,
        marker=list(size=~1e-4*Deaths))

head(top10)

fig1 <- plot_ly(top10, labels=~Country, values=~Confirmed, type='pie', textinfo='label+percent')
fig1


fig2 <- plot_ly(top10, labels=~Country, values=~Deaths, type='pie', textinfo='label+percent')
fig2

fig <- plot_ly(top10, labels=~Country, values=~Deaths, textinfo='label+percent')
fig <- fig %>% add_pie(hole=0.6)
fig

