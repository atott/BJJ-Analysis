library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(data.table)
library(ggrepel)
library(plotly)
library(shinythemes)


#################################################
df<-read.csv("Jiu-Jitsu_log.csv",header = T,stringsAsFactors = FALSE)
df$date<-as.Date(df$date, "%m/%d/%Y")
df$week<-week(df$date) +53*(year(df$date)-min(year(df$date)))
################################################
df$week<-as.character(df$week)
df2<-aggregate(hours ~ week, data = df, sum)
df2<-as.data.table(df2)
week.d<-as.data.table(df$week)
week.e<-as.vector(df2$week)
week.d<-unique(as.character(unlist(week.d)))
df2$week<-factor(df2$week, levels=week.d)
df2<-df2[order(df2$week),]
df2$week<- as.character(df2$week)
df2$week<- as.integer(df2$week)
df2<- mutate(df2, belt = ifelse(week > 70, "blue", "white"))
df3<-df%>%select(week,comment)
df3<-unique(df3)
df3$week<-as.numeric(df3$week)
df2<-full_join(df3,df2,by="week")
df2<-df2 %>% group_by(week,hours,belt)%>%summarise(comment=paste(na.omit(comment), collapse=","))
df2[ df2 == "" ] <- NA





ui<- fluidPage(
  theme = shinytheme("cyborg"),
    titlePanel("My BJJ Data Analysis"),
  
    mainPanel(
      textOutput("txtOutput")
      ),
  
  
      
      fluidRow(hr(),
        column(12,
          plotlyOutput("plot1"),
          hr())),

      fluidRow(
        column(6,plotlyOutput("plot2")),
        column(6,plotlyOutput("plot3"))
        ),

      fluidRow(hr(),
        column(6,plotlyOutput("plot4")),
        column(6,plotlyOutput("plot5"))
        ),
      
      fluidRow(hr(),
        column(12,plotlyOutput("plot6",height="700px"))
        ) 
  
  )





server <- function(input, output, session) {

  output$txtOutput= renderText({
    paste0("By: Alex Totten")
  })

  output$plot1 <- renderPlotly({
    
    
    a<-ggplot(df2,aes(x=week,y=hours,label=comment, group=1))+
      geom_line(data=df2,aes(color=belt),size=1)+
      geom_point(data=df2,aes(color=belt), size=3)+
      geom_vline(data=subset(df2, !is.na(df2$comment)),aes(xintercept=week,color="red"),   # Ignore NA values for mean
                 linetype="dashed", size=.5, na.rm=T)+
      ggtitle("Hours of Training Per Week")+
      labs(x="Week", y="Hours")+
      scale_color_manual(name = 'Legend', 
                         values =c('white'='white','blue'='blue', 'red'='red'))+
      theme(panel.background = element_rect(fill = "transparent",colour = NA),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(linetype = "dashed",color="grey"),
            axis.title.x = element_text(size=15,color="grey"),
            axis.title.y = element_text(size=15,color="grey"),
            axis.text.x = element_text(size=11,color="grey"),
            axis.text.y = element_text(size=11,color="grey"),
            title=element_text(size=18, color="grey"),
            legend.title=element_text(size=13,colour="grey"),
            legend.text = element_text(color='grey'),
            legend.background = element_rect(fill="transparent",size=0.5, linetype="solid", colour =NA),
            legend.key = element_rect(colour = 'dimgrey', fill = 'dimgrey', size = 0.5, linetype='dashed'))
    
    a<-ggplotly(a)
    a[['x']][['data']][[1]][['name']] <- 'Blue Belt'
    a[['x']][['data']][[2]][['name']] <- 'White Belt'
    a[['x']][['data']][[3]][['name']] <- 'Event'
    a%>%
      layout(plot_bgcolor='black') %>% 
      layout(paper_bgcolor='black')
    
    
  })

  output$plot2<-renderPlotly({
    plot_ly(data=as.data.frame(table(df$technique_type)), labels = ~Var1, values = ~Freq, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            showlegend = FALSE) %>%
      layout(title = 'Technique Type',font=list(size=13),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
      layout(plot_bgcolor='black') %>% 
      layout(paper_bgcolor='black')
  })
  
  output$plot3<-renderPlotly({
    plot_ly(data=as.data.frame(table(df$position)), labels = ~Var1, values = ~Freq, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            showlegend = FALSE) %>%
      layout(title = 'Position',font=list(size=13),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
      layout(plot_bgcolor='black') %>% 
      layout(paper_bgcolor='black')
  })
  
  output$plot4<-renderPlotly({
    plot_ly(data=as.data.frame(table(df$instructor)), labels = ~Var1, values = ~Freq, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            showlegend = FALSE,textposition="inside") %>%
      layout(title = 'Instructor',  font=list(size=13),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
      layout(plot_bgcolor='black') %>% 
      layout(paper_bgcolor='black')
  })
  
  output$plot5<-renderPlotly({
    plot_ly(data=as.data.frame(table(df$type)), labels = ~Var1, values = ~Freq, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            showlegend = FALSE,textposition="inside") %>%
      layout(title = 'Grappling Type',  font=list(size=13),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
      layout(plot_bgcolor='black') %>% 
      layout(paper_bgcolor='black')
  })
  
  output$plot6<-renderPlotly({
  
    b<-ggplot(data = filter(df, !name_technique %in% c("Open roll")), aes(x=name_technique, y=hours, fill=technique_type, group=1))+
      geom_bar(stat = "identity",width = 1)+
      ggtitle("Techniques")+
      labs(x="Technique Name", y="Hours", color='grey')+
      scale_fill_discrete(name = "Technique Type")+
    theme(panel.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = "dashed", color="grey"),
          axis.title.x = element_text(size=18,colour="grey"),
          axis.title.y = element_text(size=18,colour="grey"),
          axis.text.x = element_text(size=9, angle=90, hjust = 1, vjust=.2,colour="grey"),
          axis.text.y = element_text(size=10,colour="grey"),
          title=element_text(size=20,colour="grey"),
          legend.background = element_rect(fill = "transparent"),
          legend.key = element_rect( size = 0.5, linetype='dashed',colour="grey"),
          legend.title=element_blank(),
          legend.text = element_text(color='grey'),
          plot.margin = unit(c(1,1,1,1.5), "cm"))
    
    ggplotly(b)%>%
      layout(margin = list(b = 300), xaxis = list(tickangle = -60))%>%
    layout(plot_bgcolor='black') %>% 
      layout(paper_bgcolor='black',legend = list(orientation = 'h',y = 1.1, x = 0))
  })

}





shinyApp(ui = ui, server = server)