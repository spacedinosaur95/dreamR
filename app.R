library(tm)
library(wordcloud)
library(shiny)
library(plotly)

clean.text = function(x)
{
  
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

cleanTextDay <- clean.text("i woke up went to research design and worked on senior design during research design
i logged into JIRA and checked the user stories that were in sprint planning and looked at 
the sprint backlog to make sure that there were not any pressing user stories left at the top
of the product backlog and then i worked on documentation for senior design and i worked on 
our end of sprint demo presentation for senior design and looked over JIRA again and made sure 
that all of the user stories had acceptance criteria after research design i went to the human 
factors department and consulted with my human factors professors on the design of the user 
interface of the product for my senior design project after that i went to the engineering 
building and stopped by the lab and went to senior design and our demo presentation went well
and then i worked on other classwork that had deadlines coming up and talked with my senior 
design team about sprint planning ")
vectorDay <- paste(cleanTextDay,collapse=" ")
remwords <- c("that","and","the")
vectorDay <- removeWords(vectorDay,c(stopwords("english"),remwords))

cleanTextDream <- clean.text("i dreamed that my senior design team started the sprint without me and that they did sprint planning 
without me and because they did sprint planning without me they did not make sure that all the user 
stories got into the sprint backlog and that not all the user stories had acceptance criteria and that 
they would be changing the sprint backlog after the sprint had started which is really bad because 
it changed the scope of the sprint and we were going to fail the sprint because i wasnt being 
a good leader and i failed and because I failed the team failed and because I failed to communicate with my team")
vectorDream <- paste(cleanTextDream,collapse=" ")
remwords <- c("that","and","the")
vectorDream <- removeWords(vectorDream,c(stopwords("english"),remwords))

mood <- c(2, 2, 4, 3, 3, 4)
hoursOfSleep <- c(6, 6.5, 8, 7, 7.5, 8)
x = c("10/23/2016", "10/24/2016", "10/25/2016", "10/26/2016"
      , "10/27/2016", "10/28/2016")
sleepmood <- data.frame(x, hoursOfSleep, mood)

# establishing the server component 
server <- function(input, output, session){ 
  
  #the day before word cloud
  output$plot1 <- renderPlot({
    wordcloud(vectorDay, scale=c(6,0.7), max.words=150, 
              random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))
  })
  # the dream after word cloud
  output$plot2 <- renderPlot({
    wordcloud(vectorDream, scale=c(6,0.7), max.words=150, 
              random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))
  })
  #showing the table
  output$plot3 <- renderPlotly({
    
    x <- c("10/24", "10/25", "10/26", "10/27")
    y1 <- c(6, 6.5, 8, 7.5) #hours of sleep
    y2 <- c(-1, 0, 0, 0) #nightmare
    y3 <- c(0, 0, -1, 0) #reoccuring nightmare
    y4 <- c(3, 3, 4, 4) #mood
    
    data <- data.frame(x, y1, y2, y3, y4)
    
    p <- plot_ly(data, x = ~x, y = ~y1, type = 'bar', name = 'Hours of Sleep') %>%
      add_trace(y = ~y2, name = 'Nightmare') %>%
      add_trace(y = ~y3, name = 'Reoccuring Nightmare') %>%
      add_trace(y = ~y4, type = 'scatter', mode = "lines", name = 'Mood') %>%
      layout(title = 'Relative Barmode',
             xaxis = list(title = 'X axis'),
             yaxis = list(title = 'Y axis'),
             barmode = 'relative')
})
}

ui <- fluidPage(
  
  h1("dreamR"),
  
  # make sidebar panel
  sidebarPanel( 
    # dates to look at because of indication of anxiety/depression/PTSD
    
    "dreamR analyzes text entries to help end users understand their dreams. Additionally, dreamR compares mood and 
    sleep data and indicates if the user had a nightmare or a reoccurring nightmare. The purpose of dreamR is to help 
    end users monitor the aspects of sleep and dreams that indicates anxiety, depression, and PTSD so that they can better 
    overcome traumatic events, or even for people to simply take care of their mental health from monitoring dreams and 
    sleep."
    
  ), 
  
  # make a main panel
  mainPanel(
    # making tabs to click on to see str, plot, and table each in one tab... "tabset"
    tabsetPanel(
      tabPanel("wordcloud", 
               h3("Day Entry"),
               plotOutput("plot1"),
               h3("Dream Entry"),
               plotOutput("plot2")
      ),
      tabPanel("sentiment"
               #sentiment analysis of day and dream entries
      ),
      tabPanel("sleep and mood", 
                   plotlyOutput('plot3'))
      )
    )
  )


# calling the shiny app, the UI and the server
shinyApp(ui, server)
