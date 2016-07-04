# 11-ui.r
library(shiny)

shinyUI(fluidPage(

	titlePanel("Uploading Files"),

 	sidebarPanel(
 	    uiOutput("inpFileName"),
		tags$hr(),
		tags$b(p("File Info")),
		textOutput('LineCountText'),
		textOutput('TotalWordCount'),
		textOutput('AvgWordsPerLine'),
		tags$hr(),
		tableOutput('LineCountEmotions'),
		tableOutput('LineCountPolarity')
    ),

    mainPanel(
      	dataTableOutput('ShowData'),
      	plotOutput('HistSigWords'),
      	plotOutput('CloudSigWords'),
      	plotOutput('HistEmotions'),
      	plotOutput('HistPolarity')
    )

))