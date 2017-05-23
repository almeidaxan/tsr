library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Time Series Retrieval"),
    tabsetPanel(
    	tabPanel(
    		"Input",
    		br(),
    		column(
    			width = 6,
	    		selectInput("arquivo", "Choose input data:", choices = arq),
	    		actionButton("botaoLeitura", "Read Data", icon = icon("download", lib="font-awesome")),
    			helpText("Press the button 'Read Data' to read the file with extension '.grd' selected above in the drop-down menu.")
    		),
        	column(
        		width = 6,
        		h3("File preview:"),
        		plotOutput("plot_teste", width = 400, height = 400)
        	)
    	),
        tabPanel(
          "Exploratory",
          br(),
          fluidRow(
          	column(
          		width = 6,
          		h3("Region of interest:"),
          		plotOutput("plot_1", width = 400, height = 400)
          	),
          	column(
          		width = 6,
          		h3("Corresponding (x,y) time series:"),
          		plotOutput("plot_2", width = 400, height = 400)
          	)
          ),

          hr(),

          fluidRow(
            column(
            	width = 6,
					wellPanel(
						h4("Choose the time (t):"),
						uiOutput("outputSlider_1")
					)
            ),
            column(
            	width = 6,
						wellPanel(
							h4("Choose a pair of coordinates (x,y):"),
							uiOutput("outputSlider_2"),
							uiOutput("outputSlider_3")
						)
            )
          )
        ),
        tabPanel(
          "Retrieval",
          br(),
          fluidRow(
            column(
            	width = 6,
					wellPanel(
						selectInput("algoritmo", "Choose the distance/similarity measure:",
						            c("Manhattan Distance",
						              "Euclidian Distance",
						              "Chebyshev Distance",
						              "DTW Distance",
						              "Cosine Similarity"
						              )
						)
					)
				),
				column(
					width = 4,
					wellPanel(
						radioButtons("tipo_entrada", "Type of input:",
										 c("Click on the image",
										   "Specify the point (x,y)")
						)
					)
				),
				column(
					width = 2,
					actionButton("botaoConsulta", "Retrieve", width = 100, icon = icon("search", lib="font-awesome"))
				),

				br(),

				column(
					width = 6,
					conditionalPanel(
						condition = "input.tipo_entrada == 'Specify the point (x,y)'",
						h3("Input"),
						h4("Choose a pair of coordinates (x,y):"),
						uiOutput("outputSlider_4"),
						uiOutput("outputSlider_5")
					),
					conditionalPanel(
						condition = "input.tipo_entrada == 'Click on the image'",
						h3("Input"),
						plotOutput("plot_3", width = 400, height = 400, click = "plotClick_3")
					)
				),
				column(
					width = 6,
					h3("Output"),
					plotOutput("plot_4a", width = 400, height = 400)
				),
				column(
					width = 12,
					conditionalPanel(
						condition = "input.tipo_entrada == 'Click on the image'",
							br(),
							hr(),
							br(),
							verbatimTextOutput("outputPlotClick")
					)
				)
          )
        )
      )
    )
)