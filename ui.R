library("shiny")
# library("plotly")
library(shinythemes)
# library("threejs")

shinyUI(fluidPage(theme = shinytheme("spacelab"),
  titlePanel(h1("FazPlot 3D - FazDev")),
  fluidRow(
    column(3,
      conditionalPanel(condition="$('li.active a').first().html() === '3Drug DataPlot'",
      wellPanel(
        fileInput('file2','Please Choose CSV File:', accept=c('text/csv','text/comma-separated-value','test/plain')),
        radioButtons('fty','Pasted data OR the uploaded file format',choices=list("Comma deliminated"='csv',"Tab deliminated"='tab'),selected='csv')
      ),
      wellPanel(
        selectInput("btytype", "Select type of the box",choices = list("full box" = "f", "back panels visibl" = "b", "back panels and grid lines" = "b2", "grey background with white gridlines" = "g", "has a black background" = "bl", "black background with grey lines" = "bl2"), selected = "b2"),
        sliderInput("theta", "Plot Rotation - x-Axis", min=0, max=360, value=135,step= 1),
        column(6,
        sliderInput("phi", "(transformed) z-axis", min=0, max=90, value=15,step= 1),
        sliderInput("yexpand", "x-axis expantion:", min=0.2, max=1.5, value=0.5,step= 0.1)
        ),column(6,
        sliderInput("dotsize", "3D XYZ dot size:", min=0, max=5, value=2,step=0.5),
        sliderInput("aixdotsize", "Axis dot size:", min=0, max=5, value=0.5,step=0.5)
        ),
        column(12,
        hr()
        ),
        column(6,
        radioButtons('showlines','Add line',choices=list("hide line"=0,"show line"=1),selected=0)
        ),column(6,
        radioButtons('pointtext','Add Colkey',choices=list("hide lables"=0,"show lables"=1),selected=0)
        ),
        column(6,
        radioButtons('showcolkey','Show colkey',choices=list("Hide colkey"=0,"Show colkey"=1),selected=0)
        ),column(6,
        radioButtons('showotherplots','Other dataplots',choices=list("hide"=0,"show"=1),selected=1)
        ),
        column(6,
        radioButtons('axisticks','Axis ticks',choices=list("Axis arrows"=0,"detailed"=1),selected=1)
        ),column(6,
        radioButtons('showovlines','Vertical data lines',choices=list("hide"=0,"show"=1),selected=1)
        ),
        conditionalPanel(
            condition = "input.showlines == 1",
              sliderInput("linesmooth", "Smoothness of the line", 
                    min=0, max=1, value=0.25,step= 0.01)
        ),
        textInput("ptitle", "Plot title", value = "3D data sample plot"),
        textInput("xlab", "X axis lable", value = ""),
        textInput("ylab", "y aixs lable", value = ""),
        textInput("zlab", "z axis lable", value = "")
      )
      ),
      
      conditionalPanel(condition="$('li.active a').first().html() === 'Checkerboards'",
      wellPanel(
        # h4("Create Checkerboards"),
        # h4("Enter data"),
        radioButtons("dataInput", "Data input type", list("Load sample data"=1,"Upload file"=2,"Paste data"=3),selected=3),
        conditionalPanel(condition="input.dataInput=='1'",
                        radioButtons("sampleData", "Load sample data", list("Sample data 1"=1,"Sample data 2"=2),selected=2)
        ),
        conditionalPanel(condition="input.dataInput=='2'",
                        h5("Upload delimited text file: "),
                        fileInput("upload", "", multiple = FALSE),
                        checkboxInput("fileHeader", "File with row and column concentrations", FALSE),
                        radioButtons("fileSepDF", "Delimiter:", list("Comma"=1,"Tab"=2,"Semicolon"=3)),#, "Space"=4))
                        HTML('<p>Data in <a href="http://en.wikipedia.org/wiki/Delimiter-separated_values">delimited text files </a> can be separated by comma, tab or semicolon. 
        For example, Excel data can be exported in .csv (comma separated) or .tab (tab separated) format. </p>')
        ),
        conditionalPanel(condition="input.dataInput=='3' ||input.dataInput=='1'",
                        # strong("Pasted data seperator"),
                        radioButtons("fileSepP", "Separator:", list("Comma"=1,"Tab"=2),selected=1),
                        strong("Data column format"),
                        checkboxInput("xyzcolmdata", "Convert X,Y,Z column to matrix", FALSE),
                        checkboxInput("smoothmatrix", "Matrix smoothness", FALSE),
                        conditionalPanel(condition="input.xyzcolmdata",
                          radioButtons("graphori", "Change Graph data orientation", list("XYZ"=1,"ZXY"=2,"YZX"=3),selected=1),
                          radioButtons("inity", "e interpolation type", list("Type 1"=1, "Type 2 (smoother)"=2,"Type 3 (NA's removed)"=3),selected=1)
                        )
        )     
        ),
        wellPanel(
        strong("Data orientation"),              
        checkboxInput("inverseData", "Inverse data matrix", FALSE),
        checkboxInput("flipDataX", "Flip data by x-axis", FALSE),
        checkboxInput("flipDataY", "Flip data by y-axis", FALSE),
        strong("Color Setting"),  
        checkboxInput("singlec", "Make graph single color", FALSE),
        radioButtons("plotType", "Select Plot type", list("3D"=0, "3D Histogram"=2,"3D Ribbon"=3,"3D Mesh"=4,"2D"=1,"2D Contours"=5)),
        conditionalPanel(condition="input.plotType=='0' || input.plotType=='2' || input.plotType=='3'|| input.plotType=='4'",
          selectInput("btytype2", "Select type of the box",choices = list("full box" = "f", "back panels visibl" = "b", "back panels and grid lines" = "b2", "grey background with white gridlines" = "g", "has a black background" = "bl", "black background with grey lines" = "bl2"), selected = "b2"),
          radioButtons('colkey2','Add point lables',choices=list("Hide colkey"=0,"Show colkey"=1),selected=0),
          conditionalPanel(condition="input.plotType=='4'",
            sliderInput("meshspace", 
                     "Mesh space:", 
                     min = 0.01, 
                     max = 0.9, 
                     value = 0.5)
          ),
          sliderInput("plottrans", 
                     "Transparency:", 
                     min = 0.1, 
                     max = 1, 
                     value = 0.8),
         h5("Rotate view:"),
         sliderInput("obs", 
                     "Horizontal:", 
                     min = 1, 
                     max = 360, 
                     value = 135),
         sliderInput("obsv", 
                     "Vertical:", 
                     min = 1, 
                     max = 360, 
                     value = 25)                  
        ),
        conditionalPanel(condition="input.plotType=='1'",
                         h5("Algorithm choices:"),
                         radioButtons("otherPlotType", "", list("Bliss"=0, "HSA"=1, "Data"=2)),
                         h5("Schema for Synergy or Antagonism:"),
                         radioButtons("myOrientation", "", list("Synergism"=0, "Antagonism"=1))
                         
        ),
        checkboxInput("labelsTitle", "Modify labels and title", FALSE),
        conditionalPanel(condition="input.labelsTitle",
                         textInput("myXlab", "X-axis label:", value=c("Drug A")),
                         textInput("myYlab", "Y-axis label:", value=c("Drug B")),
                         textInput("myZlab", "Z-axis label:", value=c("Absorbance")),
                         textInput("myTitle", "Title:", value=c(""))
     
        ),
        checkboxInput("plotSize", "Adjust plot size", FALSE),
        conditionalPanel(condition="input.plotSize",
                         sliderInput("myHeight", "Plot height:", min = 300, 
                     max = 1200, value=800),
                         sliderInput("myWidth", "Plot width:", min = 400, 
                     max = 1600, value=1100)
        ),
        checkboxInput("fontSizes", "Change font sizes", FALSE),
        conditionalPanel(condition="input.fontSizes",
                         numericInput("cexTitle", "Title font size:", value=10),
                         numericInput("cexAxislabel", "Axis label size:", value=10),
                         numericInput("cexAxis", "Axis font size:", value=10)
        )
      )
      )
    ),
    column(9,
      tabsetPanel(
        # tabPanel("Data input", 
        #   h3("Paste Data (comma-separated-values with headers)"),
        #     tags$textarea(id="datainput", rows=10, cols=120),
        #     br(),
        #     actionButton("submitclick", strong("Load & Check data table")),actionButton('clearText_button1','Clear data'),
        #     hr(),
        #     tableOutput("submitdata_table")
        # ),
        tabPanel("3Drug DataPlot", 
          h3("Paste Data (comma-separated-values with headers)"),
            tags$textarea(id="datainput", rows=10, cols=120),
            br(),
            # actionButton("submitclick", strong("Load & Check data table")),
            actionButton('clearText_button1','Clear data'),
            actionButton('load_sample_data_scatter','Load sample data'),
            hr(),br(),
            downloadButton("downloadPlotEPS1", "Download eps-file"),
            downloadButton("downloadPlotPDF1", "Download pdf-file"),
            downloadButton("downloadPlotSVG1", "Download svg-file"),
            hr(),
            plotOutput("scatterplot2", width="100%", height="800px"),
            hr(),
            #scatterplotThreeOutput("scatterplot2", width="100%", height="800px"),
            plotOutput("scatterplot1", width="100%", height="400px"),
            hr(),
            # plotlyOutput("scatterplot3", width="100%"),
            fluidRow(
              column(6, 
                verbatimTextOutput('dconsole')
                ),
              column(6, 
                tableOutput("data_table")
                )
            )
        ),
        tabPanel("Checkerboards", 
          br(),
            #scatterplotThreeOutput("scatterplot2", width="100%", height="800px"),
            conditionalPanel(condition="input.dataInput=='3'",
            h5("Paste data below:"),
                        #       HTML('<input type="button" value="Clear" onclick="javascript:eraseText();">'),
                        #         HTML('<textarea id='output' rows=20 cols=90></textarea>'),
                        #         HTML('<textarea id="output" rows="3" cols="40">Default value</textarea>'),
            tags$textarea(id="myData", rows=10, cols=120, ""),br(),
            actionButton('clearText_button','Clear data'),
            actionButton('load_sample_data_checker','Load sample data'),
            hr()
            ),
            downloadButton("downloadPlotEPS", "Download eps-file"),
            downloadButton("downloadPlotPDF", "Download pdf-file"),
            downloadButton("downloadPlotSVG", "Download svg-file"),
            hr(),
            # fluidRow(
            #   column(9, 
            #     plotOutput("rawPlot", height='100%', width='100%')
            #     )
            #   column(3, 
            #     h4("Data statistics"), 
            #     tableOutput("checkerboardStatsTable")
            #     )
            # ),
            plotOutput("rawPlot", height='100%', width='100%'),
            hr(),
            tableOutput("filetable"),verbatimTextOutput('dconsole2')
            
        ),
        # plotOutput("scatterplot"),
        # scatterplotThreeOutput("scatterplot", width="100%", height="800px"),
        # numericInput("sizes", "Number of sizes (only affects canvas renderings)", 2, min = 1, max = 3, step = 1)
        hr(),
        HTML('<div>copyright &copy; 2015-2016 - IIDR Bioinformatics 3D Data plot v1.2 - Fazmin</div>')
      ),id="tabs1"
    )
  )
))