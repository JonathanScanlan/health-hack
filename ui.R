

library(shiny)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).


# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Gene Search"),
    
    sidebarLayout(position = "right",      
      
      # Define the sidebar 
      sidebarPanel(
        selectInput("region", label=h4("Region:"), 
                    choices=list("United States"=1,"Germany"=2),selected=1),
	  textInput("GenText", label = h4("Genes"), value = ""),

	  dateRangeInput("dates", label = h4("Dates of Last Contact")),

        helpText("1. Please select a region"),
        helpText("2. Type the gene codes separated by a comma"),
        helpText("3. Select a date range.")
      ),
      
      mainPanel(
	tabsetPanel(
      tabPanel("Map", fluidRow(plotOutput("Mapped",width="100%"))),
	tabPanel("Labs", fluidRow(tableOutput("Labs")))
    )
          

      )
      
    )
  )
)
