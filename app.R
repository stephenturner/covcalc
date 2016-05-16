#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# App inspired by http://core-genomics.blogspot.com/2016/05/how-many-reads-to-sequence-genome.html

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Lander-Waterman Coverage / Read Count Calculator"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(

        numericInput("readlength", label = h4("Read length (bp)"), value = 125),
        radioButtons("sepe", label = NA,
                     choices = list("Paired-end" = 2, "Single-end" = 1),
                     selected = 2),

        numericInput("covg", label = h4("Genome size (bp)"), value = 3.2e9),
        p("Haploid genome size. Can use scientific notation (e.g., \"3.2e9\" = 3,200,000,000). Examples:"),
        HTML("<ul>
             <li>Human genome: 3.2e9</li>
             <li>Agilent V6 exome: 60e6</li>
             <li>E. coli K-12: 4.6e6</li>
             <li>HIV: 9.2e3</li>
             </ul>")

      ),

      # Show a plot of the generated distribution
      mainPanel(
         fluidRow(
           column(3, wellPanel(
             radioButtons("whatcalc", label=NA, choices=list("Reads required"=1, "Coverage obtained"=2), selected=1),
             radioButtons("test2", label=NA, choices=list("test"=1), selected=1)
           )),
           column(6, wellPanel(
             # slider goes here
           ))
         )
      )

   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2]
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })


}

# Run the application
shinyApp(ui = ui, server = server)

