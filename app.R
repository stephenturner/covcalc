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
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Coverage / Read Count Calculator"),

   h4("Calculate how much sequencing you need to hit a target depth of coverage (or vice versa)."),

   HTML("<p><strong>Instructions:</strong> set the read length/configuration and genome size, then select what you want to calculate.</p>"),

   HTML("<p>Written by <a href='http://stephenturner.us', target='blank'>Stephen Turner</a>, based on the <a href='http://www.ncbi.nlm.nih.gov/pubmed/3294162' target='_blank'>Lander-Waterman formula</a>, inspired by <a href='http://core-genomics.blogspot.com/2016/05/how-many-reads-to-sequence-genome.html' target='_blank'>a similar calculator</a> written by James Hadfield. Coverage is calculated as <em>C=LN/G</em> and reads as <em>N=CG/L</em> where <em>C</em> = Coverage (X), <em>L</em> = Read length (bp), <em>G</em> = Haploid genome size (bp), and <em>N</em> = Number of reads.</p>"),

   # HTML("<p>Written by <a href='http://stephenturner.us', target='blank'>Stephen Turner</a>, based on the <a href='http://www.ncbi.nlm.nih.gov/pubmed/3294162' target='_blank'>Lander-Waterman formula</a>, inspired by <a href='http://core-genomics.blogspot.com/2016/05/how-many-reads-to-sequence-genome.html' target='_blank'>a similar calculator</a> written by James Hadfield.</p><p>Coverage is calculated as <em>C=LN/G</em> and reads as <em>N=CG/L</em> where
   #        <ul>
   #          <li><em>C</em> = Coverage (X)</li>
   #          <li><em>L</em> = Read length (bp)</li>
   #          <li><em>G</em> = Haploid genome size (bp)</li>
   #          <li><em>N</em> = Number of reads</li>
   #        </ul>
   #      </p>"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(

        numericInput("readlength", label = h4("Read length (bp)"), value = 125),
        radioButtons("sepe", label = NA,
                     choices = list("Paired-end" = 2, "Single-end" = 1),
                     selected = 2),

        numericInput("genomesize", label = h4("Genome size (bp)"), value = 3.2e9),
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

        wellPanel(
             radioButtons("whatcalc",
                          label=h3("What do you want to know?"),
                          choices=list("# Reads (how many reads do I need to hit a target depth of coverage?)"="reads",
                                       "Coverage (what's my coverage depth obtained from a set number of reads)"="coverage"),
                          selected="reads"),
             # hr(),
             # Dynamic input goes here
             uiOutput("ui")
        ),

        h1(textOutput("text")),

        # plotOutput("myplot"),

        tags$br()

      )
   )

)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$ui <- renderUI({
    if (is.null(input$whatcalc)) {
      return()
    } else {
      switch(input$whatcalc,
             "reads"=sliderInput("dynamic", h4("Desired coverage"), min=10, max=150, value=30, step=5, post="x"),
             "coverage"=sliderInput("dynamic",
                                    label=h4("Number reads sequenced (millions)"),
                                    min=1, max=1001, value=384,
                                    post=paste0("M "))
      )
    }
  })

  # Converts genome size into SI-prefix character
  bptosi <- function(bp) {
    if (bp>1e9)      return(paste0(round(bp/1e9, 3), "GB"))
    else if (bp>1e6) return(paste0(round(bp/1e6, 3), "MB"))
    else if (bp>1e3) return(paste0(round(bp/1e3, 3), "KB"))
    else             return(paste0(bp, "bp"))
  }

  outputtext <- reactive({
    req(input$whatcalc)
    if (input$whatcalc=="reads") {
      paste0("Calculating reads", input$dynamic)
    } else if (input$whatcalc=="coverage") {
      paste0("Calculating coverage", input$dynamic)
    }
  })

  output$text <- renderText({
    req(input$whatcalc)
    if (input$whatcalc=="reads") {
      # N=CG/L
      reads <- (as.numeric(input$dynamic)*as.numeric(input$genomesize))/(as.numeric(input$readlength)*as.numeric(input$sepe))
      paste0("*", signif(reads/1e6, 3), " million reads* required for ", input$dynamic, "X coverage of a ", bptosi(input$genomesize), " genome using ", input$sepe, "x", input$readlength, " sequencing reads.")
    } else if (input$whatcalc=="coverage") {
      # C=LN/G
      coverage <- (as.numeric(input$readlength)*as.numeric(input$sepe))*as.numeric(input$dynamic)*1e6/as.numeric(input$genomesize)
      paste0("*", round(coverage), "X coverage* for a ", bptosi(input$genomesize), " genome obtained with ", input$dynamic, "M ", input$sepe, "x", input$readlength, " sequencing reads.")
    }
  })

  output$myplot <- renderPlot({
    req(input$dynamic)
    plot(input$dynamic)
  })

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

