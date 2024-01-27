library(shiny)
library(pracma)
library(animate)
library(vabidimcont)

# partea de UI
ui <- fluidPage(

    # titlu
    titlePanel(h3("V.a. bidimensionale continue",align= "center")),

    sidebarLayout(
      sidebarPanel(
        textInput("finput", "Functia f:", "2*x+10/y+x"),
        textInput("lx", "Limita inferioara x:", 0),
        textInput("ux", "Limita superioara x:", 1),
        textInput("ly", "Limita inferioara y:", 0),
        textInput("uy", "Limita superioara y:", 1),
        selectInput("action_dropdown", "Optiuni:",
                    choices = c("Test Fubini", "Alta")),
        actionButton("run", "Run")
      ),

      mainPanel(
        textOutput("rezultat"),
        plotOutput("plot")
      )
    )
)

# partea de Server
server <- function(input, output)
{

  observeEvent(input$run, {
    optiune <- input$action_dropdown

    # obtinem functia introdusa de la tastatura
    input_f <- parse(text = input$finput)

    # definim functia bidimensionala
    f <- function(x, y) eval(input_f, list(x = x, y = y))

    if (optiune == "Test Fubini") {
      # Test Fubini

      output$rezultat <- renderText({
        paste("f(3, 7) =", f(3, 7))
      })

    } else if (optiune == "") {

    }
  })
}

# UI + server => ShinyApp
shinyApp(ui = ui, server = server)
