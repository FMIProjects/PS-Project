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
        textOutput("rezultat1"),
        textOutput("rezultat2"),
        textOutput("rezultat3"),
        plotOutput("plot")
      )
    )
)

# partea de Server
server <- function(input, output)
{

  observeEvent(input$run, {
    output$rezultat1 <- renderText({paste("")})
    output$rezultat2 <- renderText({paste("")})
    optiune <- input$action_dropdown

    # obtinem functia introdusa de la tastatura
    input_f <- parse(text = input$finput)
    # obtinem borderele
    lx <- as.numeric(input$lx)
    ux <- as.numeric(input$ux)
    ly <- as.numeric(input$ly)
    uy <- as.numeric(input$uy)

    # definim functia bidimensionala
    f <- function(x, y) eval(input_f, list(x = x, y = y))

    # test limite
    if (lx > ux || ly > uy)
    {
      output$rezultat1 <- renderText({paste("Limite introduse incorect!")})
    }
    else
    {
     if (optiune == "Test Fubini")
     {
      # Test Fubini
      if(ftestFubini(f,lx,ux,ly,uy))
        {
        output$rezultat1 <- renderText({paste("Test Fubini : DA")})

        integrala_dubla <- integral2(f,lx,ux,ly,uy)
        output$rezultat2 <- renderText({paste("Rezultat Integrala : ",integrala_dubla$Q)})
        }
      else
        {
        output$rezultat1 <- renderText({paste("Test Fubini : NU")})
        }

    } else if (optiune == "")
      {

      }
   }
  })
}

# UI + server => ShinyApp
shinyApp(ui = ui, server = server)
