library(shiny)
library(pracma)
library(animate)
library(vabidimcont)
library(rgl)

# partea de UI
ui <- fluidPage(

    # titlu
    titlePanel(h3("V.a. bidimensionale continue",align= "center")),

    sidebarLayout(
      sidebarPanel(
        textInput("finput", "Functia f:", "1/5*(x+y+1)"),
        radioButtons("dim", "Dimensiune:",
                           choices = c("Unidimensionala", "Bidimensionala"),
                           selected = "Bidimensionala"),
        textInput("lx", "Limita inferioara x:", 0),
        textInput("ux", "Limita superioara x:", 1),
        textInput("ly", "Limita inferioara y:", 0),
        textInput("uy", "Limita superioara y:", 2),
        selectInput("action_dropdown", "Optiuni:",
                    choices = c("Test Fubini", "Interpretare Geometrica","Testeaza Densitate")),
        actionButton("run", "Run")
      ),

      mainPanel(
        textOutput("rezultat1"),
        textOutput("rezultat2"),
        textOutput("rezultat3"),
        rglwidgetOutput("plot1"),
        plotOutput("plot2")
      )
    )
)

# partea de Server
server <- function(input, output)
{

  observeEvent(input$run, {
    output$rezultat1 <- renderText({paste("")})
    output$rezultat2 <- renderText({paste("")})
    output$plot1 <- renderRglwidget({})
    optiune <- input$action_dropdown

    # obtinem functia introdusa de la tastatura
    input_f <- parse(text = input$finput)
    # obtinem borderele
    lx <- as.numeric(input$lx)
    ux <- as.numeric(input$ux)
    ly <- as.numeric(input$ly)
    uy <- as.numeric(input$uy)

    # definim functia bidimensionala
    if (input$dim == "Bidimensionala")
      f <- function(x, y) eval(input_f, list(x = x, y = y))
    else if (input$dim == "Unidimensionala")
      f <- function(x) eval(input_f, list(x = x))

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

    } else if (optiune == "Interpretare Geometrica")
      {
      x <- seq(lx, ux, length = 30)
      y <- seq(ly, uy, length = 30)
      z <- outer(x,y,f)

      output$plot1 <- renderRglwidget({
        try(close3d())
        persp3d(x,y,z,col="red")
        rglwidget()
      })

    }

      else if(optiune == "Testeaza Densitate"){

        if(fvabidimdens(f,lx,ux,ly,uy)){
          output$rezultat1 <- renderText({paste("Test densitate : DA")})
        }

        else{
          output$rezultat1 <- renderText({paste("Test densitate : NU")})
        }

      }


   }
  })
}

# UI + server => ShinyApp
shinyApp(ui = ui, server = server)




