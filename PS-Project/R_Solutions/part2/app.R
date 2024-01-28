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
      sidebarPanel(width=6,
        tabsetPanel(type = "tabs",
                    tabPanel("1",
                             textInput("finput", "Functia f:", "1/5*(x+y+1)"),
                             radioButtons("dim", "Dimensiune:",
                                          choices = c("Unidimensionala", "Bidimensionala"),
                                          selected = "Bidimensionala"),
                             fluidRow(
                               column(6,textInput("lx", "Limita inferioara x:", 0)),
                               column(6,textInput("ux", "Limita superioara x:", 1))
                             ),
                             fluidRow(
                               column(6,textInput("ly", "Limita inferioara y:", 0)),
                               column(6,textInput("uy", "Limita superioara y:", 2))
                             )),
                    tabPanel("2",fluidRow(
                      column(6,textInput("valx", "Valoare pentru x:", 0)),
                      column(6,textInput("valy", "Valoare pentru y:", 0))
                    ))
        ),
        selectInput("action_dropdown", "Optiuni:",
                    choices = c("Test Fubini",
                                "Interpretare Geometrica",
                                "Densitate marginala X",
                                "Densitate marginala Y"
                    )),
        actionButton("run", "Run"),
      ),

      mainPanel(width=6,
        tabsetPanel(type = "tabs",
        tabPanel("Text",
        span(textOutput("rezultat1"),style="font-size:30px;text-align:center;margin:10px"),
        span(textOutput("rezultat2"),style="font-size:30px;text-align:center;margin:10px"),
        span(textOutput("rezultat3"),style="font-size:30px;text-align:center;margin:10px")),
        tabPanel("Plot",
        rglwidgetOutput("plot1"),
        plotOutput("plot2"))
        )
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

    #obtinem valorile pentru x si y

    valx <- as.numeric(input$valx)
    valy <- as.numeric(input$valy)


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

      else if(optiune == "Test Densitate"){

        if(fvabidimdens(f,lx,ux,ly,uy)){
          output$rezultat1 <- renderText({paste("Test densitate : DA")})
        }

        else{
          output$rezultat1 <- renderText({paste("Test densitate : NU")})
        }

      }

      else if(optiune == "Densitate marginala X"){


        if(valx> ux || valx< lx){

          output$rezultat1 <- renderText({paste("Rezultat densitate X : 0 (valoarea lui x nu apartine intervalului dat)")})

        }else{
          marginalaX <- fXdens(f,valx,ly,uy)


          output$rezultat1 <- renderText({paste("Rezultat densitate X : ",marginalaX)})
        }

      }

      else if(optiune == "Densitate marginala Y"){


        if(valy> uy || valy< ly){

          output$rezultat1 <- renderText({paste("Rezultat densitate Y : 0 (valoarea lui y nu apartine intervalului dat)")})

        }else{

          marginalaY <- fYdens(f,valy,lx,ux)

          output$rezultat1 <- renderText({paste("Rezultat densitate Y : ", marginalaY)})

        }

      }

   }
  })
}

# UI + server => ShinyApp
shinyApp(ui = ui, server = server)




