library(shiny)
library(vabidimcont)

# partea de UI
ui <- fluidPage(
    # titlu
    titlePanel("V.a. bidimensionale continue")
)

# partea de Server
server <- function(input, output)
{

}

# UI + server => ShinyApp
shinyApp(ui = ui, server = server)
