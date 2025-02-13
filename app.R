library(shiny)
library(dplyr)

ui <- fluidPage(
    titlePanel(
        div("Tratamento Dados CRM", style = "text-align: center;")
    ),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Escolha um arquivo CSV", accept = ".csv"),
            downloadButton("downloadData", "Baixar Arquivo Tratado")
        ),
        mainPanel(
            tableOutput("table")
        )
    )
)

server <- function(input, output) {
    dados_tratados <- reactive({
        req(input$file1)
        
        # Lê o arquivo CSV
        dados <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
        
        # Tratamento dos dados
        dados <- dados %>% select(c('customer_name', 'customer_email', 'tracking_code'))
        dados$customer_name <- sub(" .*", "", dados$customer_name)
        dados$customer_name <-stringr::str_to_title(dados$customer_name)
        dados <- dados %>% group_by(customer_name, customer_email) %>% 
            mutate(id_rank = row_number()) %>% 
            tidyr::pivot_wider(names_from = id_rank, values_from =tracking_code, names_prefix = 'tracking_code')
        return(dados)
    })
    
    output$table <- renderTable({
        dados_tratados()
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            "dados_tratados.csv"
        },
        content = function(file) {
            write.csv(dados_tratados(), file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)
