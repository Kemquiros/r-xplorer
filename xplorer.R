#-----------------
# Jimenez S.
# Tapias J.
# Curso: Exploración de gráficos multivariados
# Grupo: 6
# Tema: Shiny avanzado
# Fecha: 2019-11-23
#-----------------

library(shiny)
library(DT)
library(markdown)
library(rpart)
library(rpart.plot)



#shinyUI
ui <- navbarPage(
    "Xplorer",
    
    # Subir datos
    tabPanel(
        "Subir Datos",
        titlePanel("Subir Archivo CSV"),
        radioButtons(
            "separador",
            "Seleccionar Separador: ",
            choices = c(";", ",", ":"),
            selected = ";",
            inline = TRUE
        ),
        fileInput(
            'archivo_cargado',
            'Seleccionar archivo',
            accept = c('text/csv',
                       'text/comma-separated-values',
                       '.csv')
        ),
        DT::dataTableOutput("sample_table")),
    
    # Primera revisión de los datos
    tabPanel("Resumen",
             titlePanel("Resumen de los Datos"),
             verbatimTextOutput("summary"),
             sidebarPanel(
                 uiOutput('choose_var_hist'),
             ),
             mainPanel(
                 titlePanel("Histograma"),
                 plotOutput("plot_hist"))
             ),
    
    # Regresión árboles de decisión
    tabPanel("Regresión",
        titlePanel('Regresión Árbol de Decisión'),
        sidebarLayout(
            sidebarPanel(
                h3('Modelo de ingresos'),
                uiOutput('choose_y'),
                uiOutput('choose_x'),
                actionButton('c50', label = 'Generar modelo')
            ),
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Resumen", verbatimTextOutput('tree_summary')),
                            tabPanel("Gráfica", plotOutput('tree_plot'))
                )
            )
        )
    ),
    
    tabPanel("Guardar",
             titlePanel('Guardar estado de la sesión'),
             bookmarkButton())
    
)

#-------------------------------------------------------------------------------
# Shiny Server

server <- shinyServer(function(input, output) {
    df_upload <- reactive({
        inFile <- input$archivo_cargado
        if (is.null(inFile))
            return(NULL)
        df <-
            read.csv(inFile$datapath,
                     header = TRUE,
                     sep = input$separador)
        return(df)
    })
    
    output$sample_table <- DT::renderDataTable({
        df <- df_upload()
        DT::datatable(df)
    })
    
    output$summary <- renderPrint({
        df <- df_upload()
        if (is.null(df)) {
            return("Sin datos")
        } else {
            summary(df)
        }
    })
    
    output$choose_var_hist <- renderUI({
        df <- df_upload()
        if (is.null(df)) {
            return("Sin datos")
        } else {
            is_factor <- sapply(df, FUN = is.numeric)
            var_choices <- names(df)[is_factor]
            selectInput('choose_var_hist', label = 'Selecciona variable', choices = var_choices)
        }
    })
    
    output$plot_hist <- renderPlot({
        df <- df_upload()
        #n <- input$n
        if (is.null(df)) {
            return("Sin datos")
        } else if(is.null(input$choose_var_hist)) {
            return(NULL)
        } else {
            hist(df[, input$choose_var_hist],
                 main = "Histograma",
                 col = "#75AADB",
                 border = "white",
                 xlab=paste(input$choose_var_hist, sep="")
                 )
        }
    })
    
    output$choose_y <- renderUI({
        df <- df_upload()
        is_factor <- sapply(df, FUN = is.numeric)
        y_choices <- names(df)[is_factor]
        selectInput('choose_y', label = 'Selecciona variable objetivo', choices = y_choices)
    })
    
    output$choose_x <- renderUI({
        df <- df_upload()
        x_choices <- names(df)[!names(df) %in% input$choose_y]
        checkboxGroupInput('choose_x', label = 'Selecciona variables predictoras', choices = x_choices)
    })
    
    observeEvent(input$c50, {
        df <- df_upload()
        c50_fit <- rpart(as.formula(paste(isolate(input$choose_y), '~', paste(isolate(input$choose_x), collapse = '+'))), data = df)
        output$tree_summary <- renderPrint(summary(c50_fit))
        output$tree_plot <- renderPlot({
            rpart.plot(c50_fit)
            text(c50_fit, use.n=TRUE, all=TRUE, cex=.8)
        })
    })
})

enableBookmarking(store = "url")

# Run the application
shinyApp(ui = ui, server = server)
