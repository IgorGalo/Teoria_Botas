# ==============================================================================
# PROJETO: Teoria das Botas de Sam Vimes (Dashboard Interativo)
# OBJETIVO: Analisar o comprometimento da renda em diferentes classes sociais.
# ==============================================================================

library(shiny)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. INTERFACE DO USUÁRIO (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      /* Largura equilibrada */
      .container-fluid { max-width: 1200px; margin: auto; }
      body { background-color: #f8f9fa; }

      #vimesPlot { cursor: pointer; }

      /* PAINEL CONCEITUAL: Elegante e integrado */
      .painel-teoria {
        background-color: #fcf8e3; 
        color: #8a6d3b;
        padding: 20px;
        border-radius: 8px;
        border: 1px solid #faebcc;
        margin-bottom: 25px;
        text-align: left;
        line-height: 1.5;
        font-size: 1.3em;
      }
      .painel-teoria h3 { margin-top: 0; font-weight: bold; color: #66512c; font-size: 1.5em; }

      .instrucao { 
        font-weight: bold; color: #2c3e50; background: #d1ecf1; 
        padding: 10px; border-radius: 5px; margin-bottom: 20px; font-size: 1.2em;
      }
      
      .nota-contexto {
        background-color: #e9ecef; color: #495057; padding: 25px;
        border-radius: 8px; border-left: 5px solid #007bff;
        margin-top: 20px; margin-bottom: 20px; 
        font-style: italic; font-size: 1.1em;
      }
      
      .titulo-app { 
        text-align: center; margin-top: 20px; margin-bottom: 15px; 
        color: #2c3e50; font-weight: bold; font-size: 3.0em; 
      }
    "))
  ),

  div(class = "titulo-app", "Teoria das Botas de Sam Vimes: Onde vai a Renda?"),
  
  fluidRow(
    column(width = 12,
      div(class = "painel-teoria",
          tags$h3("Você conhece a Teoria das Botas?"),
          tags$p("A Teoria das Botas de Sam Vimes, criada por Terry Pratchett em 'Homens de Armas' (Discworld), 
                 explica que a pobreza é cara. Pessoas pobres gastam mais dinheiro a longo prazo comprando 
                 itens baratos e de baixa qualidade que duram pouco, enquanto ricos compram itens duráveis, 
                 economizando no final. Este dashboard analisa como o custo de vida essencial consome 
                 proporcionalmente mais de quem ganha menos.")
      )
    )
  ),
  
  fluidRow(
    column(width = 12, align = "center",
      
      div(class = "instrucao", "👆 Clique em uma barra para detalhar a composição da Renda Total (Percentual)."),
      
      fluidRow(
        column(width = 7, plotOutput("vimesPlot", click = "plot_click", height = "500px")),
        column(width = 5, plotOutput("pizzaPlot", height = "500px"))
      ),
      
      hr(), 
      
      div(style = "font-size: 1.5em; font-weight: bold; margin-bottom: 15px; color: #2c3e50;", 
          "Tabela de Médias Mensais (R$)"),
      
      tableOutput("tabelaResumo"),
      
      div(class = "nota-contexto",
          "“Os valores podem parecer contraintuitivos em um contexto brasileiro, mas refletem a estrutura de consumo 
          de países em desenvolvimento como as Filipinas, onde alimentação ocupa maior parcela da renda e 
          transporte/moradia têm dinâmicas diferentes.”"),
      
      hr(),
      
      tags$footer(
        tags$p(style = "color: gray; font-style: italic; font-size: 0.9em; text-align: center; padding-bottom: 30px;",
          "Nota: Os dados originais da pesquisa FIES (Filipinas) registram rendimentos anuais. ",
          "Para esta análise, os valores foram convertidos de Pesos Filipinos (PHP) para Reais (BRL) ",
          "utilizando uma taxa estimada e divididos por 12 para refletir a realidade mensal brasileira."
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 2. LÓGICA DO SERVIDOR (SERVER)
# ------------------------------------------------------------------------------
server <- function(input, output) {
  
  dados_processados <- reactive({
    df <- read.csv("Income.csv", check.names = FALSE)
    taxa <- (0.09 / 12)
    
    df$Renda        <- df[['Total Household Income']] * taxa
    df$Alimentacao  <- df[['Total Food Expenditure']] * taxa
    df$Moradia      <- df[['Housing and water Expenditure']] * taxa
    df$Transporte   <- df[['Transportation Expenditure']] * taxa
    df$Saude        <- df[['Medical Care Expenditure']] * taxa
    df$Educacao     <- df[['Education Expenditure']] * taxa
    
    quebras <- quantile(df$Renda, probs = seq(0, 1, 0.2), na.rm = TRUE)
    df$Quintil <- cut(df$Renda, breaks = quebras, 
                      labels = c("20% Mais Pobres", "Pobre-Média", "Média", "Média-Alta", "20% Mais Ricos"),
                      include.lowest = TRUE)
    
    resumo <- aggregate(cbind(Alimentacao, Moradia, Transporte, Saude, Educacao, Renda) ~ Quintil, 
                        data = df, FUN = mean)
    
    resumo[, 2:7] <- lapply(resumo[, 2:7], function(x) round(x, -1))
    return(resumo)
  })

  # --- 2.1. Gráfico de Barras ---
  output$vimesPlot <- renderPlot({
    res <- dados_processados()
    res$GastoEssencial <- rowSums(res[, 2:6])
    res$PercTotal <- res$GastoEssencial / res$Renda
    
    ggplot(res, aes(x = Quintil, y = PercTotal)) +
      geom_col(fill = "#2a9d8f", alpha = 0.9, width = 0.7) + 
      geom_text(aes(label = scales::percent(PercTotal, accuracy = 1)), 
                vjust = -0.5, size = 5, fontface = "bold") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) +
      theme_minimal(base_size = 15) +
      theme(
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 15))
      ) +
      labs(title = "Percentual de Gastos de acordo com Renda", 
           y = "% da Renda Total Média\n", x = "\nFaixa de Renda")
  })

  # --- 2.2. Gráfico de Pizza ---
  output$pizzaPlot <- renderPlot({
    validate(need(input$plot_click, "Aguardando clique em uma barra..."))
    
    res <- dados_processados()
    lvls <- levels(res$Quintil)
    escolha <- lvls[round(input$plot_click$x)]
    if (is.na(escolha)) return(NULL)
    
    df_f <- res[res$Quintil == escolha, ]
    soma_5_itens <- sum(df_f[1, 2:6])
    restante <- max(0, df_f$Renda - soma_5_itens)
    
    df_p <- data.frame(
      Categoria = c("Alimentação", "Moradia", "Transporte", "Saúde", "Educação", "Não Gasto/Outros"),
      Valor = c(df_f$Alimentacao, df_f$Moradia, df_f$Transporte, df_f$Saude, df_f$Educacao, restante)
    )
    df_p$PercReal <- df_p$Valor / df_f$Renda
    
    df_p <- df_p[order(df_p$Valor, decreasing = TRUE), ]
    df_p$ymax <- cumsum(df_p$Valor)
    df_p$ymin <- c(0, head(df_p$ymax, n=-1))
    df_p$labelPosition <- (df_p$ymax + df_p$ymin) / 2
    
    # PALETA DE CORES MELHORADA: Contrastes fortes e profissionais
    cores_vibrantes <- c(
      "Alimentação"      = "#264653", # Azul Petróleo
      "Moradia"           = "#2a9d8f", # Esmeralda
      "Transporte"        = "#e9c46a", # Âmbar
      "Saúde"             = "#f4a261", # Laranja Suave
      "Educação"          = "#e76f51", # Terracota
      "Não Gasto/Outros"  = "#a8a8a8"  # Cinza Neutro
    )
    
    ggplot(df_p, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=0, fill=Categoria)) +
      geom_rect(color = "white", size = 0.5) + 
      coord_polar(theta="y") + 
      scale_fill_manual(values = cores_vibrantes) +
      geom_text(aes(x=2.5, y=labelPosition, label=scales::percent(PercReal, accuracy = 1)), 
                size=4.5, fontface="bold", color="white") + # Texto branco para contraste
      theme_void(base_size = 15) + 
      theme(legend.position = "bottom", 
            plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10))) +
      labs(title = paste("\nComposição:", escolha), fill = "") +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  })
  
  # --- 2.3. Tabela Resumo ---
  output$tabelaResumo <- renderTable({
    res <- dados_processados()
    res[,2:7] <- lapply(res[,2:7], function(x) {
      format(x, big.mark = ".", decimal.mark = ",", nsmall = 2, scientific = FALSE)
    })
    colnames(res) <- c("Faixa de Renda", "Alimentação", "Moradia", "Transporte", "Saúde", "Educação", "Renda Média")
    res
  }, striped = TRUE, hover = TRUE, align = 'r')
}

shinyApp(ui = ui, server = server)