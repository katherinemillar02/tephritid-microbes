library(shiny)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

# Microbiota database
microbiota_db <- list(
  "Ceratitis capitata" = list(
    
    "Metagenomics" = list(
      "Mass-reared" = list(
        refs = list(),   # nothing here yet
        facts = c("No metagenomic studies available yet for mass-reared flies"),
        image = "no_data_available.jpg"
      ),
      
      "Wild" = list(
        refs = list(),
        facts = c("No metagenomic studies available yet for wild flies"),
        image = "no_data_available.jpg"
      ),
      
      "Lab" = list(
        refs = list(),
        facts = c("No metagenomic studies available yet for lab flies"),
        image = "no_data_available.jpg"
      )
    ),
    
    
    
    "Long-read 16S" = list(
      
      
      "Mass-reared" = list(
        refs = list(
          "Mason et al. 2025" = c("Enterobacter" = 95, "Other" = 5)
        ),
        facts = c("Long-read 16S: high resolution of bacterial species"),
        image = "app-medfly-long16S.jpg"
      ),
      
      
      "Wild" = list(
        refs = list(),
        facts = c("No long-read studies available yet for wild flies"),
        image = "no_data_available.jpg"
      ),
      "Lab" = list(
        refs = list(
          "Ravigne et al. 2022" = c("Citrobacter" = 15, "Enterobacter" = 70, "Klebsiella" = 10, "Providencia" = 5),
          "Mason et al. 2025" = c("Enterococcus" = 50, "Enterobacter" = 15, "Pantoea" = 35, "Other" = 5),
          "Aoki et al. 2025" = c("Enterococcus" = 50, "Klebsiella" = 5, "Enterobacter" = 15, "Serratia" = 10, "Providencia" = 20)
        ),
        facts = c("Lab flies: simplified microbiota"),
        image = "app-medfly-long16S-lab.jpg"
      )
    ),
    
    
    
    "Short-read 16S" = list(
      "Mass-reared" = list(
        refs = list(
          "Bel Mokhtar et al. 2022; V4" = c("Klebsiella" = 85, "Salmonella" = 10, "Pantoea" = 3, "Other" = 2), 
          "Mason et al. 2024; V4" = c("Klebsiella" = 85, "Salmonella" = 10, "Pantoea" = 3, "Other" = 2), 
          "Haytham et al. 2024; V4, 515F-805R" = c("Klebsiella" = 50, "Enterobacter" = 20, "Rauoltella" = 8, "Kluyvera" = 8, "Providencia" = 7, "Serratia" = 7)
        ),
        facts = c("Short-read 16S: high resolution of bacterial species"),
        image = "app-medfly-short16S.jpg"
        
        
        
      ),
      "Wild" = list(
        refs = list(
          "De Cock et al. 2020; V3-V4, 341F-805R" = c("Pluribacter" = 6, "Klebsiella" = 45, "Providencia" = 25, "Citrobacter" = 10, "Pantonea" = 4, "Enterococcus" = 6, "Psuedomonas" = 4),
          "Nikoluli et al. 2020" = c("Tatumella" = 10, "Klebsiella" = 80, "Providencia" = 10),
          "Cappelli et al. 2022" = c("Providencia" = 10, "Klebsiella" = 70, "Propionibacterium" = 10, "Asaia" = 5, "Chroococcidiopsis" = 5),
          "Darrington et al. 2022; 28F-806R" = c("Klebsiella" = 60, "Enterococcus" = 10, "Pantoea" = 30), 
          "Arias et al. 2022; V4, 515F-806R" = c("Acinetobacter" = 40, "Lactobacillus" = 10, "Other" = 50), 
          "Jose et al. 2023; V4, 515F-806R" = c("Klebsiella" = 85, "Commensalibacter" = 10), 
          "Tanfouri et al. 2025; V4-V5, 564F-908R" = c("Klebsiella" = 80, "Serratia" = 10, "Acetobacter" = 2, "Morganella" = 2, "Gluconobacter" = 1)
          
        ),
        facts = c("Short-read 16S: high resolution of bacterial species"),
        image = "app-medfly-short16S.jpg"
      ),
      
      
      "Lab" = list(
        refs = list(
          "Darrington et al. 2022; 28F-806R" = c("Klebsiella" = 80, "Enterococcus" = 20),
          "Mason et al. 2023; V4, 515F-806R" = c("Klebsiella" = 60, "Serratia" = 20, "Providencia" = 20)
            ),
        facts = c("Short-read 16S: high resolution of bacterial species"),
        image = "app-medfly-short16S-lab.jpg"
      )
    )
  ),
  
  

  "Anastrepha ludens" = list(
    facts = "data coming soon",
    refs = "data coming soon",
    image = "data_coming_soon.jpg"
  ),
  
  "Bactrocera dorsalis " = list(
    facts = "data coming soon",
    refs = "data coming soon",
    image = "data_coming_soon.jpg"
  ),
  
  
  "Bactrocera tryoni" = list(
    facts = "data coming soon",
    refs = "data coming soon",
    image = "data_coming_soon.jpg"
  ),

  "Bactrocera cucurbitae" = list(
    facts = "data coming soon",
    refs = "data coming soon",
    image = "data_coming_soon.jpg"
  ), 
  
  "Bactrocera oleae" = list(
    facts = "data coming soon",
    refs = "data coming soon",
    image = "data_coming_soon.jpg"
  ))

  
  
# UI
ui <- fluidPage(
  titlePanel("Tephritid Microbiota Atlas"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select a Tephritid Species:", choices = names(microbiota_db)),
      uiOutput("methodUI"),
      uiOutput("sampleUI")
    ),
    mainPanel(
      h3(textOutput("speciesName")),
      uiOutput("speciesImage"),
      plotOutput("compositionPlot", height = "400px"),
      h4("Key Facts"),
      uiOutput("factList"),
      h4("References"),
      uiOutput("refList")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$methodUI <- renderUI({
    req(input$species)
    if (input$species == "Ceratitis capitata") {
      selectInput("method", "Select Method:", choices = names(microbiota_db[["Ceratitis capitata"]]))
    }
  })
  
  output$sampleUI <- renderUI({
    req(input$species)
    if (input$species == "Ceratitis capitata" && !is.null(input$method)) {
      selectInput("sampleType", "Select Sample Type:",
                  choices = names(microbiota_db[["Ceratitis capitata"]][[input$method]]))
    }
  })
  
  output$speciesName <- renderText({
    paste("Microbiota of", input$species)
  })
  
  get_data <- reactive({
    if (input$species == "Ceratitis capitata") {
      req(input$method, input$sampleType)
      microbiota_db[[input$species]][[input$method]][[input$sampleType]]
    } else {
      microbiota_db[[input$species]]
    }
  })
  
  output$speciesImage <- renderUI({
    req(get_data())
    tags$img(src = get_data()$image, height = "250px")
  })
  output$compositionPlot <- renderPlot({
    data <- get_data()
    req(data)
    
    if (input$species == "Ceratitis capitata") {
      ref_list <- data$refs
      
      # ⛔ If no refs, show message instead of trying to plot
      if (length(ref_list) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for this category", cex = 1.5)
        return()
      }
      
      # Otherwise build dataframe and plot
      df <- do.call(rbind, lapply(names(ref_list), function(ref) {
        data.frame(
          Bacteria = names(ref_list[[ref]]),
          Abundance = ref_list[[ref]],
          Reference = ref
        )
      }))
      
      # Wrap long labels
      library(stringr)
      df$Reference <- str_wrap(df$Reference, width = 25)
      
      ggplot(df, aes(x = Reference, y = Abundance, fill = Bacteria)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        ylab("Relative Abundance (%)") +
        xlab("Study Reference") +
        ggtitle(paste("Microbiota Composition of", input$species)) +
        scale_fill_manual(
          values = colorRampPalette(brewer.pal(12, "Set3"))(length(unique(df$Bacteria)))
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      dat <- data$dominant
      
      if (length(dat) == 0) {
        plot.new()
        text(0.5, 0.5, "No data available for this category", cex = 1.5)
        return()
      }
      
      df <- data.frame(Bacteria = names(dat), Abundance = dat)
      
      ggplot(df, aes(x = "", y = Abundance, fill = Bacteria)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        ylab("Approximate Relative Abundance (%)") +
        xlab("") +
        ggtitle(paste("Microbiota Composition of", input$species)) +
        scale_fill_manual(
          values = colorRampPalette(brewer.pal(12, "Set3"))(length(unique(df$Bacteria)))
        )
    }
  })
  
  output$factList <- renderUI({
    req(get_data())
    tags$ul(lapply(get_data()$facts, tags$li))
  })
  
  output$refList <- renderUI({
    req(get_data())
    refs <- if (input$species == "Ceratitis capitata") {
      names(get_data()$refs)
    } else {
      get_data()$refs
    }
    tags$ul(lapply(refs, tags$li))
  })
}

# Run app
shinyApp(ui, server)
