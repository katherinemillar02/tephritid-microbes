library(shiny)
library(ggplot2)
library(cowplot)
library(RColorBrewer)

# Microbiota database
microbiota_db <- list(
  "Ceratitis capitata" = list(
    
    
    "Metagenomics" = list(
      "Mass-reared" = list(
        refs = list(
          "Behar et al. 2005" = c("Klebsiella" = 40, "Enterobacter" = 30, "Pantoea" = 20, "Other" = 10),
          "Ben Ami et al. 2010" = c("Klebsiella" = 35, "Enterobacter" = 35, "Pantoea" = 20, "Other" = 10)
        ),
      
        facts = c(
          "Metagenomics: captures full genome of all microbes",
          "Shows rare taxa not detected by 16S"
        ),
        image = "app-medfly-metagenomics.jpg"
      ),
      "Wild" = list(
        refs = list(
          "Behar et al. 2005" = c("Klebsiella" = 45, "Enterobacter" = 25, "Pantoea" = 20, "Other" = 10)
        ),
        facts = c("Wild flies may have more diverse microbiota"),
        image = "app-medfly-metagenomics-wild.jpg"
      ),
      "Lab" = list(
        refs = list(
          "Mason et al. 2025" = c("Klebsiella" = 30, "Enterobacter" = 40, "Pantoea" = 20, "Other" = 10),
          "Aoki et al. 2025" = c("Klebsiella" = 30, "Enterobacter" = 40, "Pantoea" = 20, "Other" = 10)
        ),
        facts = c("Lab flies may have simplified microbiota due to controlled diet"),
        image = "app-medfly-metagenomics-lab.jpg"
      )
    ),
    
    
    
    "Long-read 16S" = list(
      "Mass-reared" = list(
        refs = list(
          "Mason et al. 2025" = c("Klebsiella" = 35, "Enterobacter" = 35, "Pantoea" = 20, "Other" = 10)
        ),
        facts = c("Long-read 16S: high resolution of bacterial species"),
        image = "app-medfly-long16S.jpg"
      ),
      "Wild" = list(
        refs = list(
          "Aoki et al. 2025" = c("Klebsiella" = 35, "Enterobacter" = 35, "Pantoea" = 20, "Other" = 10)
        ),
        facts = c("Wild flies: higher bacterial diversity"),
        image = "app-medfly-long16S-wild.jpg"
      ),
      "Lab" = list(
        refs = list(
          "Ravigne et al. 2022" = c("Citrobacter" = 15, "Enterobacter" = 70, "Klebsiella" = 10, "Providencia" = 5),
          "Mason et al. 2025" = c("Enterococcus" = 50, "Enterobacter" = 15, "Pantoea" = 30, "Other" = 5),
          "Aoki et al. 2025" = c("Enterococcus" = 50, "Klebsiella" = 5, "Enterobacter" = 15, "Serratia" = 10, "Providencia" = 20)
        ),
        facts = c("Lab flies: simplified microbiota"),
        image = "app-medfly-long16S-lab.jpg"
      )
    ),
    
    
    
    "Short-read 16S" = list(
      "Mass-reared" = list(
        refs = list(
          "Mason et al. 2025" = c("Klebsiella" = 35, "Enterobacter" = 35, "Pantoea" = 20, "Other" = 10)
        ),
        facts = c("Short-read 16S: high resolution of bacterial species"),
        image = "app-medfly-short16S.jpg"
      ),
      "Wild" = list(
        refs = list(
          "Darrington et al. 2022" = c("Klebsiella" = 60, "Enterococcus" = 10, "Pantoea" = 30), 
          "Arias et al. 2022 - V4, 515F-806R" = c("Acinetobacter" = 40, "Lactobacillus" = 10, "Clostridiales unclassified" = 30, "Burkholderiaceae unclassified" = 30), 
          "Jose et al. 2023 - V4, 515F-806R" = c("Klebsiella" = 85, "Commensalibacter" = 10)
        ),
        facts = c("Short-read 16S: high resolution of bacterial species"),
        image = "app-medfly-short16S.jpg"
      ),
      "Lab" = list(
        refs = list(
          "Darrington et al. 2022" = c("Klebsiella" = 80, "Enterococcus" = 20)
        ),
        facts = c("Short-read 16S: high resolution of bacterial species"),
        image = "app-medfly-short16S-lab.jpg"
      )
    )
  ),
  
  
  
  
  
  "Anastrepha ludens" = list(
    dominant = c("Pseudomonas" = 35, "Enterobacter" = 25, "Providencia" = 20, "Other" = 20),
    facts = c(
      "Dominant gut bacteria: Pseudomonas, Enterobacter, Providencia",
      "Microbiota affects larval development and mating success"
    ),
    refs = c("Kuzina et al. 2001", "Lauzon 2003"),
    image = "anastrepha_ludens.jpg"
  ),
  "Bactrocera dorsalis" = list(
    dominant = c("Enterobacter" = 45, "Serratia" = 25, "Lactobacillus" = 20, "Other" = 10),
    facts = c(
      "Dominant gut bacteria: Enterobacter, Serratia, Lactobacillus",
      "Important in host plant utilization and insecticide resistance",
      "Candidate for probiotic supplementation in SIT programs"
    ),
    refs = c("Wang et al. 2011", "Noman et al. 2018"),
    image = "bactrocera_dorsalis.jpg"
  )
)

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
      df <- do.call(rbind, lapply(names(ref_list), function(ref) {
        data.frame(Bacteria = names(ref_list[[ref]]),
                   Abundance = ref_list[[ref]],
                   Reference = ref)
      }))
      ggplot(df, aes(x = Reference, y = Abundance, fill = Bacteria)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        ylab("Relative Abundance (%)") +
        xlab("Study Reference") +
        ggtitle(paste("Microbiota Composition of", input$species)) +
        scale_fill_brewer(palette = "Set3")
    } else {
      dat <- data$dominant
      df <- data.frame(Bacteria = names(dat), Abundance = dat)
      ggplot(df, aes(x = "", y = Abundance, fill = Bacteria)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        ylab("Approximate Relative Abundance (%)") +
        xlab("") +
        ggtitle(paste("Microbiota Composition of", input$species)) +
        scale_fill_brewer(palette = "Set3")
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
