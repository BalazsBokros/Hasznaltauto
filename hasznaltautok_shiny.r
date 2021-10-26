library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(ggcorrplot)

#save.image(file = "cars_data.RData")
#write_rds(cars_data,"hasznaltautok1025.rds")

load("cars_data.RData")

ui <- dashboardPage(
  dashboardHeader(title = "Hasznaltauto.hu elemzés"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Piac elemzés", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Autó választás", tabName = "widgets", icon = icon("th")),
      menuItem("Szűrt adatok", tabName = "adatok", icon = icon("th"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h2("Magyar használtautó piac elemzése"),
              h2(),
              fluidRow(
              tabBox(width = 10,
                     tabPanel(
                       "Márka szerinti elemzés",
                       fluidRow(
                       box(width = 8, plotOutput("brand"),plotOutput("brand2")),
                       box(width = 4, title = "Beállítások", sliderInput("slider", "TOP Márkák száma", 1, 50, 15,1)))
                     ),
                     tabPanel(
                       "Autók kora szerinti elemzés",
                       fluidRow(
                       box(width = 8,
                       plotOutput("age"),
                       plotOutput("age2"),
                       plotOutput("age3")),
                       box(width = 4, title = "Beállítások", 
                           sliderInput("slider2",
                                       "Évek",
                                       min = 1980,
                                       max = 2020,
                                       value = c(1980, 2020),
                                       step = 1,
                                       sep = "")))
                      ),
                     tabPanel(
                       "Korrelomátrix",
                       plotOutput("corr")
                     ))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Autó vásárlás elemzés"),
              h2(),
              fluidRow(
              box(width = 8,
                  title = "Elemzés",
                  verbatimTextOutput ("stats"),
                  plotOutput("boxp1"),
                  plotOutput("boxp2"),
                  plotOutput("boxp3"),
                  plotOutput("boxp4")
                  ),
              box(width = 4,
                  title = "Szűrések",
                  shiny::selectInput(inputId = "brand", "Márka:",
                                     choices =  c(levels(cars_data$brand))),
                  shiny::selectInput(inputId = "model", "Modell:",
                                     choices =  NULL),
                  # shiny::selectInput(inputId = "fuel", "Üzemanyag:",
                  #                    choices = NULL),
                  shiny::sliderInput("year_filter", "Év:", min = 1980, max = 2020, value = c(1980, 2020), step = 1, sep = ""),
                  shiny::sliderInput("price_filter", "Ár:", min = 0, max = 50e6, value = c(0, 50e6), step = 1e5, sep = " ")
              ))
      ),
      
      # Third tab content
      
      tabItem(tabName = "adatok",
              h2("Összes autó"),
              h2(),
              fluidRow(
                box(width = 12,
                  dataTableOutput("adatok")
              )))
    )
  )
)



#Szűrés lista kreálása

filter_list <- cars_data[,c("brand","modell","uzemanyag","vetelar","evjarat")]

#Brand frequency table kreálása (1. rész, 1.ábrához)

df_brands <- as.data.frame(table(cars_data$brand))
df_brands <- df_brands %>% 
  dplyr::rename (brands = Var1) %>% 
  arrange(desc(Freq))

#Brand átlagár table kreálása (1. rész, 2.ábrához)

df_brands2 <- cars_data %>% 
  dplyr::group_by(brand) %>% 
  dplyr::summarise(vetelar_atlag= mean(vetelar)) %>% 
  dplyr::arrange(desc(vetelar_atlag))

df_brands2$vetelar_atlag_mf <- df_brands2$vetelar_atlag / 1000000

#Évjárat átlagár table kreálása (1. rész, 4.ábrához)

df_age2 <- cars_data %>% 
  group_by(evjarat) 

df_age2$vetelar_mf <- df_age2$vetelar / 1000000

##Korrelomatrix

df_corr <- cars_data %>% 
  dplyr::rename (évjárat = evjarat,
          teljesítmény = teljesitmeny,
          hengerűrtartalom = hengerurtartalom,
          tömeg = teljes_tomeg,
          kilóméteróra_állása = kilometerora_allasa,
          vételár = vetelar)
df_corr$állapot <- as.numeric(as.vector(plyr::revalue(df_corr$allapot,
                                           c("Hiányos" = 1, "Sérülésmentes"= 2, 
                                             "Megkímélt" = 3,"Normál" = 4, 
                                             "Újszerű"= 5, "Kitűnő" = 6))))
df_corr <- df_corr[ , c('évjárat', 'kilóméteróra_állása', 'hengerűrtartalom',
                  'teljesítmény', 'vételár', 'tömeg', 
                  'állapot')]
df_corr <- na.omit (df_corr)
cor_mat <- cor(df_corr)
cor <- ggcorrplot:: ggcorrplot (cor_mat, 
                                lab = T,
                                type = "lower",
                                hc.order = T) 
corrplot <- cor + theme_classic(base_size = 16) +
  theme ( axis.title.x = element_blank(),axis.title.y = element_blank(), 
          axis.text.x = element_text(angle=45,vjust = 0.5, hjust=0.5
                                     ))

server <- function(session,input, output) {
  
  
  subdata <- reactive({
    cars_data %>% 
      filter(
        evjarat >= input$year_filter[1] &
          evjarat <= input$year_filter[2] &
          vetelar >= input$price_filter[1] &
          vetelar <= input$price_filter[2] &
          # uzemanyag == input$fuel &
          brand == input$brand &
          modell == input$model
      )
    
  })
  
  #1. Rész 1. Ábra
  
  output$brand <- renderPlot({
    ggplot (head(df_brands,input$slider), aes(x = reorder(brands, -Freq), y = Freq)) +
      geom_col(aes( fill = brands), show.legend = F ) +
      theme_classic(base_size = 16, base_line_size = 0.4, base_rect_size = 0.7) +
      theme (axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=1)) +
      labs(x= "Autómárkák",
           y= "Hirdetések száma",
           title =paste("Top",input$slider,"márka hirdetések számai alapján"))
  })
  
  #1. Rész 2. Ábra
  
  output$brand2 <- renderPlot({
    ggplot(head(df_brands2,input$slider) , aes(reorder(brand, -vetelar_atlag_mf),vetelar_atlag_mf , fill=brand))+
      geom_col(show.legend = F)+
      labs(x="Autómárkák",
           y="Átlagos vételár (mFt)",
           title=paste("Top",input$slider,"márka átlagos vételárak alapján")) +
      theme_classic(base_size = 16,base_line_size = 0.4, base_rect_size = 0.7)+
      theme(axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=1)) +
      geom_hline(yintercept = mean(cars_data$vetelar/1000000))
  })
  
  
  #1. Rész 3. Ábra
  
  output$age <- renderPlot({
    ggplot(cars_data %>% filter(evjarat >= input$slider2[1]) %>% filter(evjarat <= input$slider2[2]))+
      geom_histogram(aes(evjarat),
                     fill = "red",
                     binwidth = 1)+
      labs(
        title = "Az autók gyártási évének eloszlása",
        x = "Gyártási év",
        y = "Darab",
      )+
      theme_classic(base_size = 16) +
      scale_x_continuous("evjarat")
  })
  
  #1. Rész 4. Ábra
  
  output$age2 <- renderPlot({
    ggplot(cars_data %>% filter(evjarat >= input$slider2[1]) %>% filter(evjarat <= input$slider2[2]) %>% group_by(evjarat) , aes(x=reorder(evjarat,evjarat),
                     y=vetelar/1000000, fill = evjarat)) +
      geom_boxplot(outlier.shape = NA)+
      geom_hline(aes(yintercept=mean(cars_data$vetelar)/1000000))+
      #stat_summary(fun= mean,color="red")
      labs(x="Évjárat",
           y="Átlagos vételár (mFt)",
           title="Az autók átlagárai évjárat szerint")+
      
      theme_classic(base_size = 16, base_line_size = 0.4, base_rect_size = 0.7)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_y_continuous(limits = c(0, 20))
  })
  
  #1. Rész 5. Ábra
  
  output$age3 <- renderPlot({
    ggplot (cars_data %>% filter(evjarat >= input$slider2[1]) %>% filter(evjarat <= input$slider2[2]), aes (x = evjarat, 
                     y = reorder (allapot , evjarat),
                     fill = allapot)) +
      geom_violin () +
      #stat_summary(fun = mean , color="blue") +
      theme_classic(base_size = 16, base_line_size = 0.4, base_rect_size = 0.7) +
      labs(x= "Évjárat",
           y= "Állapot",
           title ="Az autók állapota évjárat szerint",
           fill = "Állapot") 
    
  })
  
  #1. Rész Korrelogram
  
  output$corr <- renderPlot({
    corrplot
  })
  
  #2. Rész - Leíró statisztika
  
  output$stats <- renderPrint ({
    psych::describe(subdata())[c(3,6,8,13,21,23,24,26),c(2:5,8:9,11:13)]
  })
  
  #2. Rész - Elemek kilistázása
  
  output$adatok <- renderDataTable ({
    subdata()[,c(1:6,11:13,21,66)]
  })
  
  #2. Rész 1. Boxplot (evjarat)
  
  output$boxp1 <- renderPlot({
    ggplot(subdata() %>% group_by(evjarat), aes(reorder(evjarat,evjarat),vetelar/1000, fill = evjarat)) +
      geom_boxplot(outlier.shape = NA)+
      labs(x="Évjárat",
           y="Átlagos vételár (eFt)",
           title="Autók átlagárai évjárat szerint")+
      theme_classic(base_size = 16, base_line_size = 0.4, base_rect_size = 0.7)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  
  #2. Rész 2. Boxplot (Váltó)
  
  output$boxp2 <- renderPlot({
    ggplot(subdata(), aes(sebessegvalto_fajtaja,vetelar/1000, fill = sebessegvalto_fajtaja)) +
      geom_boxplot(outlier.shape = NA)+
      labs(x="Sebességváltó fajtája",
           y="Átlagos vételár (eFt)",
           title="Autók átlagárai sebességváltó fajtája szerint")+
      theme_classic(base_size = 16, base_line_size = 0.4, base_rect_size = 0.7)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      stat_summary(fun = mean,
                   color = "blue")
  })
  
  #2. Rész 3. Boxplot (Üzemanyag)
  
  output$boxp3 <- renderPlot({
    ggplot(subdata(), aes(uzemanyag,vetelar/1000, fill = uzemanyag)) +
      geom_boxplot(outlier.shape = NA)+
      labs(x="Üzemanyag",
           y="Átlagos vételár (eFt)",
           title="Autók átlagárai üzemanyag fajtája szerint")+
      theme_classic(base_size = 16, base_line_size = 0.4, base_rect_size = 0.7)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      stat_summary(fun = mean,
                   color = "blue")
  })
  
  
  #2. Rész 4. Boxplot (Állapot)
  
  output$boxp4 <- renderPlot({
    ggplot(subdata(), aes(allapot,vetelar/1000, fill = allapot)) +
      geom_boxplot(outlier.shape = NA)+
      labs(x="Állapot",
           y="Átlagos vételár (eFt)",
           title="Autók átlagárai állapot szerint")+
      theme_classic(base_size = 16, base_line_size = 0.4, base_rect_size = 0.7)+
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      stat_summary(fun = mean,
                   color = "blue")
  })
  
  ### Szűréseknél, hogy ne minden opciót mutasson a legördülő menü:
  
  observe({
    print(input$brand)
    x <- filter_list %>% filter(brand == input$brand) %>% select(modell) 
    updateSelectInput(session,"model","Modell:",choices = unique(x))
  })
  
  observe({
    y <- filter_list %>% filter(modell == input$model) %>% select(uzemanyag)
    updateSelectInput(session,"fuel","Üzemanyag:",choices = unique(y), selected = "Benzin")
  })
}

shinyApp(ui, server)