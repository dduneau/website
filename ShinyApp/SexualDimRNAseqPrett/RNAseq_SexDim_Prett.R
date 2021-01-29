#
#Shiny app developped by Becca Belmonte, PhD student supervised by Jennifer Regan (Edinburgh University).
#Data from Duneau et al. BMC Biology 2017

library(shiny)
library(DT)
library(tidyverse)
library(viridis)
library(plotly)
library(heatmaply)
library(VennDiagram)

load("RNA_seq.RData")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   title = "Differential Expression Analysis",
  
fluidRow(  
  column(width = 1,
    
   # Comparisons 
   selectizeInput("Sex_1",
                      label = h3("Sex"),
                      choices = list("Male" = "M",
                                     "Female" = "F"),
                      selected = "F")),
  column(width = 1,
         
         # Comparisons 
         selectizeInput("Dissect_1",
                        label = h3("Gonads"),
                        choices = list("No Gonads" = "Y",
                                       "Gonads" = "N"),
                        selected = "N")),
  column(width = 1,
         
         # Comparisons 
         selectizeInput("Time_1",
                        label = h3("Time"),
                        choices = list("8 hours" = "8"#,
                                       #"24 hours" = "24",
                                       #"72 hours" = "72"
                                       ),
                        selected = "8")),
  column(width = 1,
         selectizeInput("Challenge_1",
                        label = h3("Challenge"),
                        choices = list("Unchallenged" = "UC",
                                       "P. rettgeri" = "Pr",
                                       "Wounded (8 hours only)" = "WD"),
                        selected = "UC")),

  column(width = 1, offset = 2,
         
         # Comparisons 
         selectizeInput("Sex_2",
                        label = h3("Sex"),
                        choices = list("Male" = "M",
                                       "Female" = "F"),
                        selected = "M")),
  column(width = 1,
         
         # Comparisons 
         selectizeInput("Dissect_2",
                        label = h3("Gonads"),
                        choices = list("No Gonads" = "Y",
                                       "Gonads" = "N"),
                        selected = "N")),
  column(width = 1,
         
         # Comparisons 
         selectizeInput("Time_2",
                        label = h3("Time"),
                        choices = list("8 hours" = "8"#,
                                       #"24 hours" = "24",
                                       #"72 hours" = "72"
                                       ),
                        selected = "8")),
  column(width = 1,
         selectizeInput("Challenge_2",
                        label = h3("Challenge"),
                        choices = list("Unchallenged" = "UC",
                                       "P. rettgeri" = "Pr",
                                       "Wounded (8 hours only)" = "WD"),
                        selected = "UC"))
  ),
fluidRow(
   column(width = 6,    
       DTOutput('table'),
       plotlyOutput('volcano'),
       plotlyOutput('heatmap')),
       
   column(width = 6,
       DTOutput('table2'),
       plotlyOutput('volcano2'),
       plotlyOutput('heatmap2'))

  ),
fluidRow(
  column(12,
         plotOutput('venn'))
),
fluidRow(
  column(4,
         textOutput('onetext')),
  column(4,
         textOutput('commontext')),
  column(4,
         textOutput('twotext'))
),
fluidRow(
  column(4,
         DTOutput('comp1')),
  column(4,
         DTOutput('common')),
  column(4,
         DTOutput('comp2'))
),
fluidRow(
  column(width = 2,
         selectizeInput("Gene",
                        label = h3("Enter one gene name"),
                        choices = genes,
                        selected = "128up")),
  column(width = 10,
         plotOutput('rawpoints')))
)


# Define server logic required to create table
server <- function(input, output) {
  output$table <- renderDT({
  if(input$Sex_1 == "F" & input$Challenge_1 == "UC"){top <- Tab_top_F_UC_8}
  if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top <- Tab_top_Y_F_Pr_8}
  if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top <- Tab_top_N_F_Pr_8}
  if(input$Sex_1 == "M" & input$Challenge_1 == "UC"){top <- Tab_top_M_UC_8}
  if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top <- Tab_top_Y_M_Pr_8}
  if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top <- Tab_top_N_M_Pr_8}
  if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top <- Tab_top_Y_F_WD_8}
  if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top <- Tab_top_N_F_WD_8} 
  if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top <- Tab_top_Y_M_WD_8}
  if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top <- Tab_top_N_M_WD_8}
  if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top <- Tab_top_Y_F_Pr_24}
  if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top <- Tab_top_N_F_Pr_24}
  if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top <- Tab_top_Y_M_Pr_24}
  if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top <- Tab_top_N_M_Pr_24}
  if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top <- Tab_top_Y_F_Pr_72}
  if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top <- Tab_top_N_F_Pr_72}
  if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top <- Tab_top_Y_M_Pr_72}
  if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top <- Tab_top_N_M_Pr_72}
  
    top <- top[,1:7]
    top[,1:6] <- round(top[,1:6], digits = 3)
 datatable(top)  }
   )
   output$volcano <- renderPlotly({
     if(input$Sex_1 == "F" & input$Challenge_1 == "UC"){volcano <- vol_F_UC_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){volcano <- vol_Y_F_Pr_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){volcano <- vol_N_F_Pr_8}
     if(input$Sex_1 == "M" & input$Challenge_1 == "UC"){volcano <- vol_M_UC_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){volcano <- vol_Y_M_Pr_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){volcano <- vol_N_M_Pr_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){volcano <- vol_Y_F_WD_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){volcano <- vol_N_F_WD_8} 
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){volcano <- vol_Y_M_WD_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){volcano <- vol_N_M_WD_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){volcano <- vol_Y_F_Pr_24}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){volcano <- vol_N_F_Pr_24}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){volcano <- vol_Y_M_Pr_24}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){volcano <- vol_N_M_Pr_24}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){volcano <- vol_Y_F_Pr_72}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){volcano <- vol_N_F_Pr_72}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){volcano <- vol_Y_M_Pr_72}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){volcano <- vol_N_M_Pr_72}
     
    volcano                             
    })
   
   output$heatmap <- renderPlotly({
     if(input$Sex_1 == "F" & input$Challenge_1 == "UC"){heatmap <- heat_F_UC_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){heatmap <- heat_Y_F_Pr_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){heatmap <- heat_N_F_Pr_8}
     if(input$Sex_1 == "M" & input$Challenge_1 == "UC"){heatmap <- heat_M_UC_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){heatmap <- heat_Y_M_Pr_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){heatmap <- heat_N_M_Pr_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){heatmap <- heat_Y_F_WD_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){heatmap <- heat_N_F_WD_8} 
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){heatmap <- heat_Y_M_WD_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){heatmap <- heat_N_M_WD_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){heatmap <- heat_Y_F_Pr_24}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){heatmap <- heat_N_F_Pr_24}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){heatmap <- heat_Y_M_Pr_24}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){heatmap <- heat_N_M_Pr_24}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){heatmap <- heat_Y_F_Pr_72}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){heatmap <- heat_N_F_Pr_72}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){heatmap <- heat_Y_M_Pr_72}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){heatmap <- heat_N_M_Pr_72}
     
     heatmap
     })
   output$table2 <- renderDT({
     if(input$Sex_2 == "F" & input$Challenge_2 == "UC"){top <- Tab_top_F_UC_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top <- Tab_top_Y_F_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top <- Tab_top_N_F_Pr_8}
     if(input$Sex_2 == "M" & input$Challenge_2 == "UC"){top <- Tab_top_M_UC_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top <- Tab_top_Y_M_Pr_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top <- Tab_top_N_M_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top <- Tab_top_Y_F_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top <- Tab_top_N_F_WD_8} 
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top <- Tab_top_Y_M_WD_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top <- Tab_top_N_M_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top <- Tab_top_Y_F_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top <- Tab_top_N_F_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top <- Tab_top_Y_M_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top <- Tab_top_N_M_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top <- Tab_top_Y_F_Pr_72}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top <- Tab_top_N_F_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top <- Tab_top_Y_M_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top <- Tab_top_N_M_Pr_72}
     
     top <- top[,1:7]
     top[,1:6] <- round(top[,1:6], digits = 3)
     datatable(top)    }
   )
   output$volcano2 <- renderPlotly({
     if(input$Sex_2 == "F" & input$Challenge_2 == "UC"){volcano2 <- vol_F_UC_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){volcano2 <- vol_Y_F_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){volcano2 <- vol_N_F_Pr_8}
     if(input$Sex_2 == "M" & input$Challenge_2 == "UC"){volcano2 <- vol_M_UC_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){volcano2 <- vol_Y_M_Pr_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){volcano2 <- vol_N_M_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){volcano2 <- vol_Y_F_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){volcano2 <- vol_N_F_WD_8} 
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){volcano2 <- vol_Y_M_WD_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){volcano2 <- vol_N_M_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){volcano2 <- vol_Y_F_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){volcano2 <- vol_N_F_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){volcano2 <- vol_Y_M_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){volcano2 <- vol_N_M_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){volcano2 <- vol_Y_F_Pr_72}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){volcano2 <- vol_N_F_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){volcano2 <- vol_Y_M_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){volcano2 <- vol_N_M_Pr_72}
     
     volcano2
     
   })
   output$heatmap2 <- renderPlotly({
     if(input$Sex_2 == "F" & input$Challenge_2 == "UC"){heatmap2 <- heat_F_UC_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){heatmap2 <- heat_Y_F_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){heatmap2 <- heat_N_F_Pr_8}
     if(input$Sex_2 == "M" & input$Challenge_2 == "UC"){heatmap2 <- heat_M_UC_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){heatmap2 <- heat_Y_M_Pr_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){heatmap2 <- heat_N_M_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){heatmap2 <- heat_Y_F_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){heatmap2 <- heat_N_F_WD_8} 
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){heatmap2 <- heat_Y_M_WD_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){heatmap2 <- heat_N_M_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){heatmap2 <- heat_Y_F_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){heatmap2 <- heat_N_F_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){heatmap2 <- heat_Y_M_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){heatmap2 <- heat_N_M_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){heatmap2 <- heat_Y_F_Pr_72}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){heatmap2 <- heat_N_F_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){heatmap2 <- heat_Y_M_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){heatmap2 <- heat_N_M_Pr_72}
     
     heatmap2
   })
   output$venn <- renderPlot({
     if(input$Sex_1 == "F" & input$Challenge_1 == "UC"){top1 <- Tab_top_F_UC_8
                                        outline1 <- "#9F2F7F"
                                        title1 <- "Females Uninfected (8 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_8
     outline1 <- "#000004"
     title1 <- "Females Infected Dissected (8 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_8
     outline1 <- "#000004"
     title1 <- "Females Infected Undissected (8 hours)"}
     if(input$Sex_1 == "M" & input$Challenge_1 == "UC"){top1 <- Tab_top_M_UC_8
     outline1 <- "#9F2F7F"
     title1 <- "Males Uninfected (8 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_8
     outline1 <- "#000004"
     title1 <- "Males Infected Dissected (8 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_8
     outline1 <- "#000004"
     title1 <- "Males Infected Undissected (8 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_Y_F_WD_8
     outline1 <- "#F1605D"
     title1 <- "Females Wounded Dissected (8 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_N_F_WD_8
     outline1 <- "#F1605D"
     title1 <- "Females Wounded Undissected (8 hours)"} 
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_Y_M_WD_8
     outline1 <- "#F1605D"
     title1 <- "Males Wounded Dissected (8 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_N_M_WD_8
     outline1 <- "#F1605D"
     title1 <- "Males Wounded Undissected (8 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_24
     outline1 <- "#000004"
     title1 <- "Infected Females (dissected, 24 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_24
     outline1 <- "#000004"
     title1 <- "Infected Females (undissected, 24 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_24
     outline1 <- "#000004"
     title1 <- "Infected Males (dissected, 24 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_24
     outline1 <- "#000004"
     title1 <- "Infected Males (undissected, 24 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_72
     outline1 <- "#000004"
     title1 <- "Infected Females (dissected, 72 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_72
     outline1 <- "#000004"
     title1 <- "Infected Females (undissected, 72 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_72
     outline1 <- "#000004"
     title1 <- "Infected Males (dissected, 72 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_72
     outline1 <- "#000004"
     title1 <- "Infected Males (undissected, 72 hours)"}
     
     if(input$Sex_2 == "F" & input$Challenge_2 == "UC"){top2 <- Tab_top_F_UC_8
     outline2 <- "#9F2F7F"
     title2 <- "Females Uninfected (8 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_8
     outline2 <- "#000004"
     title2 <- "Females Infected Dissected (8 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_8
     outline2 <- "#000004"
     title2 <- "Females Infected Undissected (8 hours)"}
     if(input$Sex_2 == "M" & input$Challenge_2 == "UC"){top2 <- Tab_top_M_UC_8
     outline2 <- "#9F2F7F"
     title2 <- "Males Uninfected (8 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_8
     outline2 <- "#000004"
     title2 <- "Males Infected Dissected (8 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_8
     outline2 <- "#000004"
     title2 <- "Males Infected Undissected (8 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_Y_F_WD_8
     outline2 <- "#F1605D"
     title2 <- "Females Wounded Dissected (8 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_N_F_WD_8
     outline2 <- "#F1605D"
     title2 <- "Females Wounded Undissected (8 hours)"} 
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_Y_M_WD_8
     outline2 <- "#F1605D"
     title2 <- "Males Wounded Dissected (8 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_N_M_WD_8
     outline2 <- "#F1605D"
     title2 <- "Males Wounded Undissected (8 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_24
     outline2 <- "#000004"
     title2 <- "Infected Females (dissected, 24 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_24
     outline2 <- "#000004"
     title2 <- "Infected Females (undissected, 24 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_24
     outline2 <- "#000004"
     title2 <- "Infected Males (dissected, 24 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_24
     outline2 <- "#000004"
     title2 <- "Infected Males (undissected, 24 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_72
     outline2 <- "#000004"
     title2 <- "Infected Females (dissected, 72 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_72
     outline2 <- "#000004"
     title2 <- "Infected Females (undissected, 72 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_72
     outline2 <- "#000004"
     title2 <- "Infected Males (dissected, 72 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_72
     outline2 <- "#000004"
     title2 <- "Infected Males (undissected, 72 hours)"}
     
     
     top1 <- top1 %>% 
       filter(log2FoldChange > 0)
     best1 <- as.character(top1$gene)
     top2 <- top2 %>% 
       filter(log2FoldChange > 0)
     best2 <- as.character(top2$gene)
     
     nrow1 <- length(best1)
     nrow2 <- length(best2)
     col1 <- venn_colors[nrow1]
     col2 <- venn_colors[nrow2]
     
     outline <- c(outline1, outline2)
     
     if(nrow1 < nrow2){rot = 180
     cats = c(title2, title1)}
     if(nrow1 >= nrow2){rot = 0
     cats = c(title1, title2)}
     
     diagram <- venn.diagram(x = list(best1, best2),
                             category.names = cats,
                             fill = (c(col1, col2)), 
                             col = outline,
                             alpha = 0.5,
                             cat.fontfamily= "sans",
                             margin = 0.125,
                             cex = 3,
                             cat.cex = 1,
                             cat.dist = 0.075, filename = NULL,
                             rotation.degree = rot
     )
     
     
     grid.draw(grobTree(diagram))
     


   })
   output$onetext <- renderText({
     if(input$Sex_1 == "F" & input$Challenge_1 == "UC"){text1 <- "Specific to Female Gonads"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Females (dissected)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Females (undissected)"}
     if(input$Sex_1 == "M" & input$Challenge_1 == "UC"){text1 <- "Specific to Male Gonads"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Males (dissected)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Males (undissected)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){text1 <- "Specific to Wounded Females (dissected)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){text1 <- "Specific to Wounded Females (undissected)"} 
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){text1 <- "Specific to Wounded Males (dissected)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){text1 <- "Specific to Wounded Males (undissected)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Females (dissected, 24 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Females (undissected, 24 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Males (dissected, 24 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Males (undissected, 24 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Females (dissected, 72 hours)"}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Females (undissected, 72 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Males (dissected, 72 hours)"}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){text1 <- "Specific to Infected Males (undissected, 72 hours)"}
   paste(text1)
     })
   output$comp1 <- renderDT({
     if(input$Sex_1 == "F" & input$Challenge_1 == "UC"){top1 <- Tab_top_F_UC_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_8}
     if(input$Sex_1 == "M" & input$Challenge_1 == "UC"){top1 <- Tab_top_M_UC_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_Y_F_WD_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_N_F_WD_8} 
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_Y_M_WD_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_N_M_WD_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_24}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_24}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_24}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_24}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_72}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_72}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_72}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_72}
   
     if(input$Sex_2 == "F" & input$Challenge_2 == "UC"){top2 <- Tab_top_F_UC_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_8}
     if(input$Sex_2 == "M" & input$Challenge_2 == "UC"){top2 <- Tab_top_M_UC_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_Y_F_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_N_F_WD_8} 
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_Y_M_WD_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_N_M_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_72}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_72}
     top1 <- top1 %>% 
       filter(log2FoldChange > 0)
     genes1 <- top1$gene
     top2 <- top2 %>% 
       filter(log2FoldChange > 0)
     genes2 <- top2$gene
     top1 <- top1 %>% 
       filter(!gene %in% genes2)
     onespecific <- top1 %>% 
       dplyr::select(baseMean, log2FoldChange, padj, gene)
     
     onespecific[,1:3] <- round(onespecific[,1:3], digits = 3)
     datatable(onespecific)
   })
   output$commontext <- renderText({
     "Genes in Common"
   })
   output$common <- renderDT({
     if(input$Sex_1 == "F" & input$Challenge_1 == "UC"){top1 <- Tab_top_F_UC_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_8}
     if(input$Sex_1 == "M" & input$Challenge_1 == "UC"){top1 <- Tab_top_M_UC_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_Y_F_WD_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_N_F_WD_8} 
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_Y_M_WD_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_N_M_WD_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_24}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_24}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_24}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_24}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_72}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_72}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_72}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_72}
     
     if(input$Sex_2 == "F" & input$Challenge_2 == "UC"){top2 <- Tab_top_F_UC_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_8}
     if(input$Sex_2 == "M" & input$Challenge_2 == "UC"){top2 <- Tab_top_M_UC_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_Y_F_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_N_F_WD_8} 
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_Y_M_WD_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_N_M_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_72}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_72}
     
     top1 <- top1 %>% 
       filter(log2FoldChange > 0)
     genes1 <- top1$gene
     top2 <- top2 %>% 
       filter(log2FoldChange > 0)
     genes2 <- top2$gene
     common <- top2 %>% 
       filter(gene %in% genes1)
     commongenes <- common %>% 
       dplyr::select(gene)
     datatable(commongenes)
   }, caption = "Upregulated in both selections",
   caption.placement = getOption("xtable.caption.placement", "top"),
   caption.width = getOption("xtable.caption.width", NULL))
   output$twotext <- renderText({
     if(input$Sex_2 == "F" & input$Challenge_2 == "UC"){text2 <- "Specific to Female Gonads"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Females (dissected)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Females (undissected)"}
     if(input$Sex_2 == "M" & input$Challenge_2 == "UC"){text2 <- "Specific to Male Gonads"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Males (dissected)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Males (undissected)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){text2 <- "Specific to Wounded Females (dissected)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){text2 <- "Specific to Wounded Females (undissected)"} 
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){text2 <- "Specific to Wounded Males (dissected)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){text2 <- "Specific to Wounded Males (undissected)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Females (dissected, 24 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Females (undissected, 24 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Males (dissected, 24 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Males (undissected, 24 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Females (dissected, 72 hours)"}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Females (undissected, 72 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Males (dissected, 72 hours)"}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){text2 <- "Specific to Infected Males (undissected, 72 hours)"}
     paste(text2)
   })
   output$comp2 <- renderDT({
     if(input$Sex_1 == "F" & input$Challenge_1 == "UC"){top1 <- Tab_top_F_UC_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_8}
     if(input$Sex_1 == "M" & input$Challenge_1 == "UC"){top1 <- Tab_top_M_UC_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_Y_F_WD_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_N_F_WD_8} 
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_Y_M_WD_8}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "8" & input$Challenge_1 == "WD"){top1 <- Tab_top_N_M_WD_8}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_24}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_24}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_24}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "24" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_24}
     if(input$Sex_1 == "F" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_F_Pr_72}
     if(input$Sex_1 == "F" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_F_Pr_72}
     if(input$Sex_1 == "M" & input$Dissect_1 == "Y" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_Y_M_Pr_72}
     if(input$Sex_1 == "M" & input$Dissect_1 == "N" & input$Time_1 == "72" & input$Challenge_1 == "Pr"){top1 <- Tab_top_N_M_Pr_72}
     
     if(input$Sex_2 == "F" & input$Challenge_2 == "UC"){top2 <- Tab_top_F_UC_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_8}
     if(input$Sex_2 == "M" & input$Challenge_2 == "UC"){top2 <- Tab_top_M_UC_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_Y_F_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_N_F_WD_8} 
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_Y_M_WD_8}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "8" & input$Challenge_2 == "WD"){top2 <- Tab_top_N_M_WD_8}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_24}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "24" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_24}
     if(input$Sex_2 == "F" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_F_Pr_72}
     if(input$Sex_2 == "F" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_F_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "Y" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_Y_M_Pr_72}
     if(input$Sex_2 == "M" & input$Dissect_2 == "N" & input$Time_2 == "72" & input$Challenge_2 == "Pr"){top2 <- Tab_top_N_M_Pr_72}
     top1 <- top1 %>% 
       filter(log2FoldChange > 0)
     genes1 <- top1$gene
     top2 <- top2 %>% 
       filter(log2FoldChange > 0)
     genes2 <- top2$gene
     top2 <- top2 %>% 
       filter(!gene %in% genes1)
     twospecific <- top2 %>% 
       dplyr::select(baseMean, log2FoldChange, padj, gene)
     twospecific[,1:3] <- round(twospecific[,1:3], digits = 3)
     datatable(twospecific)
   }, caption = "Specifically upregulated in second selection",
   caption.placement = getOption("xtable.caption.placement", "top"),
   caption.width = getOption("xtable.caption.width", NULL))
   output$rawpoints <- renderPlot({
     total <- colSums(df_countdata, na.rm = FALSE, dims = 1)
     total <- total/1000000
     Gene <- input$Gene
     df_countdata <- df_countdata %>% 
       mutate(gene = sub("XF:Z:", "", rownames(df_countdata))) %>% 
       filter(gene == Gene)
     rownames(df_countdata) <- df_countdata$gene
     df_countdata <- t(df_countdata) 
     df_countdata <- as.data.frame(df_countdata)
     df_countdata <- as.data.frame(df_countdata[1:84,])
     colnames(df_countdata) <- Gene
     df_countdata <- df_countdata %>% 
       mutate(group = rownames(df_countdata)) %>% 
       cbind(colData) 
     df_countdata[,1] <- as.numeric(as.character(df_countdata[,1]))
     
     time.labs <- c("8 hours", "24 hours", "72 hours")
     names(time.labs) <- (c("8", "24", "72"))
    
     dis.labs <- c("Dissected", "Not Dissected")
     names(dis.labs) <- (c("Y", "N"))
     
     
     df_countdata$Dissected_Y.N <- as.character(df_countdata$Dissected_Y.N)
     df_countdata$Treatment <- relevel(df_countdata$Treatment, "UC")
     ggplot(df_countdata, aes(x = Sex, y = df_countdata[,1]/total)) +
         geom_boxplot(aes(color = Treatment), position = position_dodge(width = 0.8)) +
         geom_point(aes(color = Treatment), size = 3, position = position_dodge(width = 0.8)) +
         scale_color_manual(values = c("#000004", "#FEC98D", "#F1605D")) +
         theme_bw() +
         theme(strip.background = element_rect(
           color = "black", fill = "white", linetype = "solid"),
           strip.switch.pad.grid = unit(0.2, "cm"), strip.placement = "outside",
           strip.text = element_text(size = 20),
           axis.title = element_text(size = 18),
           axis.text = element_text(size = 18),
           legend.text = element_text(size = 18),
           legend.title = element_text(size = 18)
         ) +
       labs(y = "Transcripts Per Million") +
         facet_wrap(Dissected_Y.N~as.factor(time), scales = "free", labeller = labeller(Dissected_Y.N=dis.labs, time=time.labs))
   }, height = 900, width = 1500)
}

# Run the application 
shinyApp(ui = ui, server = server)

