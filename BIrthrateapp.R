library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggthemes)
library(png)

dfgn <- read.csv("AnnualbyProvince.csv")
dfgn2 <- read.csv("AnnualbyProvinceAVG.csv")
df <- read.csv('Economy2.csv')
eddf <- read.csv('Education.csv')
healthData<-read.csv("transform_data.csv")

options(scipen = 200)

ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">China Birthrate Analyzer</a>'), id="nav",
             windowTitle = "China Birthrate Analyzer",
  
# =======================Birthrate=================================  

  tabPanel("Map of China",
           sidebarLayout(
             sidebarPanel(
               h5("The average birthrates from 2001 to 2019 in 31 provinces "),
               hr(),
               dataTableOutput("BirthrateTablegn"),width = 4
             ),
           mainPanel(
             imageOutput("Map")
             
           )
           ) 
  ),
  
  tabPanel("Birthrate",
    sidebarLayout(
      sidebarPanel(
        h4("Please select one province"),
        
        selectInput(inputId = "Provincegn",label = "Province:", choices = dfgn$Province[c(1:31)]),
        hr(),
        
        h4("Please select one year"),
        sliderInput(inputId = "Yeargn",label="Year:", min = 2001,max=2019,value=2019),
        hr(),
        submitButton('Submit'),width = 4
        
    ),
    
    mainPanel(
      tabsetPanel(
        
        
        tabPanel("Birthrate Comparision between Each Provinceand",
                 h3("Province selection does not work in this tab"),
                 plotOutput("plot_Provinces_BRgn")
        ),
        
        tabPanel("Annual changes in birthrates",
                 br(),
                 plotOutput(outputId = "BRgn"),
                 hr()
        )
      )
    )
    )
  ),

# =======================Education================================= 
  tabPanel("Education",
           
        sidebarLayout(
           sidebarPanel( 
             
             h4("Please select one province"),
             h5("(EFFECTIVE TO THE FIRST TWO FUNCTIONS)"),
             hr(),
             selectInput(inputId = "Provinceedu",label = "Province:", choices = split(eddf$Province[c(1:31)],eddf$Region)),
             hr(),
             submitButton('Submit'),
             width = 4
           ),
           
           mainPanel(
             tabsetPanel(
               
               
               tabPanel("Explore Relationships",
                        br(),
                        textOutput(outputId = "statementedu"),
                        
                        hr(),
                        plotOutput(outputId = "scatteredu"),
                        hr(),
                        h4("Conclusion:"),
                        textOutput(outputId = "correlationedu"),
                        textOutput(outputId = "judgeedu")
               ),
               
               
               tabPanel("Annual Changes in Birthrates and Education Funds",
                        br(),
                        plotOutput(outputId = "indexedu"),
                        hr(),
                        plotOutput(outputId = "BRedu")
               ),
               
               
               tabPanel("Overview of Indicators",
                        br(),
                        h5("The boxplot below shows the relationships between 31 provinces' education funds, respectively.
                  It can be found that in Guangdong, the government invests the largest amount of money in education."),
                        hr(),
                        plotOutput(outputId = "educhangeedu"),
                        hr(),
                        plotOutput(outputId = "BRBRedu"),
                        
               ),
               
               
               tabPanel("Summary",
                        br(),
                        h4("The following provinces have a relationship between birthrate and Education fund (Pearson Correlation Coefficient > 0.2):"),
                        plotOutput(outputId = "summaryedu"),
                        
               ),
               
             ),
           )    
    
  )
  
  ),
  
# ========================Economy==============================

  tabPanel("Economy",
           sidebarLayout(
             
             sidebarPanel( 
               # Define the sidebar with one input
               
               h4("Please select one province and one economic indicator"),
               h5("(EFFECTIVE TO THE FIRST TWO FUNCTIONS)"),
               
               hr(),
               
               selectInput(inputId = "Province",label = "Province:", choices = split(df$Province[c(1:31)],df$Region)),
               hr(),
               
               selectInput(inputId = "type", label="Economic Indicator:",choices = colnames(df[c(5:7)])),
               hr(),
               submitButton('Submit'),width = 4
             ),
             
             # Create a spot for the plot
             mainPanel(
               tabsetPanel(
                 
                 tabPanel("Explore Relationships",
                          br(),
                          textOutput(outputId = "statement"),
                          
                          hr(),
                          plotOutput(outputId = "scatter"),
                          hr(),
                          h4("Conclusion:"),
                          textOutput(outputId = "correlation"),
                          textOutput(outputId = "judge")
                 ),
                 
                 tabPanel("Annual Changes in Birthrates and Economy Indicators",
                          br(),
                          plotOutput(outputId = "index"),
                          hr(),
                          plotOutput(outputId = "BR")
                 ),
                 
                 tabPanel("Overview of Indicators",
                          br(),
                          h5("The boxplot below shows the relationships between 31 provinces' economies, respectively. For instance, checking the following first plot, we can see that in Beijing and Shanghai, 
                    people earn the highest average salaries from 2001 to 2019."),
                          hr(),
                          plotOutput(outputId = "wagechange"),
                          hr(),
                          plotOutput(outputId = "cpichange"),
                          hr(),
                          plotOutput(outputId = "grpchange"),
                          hr(),
                          plotOutput(outputId = "Birthrate_change")
                 ),
                 
                 tabPanel("Summary",
                          br(),
                          h4("The following provinces have a relationship between birthrate and Wages (Pearson Correlation Coefficient > 0.2):"),
                          plotOutput(outputId = "summarywage"),
                          hr(),
                          h4("The following provinces have a relationship between birthrate and CPI (Pearson Correlation Coefficient > 0.2): "),
                          plotOutput(outputId = "summarycpi"),
                          hr(),
                          h4("The following provinces have a relationship between birthrate and GRP (Pearson Correlation Coefficient > 0.2): "),
                          plotOutput(outputId = "summarygrp")
                 ),
                 
               ),
             )
             
           )
           
           ),

# ==================public health======================
  tabPanel("Public Health",
           sidebarLayout(
           
    sidebarPanel(
      selectInput('healthProvince', 'Select Province', choices = c("All Provinces",healthData$Province[c(1:31)])),
      sliderInput(inputId = "healthYear",label="Select Year (Select  2020 for all the years 2001-2019)"
                  ,min = 2001,max=2020,value=2019),
      submitButton("Submit")
        
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data",
                 h3("The following choices are selected:"),
                 textOutput("stateProvince"),
                 textOutput("stateYear"),
                 br(),
                 h3("Here is the data selected:"),
                 tableOutput("healthTable")),
        tabPanel("Trend Plots for different provinces",
                 h3("Year selection does not work in this tab"),
                 h3("<All Provinces> choice are set to Beijing as default"),
                 plotOutput("plot_Annual_BR"),
                 br(),
                 plotOutput("plot_Annual_Health")),
        tabPanel("Details for different years",
                 h3("Province selection does not work in this tab"),
                 h3("<All Years(2020)> choice are set to 2019 as default"),
                 plotOutput("plot_Provinces_BR"),
                 br(),
                 plotOutput("plot_Provinces_Health")),
        
        
        tabPanel("Summary",
                 br(),
                 h4("The following provinces have a relationship between birthrate and health care (Pearson Correlation Coefficient > 0.2):"),
                 plotOutput(outputId = "summaryph"),
                 
        ),
        
        
      ),
    
      )
    
  

  )       
  ),

# =========dataset========
tabPanel("Datasets Used",
  
         sidebarLayout(
           sidebarPanel( 
             br(),
             br(),
             h4("Click the four tabs to check the instructions:"),
             br(),
             br(),
             width = 4
           ),
           
           mainPanel(
             tabsetPanel(
               tabPanel("Birthrate",
                        
                        dataTableOutput("Birthrate11") ),

               tabPanel("Economy",
                        dataTableOutput("Economy11")),

               tabPanel("Education",
                        dataTableOutput("Education11")),
               
               tabPanel("Healthcare",
                        dataTableOutput("Healthcare11")),
               
             ),
             
           ))
               
),

# ===============user guide==============
tabPanel("User Guide",
         
         sidebarLayout(
           sidebarPanel( 
             br(),
             br(),
             h4("Click the five tabs to check the instructions:"),
             br(),
             br(),

             width = 4
           ),
         
         
         mainPanel(
           tabsetPanel(
             
             
             tabPanel("Map of China",
                      
                      br(),
                      h4("* On the left, you can see a table listing the averages of the birthrates from 2001 to 2019 across 31 provinces. You can check each province one by one, or you can use the search box to find the province you wish."),
                      hr(),
                      h4("* Using the triangular arrow, you can sort the list. You can, for example, click the arrow of birthrate to make it ascend."),
                      hr(),
                      h4("* Two maps can be found on the right. One of them is a map with 31 provinces, and another is divided by 7 geographical religions."),
                      hr(),
                      h4("* You can firstly read the maps, and then choose one province you are interested in to check the average birthrates."),
                      hr(),
                      
                       ),
             
             tabPanel("Birthrate",
                      
                      br(),
                      h4("* This part explores the birthrates in 31 provinces."),
                      hr(),
                      h4("* Before you do any operation, you need to choose one or all provinces and one year and click submit."),
                      hr(),
                      h4("* The first tab shows the differences in birthrates between 31 provinces within the selected years."),
                      hr(),
                      h4("* The second tab shows the annual changes in birthrates within the selected provinces."),
                      
                      ),
             
             
             tabPanel("Education",
                      br(),
                      h4("* This part explores the relationship between birthrates and education funds in 31 provinces."),
                      hr(),
                      h4("* Click 'Explore Relationships' to explore the relationship between birthrates and education funds within one selected province. You need to select one province before checking it."),
                      hr(),
                      h4("* Click 'Annual Changes in Birthrates and Education Funds' to check the changes of the variables within one selected province. You need to select one province before checking it."),
                      hr(),
                      h4("* The fourth button shows an overview of education funds and the birthrates. The variables are displayed for 31 provinces from 2001 to 2019."),
                      hr(),
                      h4("* The 'Summary' button is a summary of the provinces where education funds are related to birthrates.")
                      ),
             
             tabPanel("Economy",
                      
                      br(),
                      h4("* This part explores the relationship between birthrates and three different economic indicators in 31 provinces."),
                      hr(),
                      h4("* Click 'Explore Relationships' to explore the relationship between birthrates and three different economic indicators within one selected province. You need to select one province and one indicator before checking it."),
                      hr(),
                      h4("* Click 'Annual Changes in Birthrates and Economy Indicators' to check the changes of the variables within one selected province. You need to select one province and one indicator before checking it."),
                      hr(),
                      h4("* The fourth button shows an overview of economic indicators and the birthrates. The variables are displayed for 31 provinces from 2001 to 2019."),
                      hr(),
                      h4("* The 'Summary' button is a summary of the provinces where economic indicators are related to birthrates.")
                      
                      ),
             
             tabPanel("Public Health",
                      br(),
                      h4("* This part explores the relationship between birthrates and the number of health care institutions in 31 provinces."),
                      hr(),
                      h4("* Before you do any operation, you need to choose one or all provinces and one year and click submit."),
                      hr(),
                      h4("* The first tab displays the data within the province and year you selected."),
                      hr(),
                      h4("* The second tab displays the annual changes in birthrates and health institutions in the selected province."),
                      hr(),
                      h4("* The third tab displays the differences in birthrates and numbers of healthcares between 31 provinces within the selected year."),
                      hr(),
                      h4("* The 'Summary' button is a summary of the provinces where numbers of healthcares are related to birthrates."),
                      hr(),
                      ),
             
             tabPanel("Datasets used",
                      br(),
                      h4("* You can explore all four datasets used in the application."),
                      hr(),
                      h4("* Click on one of the tabs to choose a dataset."),
                      hr(),
                      h4("* Each column has a triangular arrow that enables you to sort the dataset."),
                      
             ),
             
           ),
           
         ))
         
),

  
  
)
)

server <- function(input,output){
# =====education=========  
  output$scatteredu <- renderPlot(
    {
      select_data = filter(eddf,eddf$Province == input$Provinceedu)
      ggplot(data = select_data, aes_string(x = "Education_Fund" ,y= "Birthrate")) + 
        geom_point()+ geom_smooth(method = 'lm', se = F, color = 'red') + scale_x_continuous() +
        labs(title = paste( "Education Fund vs Birthrate",":",input$Provinceedu)) + 
        theme(plot.title = element_text(hjust = 0.5,size = 20)) + stat_cor(method = "pearson")+
        scale_x_continuous(breaks = seq(0,50000000,1000000))
    }
  )
  
  
  output$statementedu <- renderText(
    {
      select_data = filter(eddf,eddf$Province == input$Provinceedu)
      num <- round(mean(select_data$Birthrate),3)
      num2 <- round(mean(select_data$Education_Fund),3)
      
      
      paste(input$Provinceedu,"is a province in the",select_data$Region[1],".","The average birthrate from 2001 to 2019 is",num,".",
            "The average education fund from 2001 to 2019 is",num2,".")
    }
  )
  
  output$correlationedu <- renderText(
    {
      select_data = filter(eddf,eddf$Province == input$Provinceedu)
      
      Cor = cor(select_data$Education_Fund,select_data$Birthrate)
      Cor =  round(Cor,3)
      paste("The Pearson Correlation Coefficient value is",Cor,".")
      
    }
  )
  
  output$judgeedu <- renderText(
    {
      select_data = filter(eddf,eddf$Province == input$Provinceedu)
      
      Cor = cor(select_data$Education_Fund,select_data$Birthrate)
      Cor =  round(Cor,3)
      if(abs(Cor) >= 0.5){
        out =  "The strength of relationship is Strong."
      }
      else if(abs(Cor)>0.2&abs(Cor)<0.5){
        out = "The strength of relationship is Moderate."
      }
      else if(abs(Cor)<= 0.2){
        out = "The strength of relationship is None or very weak."
      }
      out
    }
  )
  
  
  output$BRedu <- renderPlot(
    
    {
      select_data = filter(eddf,eddf$Province == input$Provinceedu)
      ggplot(data = select_data, aes_string(x = "Year", y = "Birthrate")) + 
        geom_line()+geom_point()+ scale_x_continuous(breaks = seq(2001,2019,1)) + 
        labs(title = paste("Birthrate",":",input$Provinceedu)) + theme(plot.title = element_text(hjust = 0.5,size = 20)) 
      
    }
  )
  
  output$indexedu <- renderPlot(
    
    {
      select_data = filter(eddf,eddf$Province == input$Provinceedu)
      ggplot(data = select_data, aes_string(x = "Year", y = "Education_Fund")) + 
        geom_line()+geom_point()+scale_x_continuous(breaks = seq(2001,2019,1)) + 
        labs(title = paste("Education Fund",":",input$Provinceedu)) + theme(plot.title = element_text(hjust = 0.5,size = 20))+
        scale_y_continuous(breaks = seq(0,50000000,1000000))
      
    }
  )
  
  
  output$educhangeedu <- renderPlot(
    {
      ggplot(data = eddf, aes_string(x = "Province", y = "Education_Fund")) + geom_boxplot()+
        stat_boxplot(geom="errorbar",width = 0.15) + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(title = "Education Fund") + theme(plot.title = element_text(hjust = 0.5,size = 20))
    }
  )
  
  output$BRBRedu <- renderPlot(
    {
      ggplot(data = eddf, aes_string(x = "Province", y = "Birthrate")) + geom_boxplot()+
        stat_boxplot(geom="errorbar",width = 0.15) + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
        labs(title = "Birthrate") + theme(plot.title = element_text(hjust = 0.5,size = 20))
    }
  )
  
  
  output$summaryedu <- renderPlot(
    {
      a <- read.csv('Education.csv')
      edutable <- data.frame(ncol=3)
      number = 1
      for(i in a$Province[c(1:31)]){
        a <- filter(a, a$Province == i)
        v <- cor(a$Education_Fund,a$Birthrate)
        v <- round(v, 3)
        if(abs(v)>0.2){
          edutable[number,1] = i
          edutable[number,2] = v
          edutable[number,3] = a$Region[c(1)]
          number = number + 1
        }
        a <- read.csv('Education.csv')
      }
      colnames(edutable) <- list("Province","Pearson_Correlation_Coefficient","Region")
      ggplot(data = edutable, aes(x=reorder(Province,-Pearson_Correlation_Coefficient),y= Pearson_Correlation_Coefficient,fill=Province,label = Pearson_Correlation_Coefficient))+
        geom_col(show.legend = FALSE)+coord_flip() +geom_text(nudge_x = 0.1) +ylab('Pearson Correlation Coefficient')+
        xlab('Province') +theme_clean()+scale_y_continuous(breaks = seq(-1,1,0.1))
    }
  )
# ========================  
# =========Economy============
  
  output$scatter <- renderPlot(
    {
      select_data = filter(df,df$Province == input$Province)
      ggplot(data = select_data, aes_string(x = input$type ,y= "Birthrate")) + 
        geom_point()+ geom_smooth(method = 'lm', se = F, color = 'red') + scale_x_continuous() +
        labs(title = paste( input$type ,"vs Birthrate",":",input$Province)) + 
        theme(plot.title = element_text(hjust = 0.5,size = 20)) + stat_cor(method = "pearson")
    }
  )
  
  output$statement <- renderText(
    {
      select_data = filter(df,df$Province == input$Province)
      type = input$type
      num <- round(mean(select_data$Birthrate),3)
      num2 = select_data[type]
      num2 = sapply(num2,mean,na.rm=T)
      num3 <- round(num2,3)
      paste(input$Province,"is a province in the",select_data$Region[1],".","The average birthrate from 2001 to 2019 is",num,".",
            "The average",input$type,"from 2001 to 2019 is",num3,".")
    }
  )
  
  
  output$correlation <- renderText(
    {
      select_data = filter(df,df$Province == input$Province)
      type = input$type
      
      Cor = cor(select_data[type],select_data$Birthrate)
      Cor =  round(Cor,3)
      
      paste("The Pearson Correlation Coefficient value is",Cor,".")
      
    }
  )
  
  output$judge <- renderText(
    {
      select_data = filter(df,df$Province == input$Province)
      type = input$type
      
      Cor = cor(select_data[type],select_data$Birthrate)
      Cor =  round(Cor,3)
      
      if(abs(Cor) >= 0.5){
        out =  "The strength of relationship is Strong."
      }
      else if(abs(Cor)>0.2&abs(Cor)<0.5){
        out = "The strength of relationship is Moderate."
      }
      else if(abs(Cor)<=0.2){
        out = "The strength of relationship is None or very weak."
      }
      out
    }
  )
  
  output$BR <- renderPlot(
    
    {
      select_data = filter(df,df$Province == input$Province)
      ggplot(data = select_data, aes_string(x = "Year", y = "Birthrate")) + 
        geom_line()+geom_point()+ scale_x_continuous(breaks = seq(2001,2019,1)) + 
        labs(title = paste("Birthrate",":",input$Province)) + theme(plot.title = element_text(hjust = 0.5,size = 20)) 
      
    }
  )
  
  output$index <- renderPlot(
    
    {
      select_data = filter(df,df$Province == input$Province)
      ggplot(data = select_data, aes_string(x = "Year", y = input$type)) + 
        geom_line()+geom_point()+scale_x_continuous(breaks = seq(2001,2019,1)) + 
        labs(title = paste(input$type,":",input$Province)) + theme(plot.title = element_text(hjust = 0.5,size = 20)) 
      
    }
  )
  
  output$wagechange <- renderPlot(
    {
      ggplot(data = df, aes_string(x = "Province", y = "Wage")) + geom_boxplot()+
        stat_boxplot(geom="errorbar",width = 0.15) + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(title = "Wage") + theme(plot.title = element_text(hjust = 0.5,size = 20)) 
    }
  )
  
  output$cpichange <- renderPlot(
    {
      
      ggplot(data = df, aes_string(x = "Province", y = "CPI")) + geom_boxplot()+
        stat_boxplot(geom="errorbar",width = 0.15) + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(title = "CPI") + theme(plot.title = element_text(hjust = 0.5,size = 20)) 
    }
  )
  
  output$grpchange <- renderPlot(
    {
      ggplot(data = df, aes_string(x = "Province", y = "GRP")) + geom_boxplot()+
        stat_boxplot(geom="errorbar",width = 0.15) + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(title = "GRP") + theme(plot.title = element_text(hjust = 0.5,size = 20)) 
    }
  )
  
  
  output$Birthrate_change <- renderPlot(
    {
      ggplot(data = df, aes_string(x = "Province", y = "Birthrate")) + geom_boxplot()+
        stat_boxplot(geom="errorbar",width = 0.15) + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
        labs(title = "Birthrate") + theme(plot.title = element_text(hjust = 0.5,size = 20))
    }
  )
  
  output$summarywage <- renderPlot(
    {
      a <- read.csv('Economy2.csv')
      wagetable <- data.frame(ncol=3)
      number = 1
      for(i in a$Province[c(1:31)]){
        a <- filter(a, a$Province == i)
        v <- cor(a$Wage,a$Birthrate)
        v <- round(v, 3)
        if(abs(v)>0.2){
          wagetable[number,1] = i
          wagetable[number,2] = v
          wagetable[number,3] = a$Region[c(1)]
          number = number + 1
        }
        a <- read.csv('Economy2.csv')
      }
      colnames(wagetable) <- list("Province","Pearson_Correlation_Coefficient","Region")
      ggplot(data = wagetable, aes(x=reorder(Province,-Pearson_Correlation_Coefficient),y= Pearson_Correlation_Coefficient,fill=Province,label = Pearson_Correlation_Coefficient))+
        geom_col(show.legend = FALSE)+coord_flip() +geom_text(nudge_x = 0.1) +ylab('Pearson Correlation Coefficient')+
        xlab('Province') +theme_clean()+scale_y_continuous(breaks = seq(-1,1,0.1))
    }
  )
  
  output$summarycpi <- renderPlot(
    {
      a <- read.csv('Economy2.csv')
      rendertable <- data.frame(ncol=3)
      number = 1
      for(i in a$Province[c(1:31)]){
        a <- filter(a, a$Province == i)
        v <- cor(a$CPI,a$Birthrate)
        v <- round(v, 3)
        if(abs(v)>0.2){
          rendertable[number,1] = i
          rendertable[number,2] = v
          rendertable[number,3] = a$Region[c(1)]
          number = number + 1
        }
        a <- read.csv('Economy2.csv')
      }
      colnames(rendertable) <- list("Province","Pearson_Correlation_Coefficient","Region")
      ggplot(data = rendertable, aes(x= reorder(Province,-Pearson_Correlation_Coefficient),y= Pearson_Correlation_Coefficient,fill=Province,label = Pearson_Correlation_Coefficient))+
        geom_col(show.legend = FALSE)+coord_flip()+geom_text(nudge_x = 0.1)+ylab('Pearson Correlation Coefficient')+
        xlab('Province')+theme_clean()+scale_y_continuous(breaks = seq(-1,1,0.1))
    }
  )
  
  output$summarygrp <- renderPlot(
    {
      a <- read.csv('Economy2.csv')
      grptable <- data.frame(ncol=3)
      number = 1
      for(i in a$Province[c(1:31)]){
        a <- filter(a, a$Province == i)
        v <- cor(a$GRP,a$Birthrate)
        v <- round(v, 3)
        if(abs(v)>0.2){
          grptable[number,1] = i
          grptable[number,2] = v
          grptable[number,3] = a$Region[c(1)]
          number = number + 1
        }
        a <- read.csv('Economy2.csv')
      }
      colnames(grptable) <- list("Province","Pearson_Correlation_Coefficient","Region")
      ggplot(data = grptable, aes(x=reorder(Province,-Pearson_Correlation_Coefficient,),y= Pearson_Correlation_Coefficient,fill=Province,label = Pearson_Correlation_Coefficient))+
        geom_col(show.legend = FALSE)+coord_flip()+geom_text(nudge_x = 0.1)+theme_clean()+ylab('Pearson Correlation Coefficient')+
        xlab('Province')+scale_y_continuous(breaks = seq(-1,1,0.1))
    }
  )
  
# ======================================
  
# ===========healthcare==============
  output$stateProvince <-renderText({
    input$healthProvince
  })
  
  output$stateYear <-renderText({
    input$healthYear
  })
  
  output$healthTable <-renderTable({
    selectHealth = healthData
    if(input$healthYear != 2020){
      selectHealth = filter(selectHealth,selectHealth$Year == input$healthYear)
    }
    if(input$healthProvince != "All Provinces"){
      selectHealth = filter(selectHealth,selectHealth$Province == input$healthProvince)
    }
    selectHealth
  })
  
  
  output$plot_Annual_BR <- renderPlot(
    {
      selectProvinceHealth = healthData
      healthProvinceTemp = input$healthProvince
      if(healthProvinceTemp == "All Provinces"){
        healthProvinceTemp = "Beijing"
      }
      selectProvinceHealth = filter(selectProvinceHealth,selectProvinceHealth$Province == healthProvinceTemp)
      ggplot(data = selectProvinceHealth, aes_string(x = "Year", y = "Birthrate")) + 
        geom_line()+geom_point()+scale_x_continuous(breaks = seq(2001,2019,1))+
        labs(title = paste("Birthrate",":",healthProvinceTemp)) +
        theme(plot.title = element_text(hjust = 0.5,size = 20)
        )
    }
  )
  
  output$plot_Annual_Health <- renderPlot(
    {
      selectProvinceHealth = healthData
      healthProvinceTemp = input$healthProvince
      if(healthProvinceTemp == "All Provinces"){
        healthProvinceTemp = "Beijing"
      }
      selectProvinceHealth = filter(selectProvinceHealth,selectProvinceHealth$Province == healthProvinceTemp)
      ggplot(data = selectProvinceHealth, aes_string(x = "Year", y = "Healthcare")) + 
        geom_line()+geom_point()+scale_x_continuous(breaks = seq(2001,2019,1))+
        labs(title = paste("Healthcare",":",healthProvinceTemp)) +
        theme(plot.title = element_text(hjust = 0.5,size = 20)
        )
    }
  )
  
  output$plot_Provinces_BR <- renderPlot(
    {
      selectHealth = healthData
      healthYearTemp = input$healthYear
      if(healthYearTemp == "2020"){
        healthYearTemp = "2019"
      }
      selectHealth = filter(selectHealth,selectHealth$Year == healthYearTemp)
      ggplot(data = selectHealth, aes_string(x = "Province", y = "Birthrate")) + 
        geom_bar(stat="identity") + 
        labs(title = paste("Birthrate",":",healthYearTemp)) + 
        theme(plot.title = element_text(hjust = 0.5,size = 20)) + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
    }
  )
  
  output$plot_Provinces_Health <- renderPlot(
    {
      selectHealth = healthData
      healthYearTemp = input$healthYear
      if(healthYearTemp == "2020"){
        healthYearTemp = "2019"
      }
      selectHealth = filter(selectHealth,selectHealth$Year == healthYearTemp)
      ggplot(data = selectHealth, aes_string(x = "Province", y = "Healthcare")) + 
        geom_bar(stat="identity") + 
        labs(title = paste("Healthcare",":",healthYearTemp)) + 
        theme(plot.title = element_text(hjust = 0.5,size = 20)) + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))
    }
  )
  
  output$summaryph <- renderPlot(
    {
      a <- read.csv('transform_data.csv')
      edutable <- data.frame(ncol=2)
      number = 1
      for(i in a$Province[c(1:31)]){
        a <- filter(a, a$Province == i)
        v <- cor(a$Healthcare,a$Birthrate)
        v <- round(v, 3)
        if(abs(v)>0.2){
          edutable[number,1] = i
          edutable[number,2] = v
          number = number + 1
        }
        a <- read.csv('transform_data.csv')
      }
      colnames(edutable) <- list("Province","Pearson_Correlation_Coefficient")
      ggplot(data = edutable, aes(x=reorder(Province,-Pearson_Correlation_Coefficient),y= Pearson_Correlation_Coefficient,fill=Province,label = Pearson_Correlation_Coefficient))+
        geom_col(show.legend = FALSE)+coord_flip() +geom_text(nudge_x = 0.1) +ylab('Pearson Correlation Coefficient')+
        xlab('Province') +theme_clean()+scale_y_continuous(breaks = seq(-1,1,0.1))
    }
  )
  
  
  
  
# ==================================

# ==========Birthrate=============
  output$Map <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- normalizePath(file.path(
      paste('Map', input$n, '.png', sep='')))
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$BirthrateTablegn <-renderDataTable({
    dfgn2
  })
  
  output$plot_Provinces_BRgn <- renderPlot(
    {
      selectBirthrate = dfgn
      BirthrateYearTemp = input$Yeargn
      selectBirthrate= filter(selectBirthrate,selectBirthrate$Year == BirthrateYearTemp)
      ggplot(data = selectBirthrate, aes_string(x = "Birthrate", y = "Province")) + 
        geom_bar(stat="identity") + 
        labs(title = paste("Birthrate",":",BirthrateYearTemp)) + 
        theme(plot.title = element_text(hjust = 0.5,size = 20))+ theme_clean()
    }
  )
  
  output$BRgn <- renderPlot(
    
    {
      select_data = filter(dfgn,dfgn$Province == input$Provincegn)
      ggplot(data = select_data, aes_string(x = "Year", y = "Birthrate")) + 
        geom_line()+geom_point()+ scale_x_continuous(breaks = seq(2001,2019,1)) + 
        labs(title = paste("Birthrate",":",input$Provincegn)) + theme(plot.title = element_text(hjust = 0.5,size = 20)) 
    }
  )

# =================================
# ======Dataset=======
  output$Birthrate11 <-renderDataTable({
    dfgn
  })
  
  output$Economy11 <-renderDataTable({
    df
  })
  
  output$Healthcare11 <-renderDataTable({
    healthData
  })
  
  output$Education11 <-renderDataTable({
    eddf
  })
  
  
}


shinyApp(ui = ui, server = server)

