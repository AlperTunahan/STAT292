

library(shiny)

#load packages. install if missing
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(gganimate)) install.packages("gganimate"); library(gganimate)
if(!require(plotly)) install.packages("plotly"); library(plotly)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(reshape2)) install.packages("reshape2"); library(reshape2)
if(!require(shinythemes)) install.packages("shinythemes"); library(shinythemes)
if(!require(gifski)) install.packages("gifski"); library(gifski)

#read data with N/A values as NA
data_2018 <- read.csv("data/2018.csv", na.strings = c("N/A"))
data_2019 <- read.csv("data/2019.csv", na.strings = c("N/A"))
#combine data adding year
data18 <- data_2018 %>%
  mutate("Year" = 2018)
data19 <- data_2019 %>%
  mutate("Year" = 2019)
data_all <- rbind(data18, data19)

# store country names
countries <- sort(as.vector(data_2019$Country.or.region))
# store column names. exclude rank and country name
variables <- names(data_2019)[ -c(1,2)]


ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(title = "Don't worry, be HAPPY!",
             ### INTRO PANEL ###
             tabPanel(title = "About App",
                      mainPanel(
                        br(),
                        h2("What is this app?"),
                        br(),
                        p("In this R Shiny Application that we wrote-down, it could be made different activities 
                        on a large scale. Our application gathers data from the outcome of the countries' people.
                        The data information tab, according to the year and the number of Countries selected, 
                        gives outcomes of;The total happiness score of the country, Life expectancy concerning 
                          health , Social Supports etc. Also the variable chosen in the program can be sorted 
                          as increasing or as decreasing by the number the countries the User picks."),
                        br(),
                        p("In addition, the changes of the mentioned countries over the years can be seen thanks 
                        to the graph presented. It is also possible to see the components that give this result 
                        by this graphic. All of the countries can be compared by the wished variable by choosing 
                        in the Application. Every variable has been shown in a different colour, and the added-up 
                          of all the colours gives us the final score of the country. The 'Sum of Variable Scores', 
                          is consistent with the countries' ranking, which was sorted in the first part."),
                        br(),
                        p("The Relation between variables tab, as understood from its name, shows the scatter plot 
                          and the linear relationship (if any) between the independent variable x and the dependent 
                          variable y. The positive or negative relationship can be seen through the line which is 
                          drawn and the year can be selected to see the changes. The grey area surrounding the line 
                          is the Confidence interval. The more narrow the shaded area around the line, means we are 
                          more confident that our data falls in the same rift."),
                        br(),
                        p("And at the final part, it's up to the User. You can assume it's a game in creative mode. 
                          After choosing the variables of the Country you named, the ranking of your country 
                          compared to other world countries pop-out. The final score is essential in the ranking 
                          and the coefficients of each component has been provided in the formula. Have fun !"),
                        br()
                      )
             ),
             
             ### FIRST PANEL ###
             tabPanel(title = "Data Information",
                      sidebarPanel(width = "3",
                                   br(),
                                   selectInput(inputId = "year1", 
                                               label = "Year:",
                                               choices = c(2018, 2019)),
                                   numericInput(inputId = "top",
                                                label = "Number of Countries:",
                                                value = 10, min = 0, max = 156, step = 5),
                                   selectInput(inputId = "var", 
                                               label = "Sort via variable:",
                                               choices = c("Score", "GDP per capita",	
                                                           "Social Support",	"Healthy life expectancy",	"Freedom to make life choices",	
                                                           "Generosity",	"Perceptions of corruption")),
                                   checkboxInput(inputId = "reverse_order", 
                                                 label = "Reverse the order.", value = FALSE),
                                   HTML('<iframe width=100% height="315" src="https://www.youtube.com/embed/9OQ1sZE6bTg" 
                                   frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; 
                                        picture-in-picture" allowfullscreen></iframe>')
                      ),
                      mainPanel(
                        tags$blockquote("The happiness scores and rankings use data from the Gallup World Poll. 
                                        The scores are based on answers to the main life evaluation question asked in the poll. 
                                        This question, known as the Cantril ladder, asks respondents to think of a ladder with 
                                        the best possible life for them being a 10 and the worst possible life being a 0 and to 
                                        rate their own current lives on that scale."),
                        textInput(inputId = "search",
                                  label = "Search:",
                                  placeholder = "eg: Turkey"),
                        tableOutput("top_out")
                      )
             ),
             
             ### SECOND PANEL ###
             tabPanel(title = "Comparing Countries",
                      sidebarPanel(width = "auto",
                                   selectInput(inputId = "year2", label = "Year:",
                                               choices = c(2018, 2019)),
                                   selectInput(inputId = "country",
                                               multiple = TRUE,
                                               label = "Country (Select multiple to compare):",
                                               choices = countries,
                                               selected = c("Turkey", "United States", "Russia", "Finland", "China")),
                                   actionButton(inputId = "render",
                                                label = "Show Graphs"),
                                   
                                   
                      ),
                      
                      mainPanel(
                        fluidRow(
                          splitLayout(cellWidths = c("auto", "50%"),
                                      plotlyOutput("barplot_out"),
                                      imageOutput("gif_out",height = "100%")
                          )
                        ),
                        
                      ),
             ),
             
             ### THIRD PANEL ###
             tabPanel(title = "Relationship between variables",
                      sidebarPanel(
                        selectInput(inputId = "year3", label = "Year:",
                                    choices = c(2018, 2019)),
                        selectInput(inputId = "x", label = "x (independent variable)",
                                    choices = variables, selected = "GDP.per.capita"),
                        selectInput(inputId = "y", label = "y (dependent variable)",
                                    choices = variables)
                      ),
                      mainPanel(
                        plotlyOutput("scatterplot_out"),
                        textOutput("cor_out"),
                        textOutput("pval_out"),
                        helpText("Note:"),
                        helpText("r value indicates strength of relationship between variables: 
                                 Closer to 1 means strong positive relationship, closer to 0 means no 
                                 relationship and closer to -1 means strong negative relationship."),
                        helpText("If p-value is less than 0.05, it is safe to conclude that at 95% confidence
                                 level there is a relationship between variables x and y.")
                      )
             ),
             
             ### FOURTH PANEL ###
             tabPanel(title = "Create your own country!",
                      sidebarPanel(
                        textInput("CountryName", "Enter Your Country's Name"),
                        helpText("Very Weak = 0; Very Strong = 10"),
                        helpText("Higher rating means more positive!"),
                        sliderInput("gdp", "Rate your country's GDP per capita:",
                                    min = 0, max= 10, value= 0),
                        sliderInput("ssupport", "Rate your country's social support:",
                                    min = 0, max= 10 , value= 0),
                        sliderInput("lifexp", "Rate your country's healthy life expectancy:",
                                    min = 0, max= 10, value= 0),
                        sliderInput("ftmlc", "Rate your country's freedom to make life choices:",
                                    min = 0, max= 10, value= 0),
                        sliderInput("genr", "Rate your country's generosity:",
                                    min = 0, max= 10, value= 0),
                        sliderInput("crrp", "Rate your country's perceptions of corruption:",
                                    min = 0, max= 10, value= 0)
                      ),
                      mainPanel(
                        h4("You can create your utopic country, rate the variables and see the result!", color= "red"),
                        hr(),
                        tags$i("The formula used to calculate the score: 1.7952 + (GDP.per.capita*0.7754) + (Social support*1.1242) + 
                        (Healthy life expectancy*1.0781) + (Freedom to make life choices*1.4548) + (Generosity*0.4898) + 
                        (Perceptions of corruption*0.9723)"),
                        br(),
                        tags$i("The score below is used to find the rank of your country."),
                        br(),
                        textOutput("score_out"),
                        br(),
                        actionButton("cal", "Let's Go!"),
                        br(),
                        h4(tags$i(textOutput("rank_out")))
                      )
             ),
             
             ### REFERENCE PANEL ###
             tabPanel(title = "References",
                      mainPanel(
                        HTML('<img src="reference.png" alt="References">'),
                        br(),
                        br(),
                        h4(tags$i("1 -  Dataset retrived from Sustainable Development Solutions Network at Kaggle.com. 2020. 
                               World Happiness Report. [online] Available at: <https://www.kaggle.com/unsdsn/world-happiness> 
                               [Accessed 7 June 2020].")),
                        br(),
                        h4(tags$i("2 -  Ggplot2.tidyverse.org. 2020. Complete Themes - Ggtheme. [online] 
                               Available at: <https://ggplot2.tidyverse.org/reference/ggtheme.html> [Accessed 6 June 2020].")),
                        br(),
                        h4(tags$i("3 - Global happiness report. 2020. Roundtable. [online] 
                               Available at: <https://www.youtube.com/watch?v=9OQ1sZE6bTg&t=3s> [Accessed 6 June 2020].")),
                        br(),
                        h4(tags$i("4 - Plotly.com. 2020. Ggplot2 Graphing Library. [online] 
                               Available at: <https://plotly.com/ggplot2/> [Accessed 6 June 2020]..")),
                        br(),
                        h4(tags$i("5 - Shiny.rstudio.com. 2020. Shiny - Tutorial. [online] 
                               Available at: <https://shiny.rstudio.com/tutorial/>..")),
                        br(),
                        h4(tags$i("6 - Stack Overflow. 2020. 'R' Questions. [online] 
                                  Available at: <https://stackoverflow.com/questions/tagged/r>.")),
                        br(),
                        h4(tags$i("7 - Worldhappiness.report. 2020. Home. [online] 
                               Available at: <https://worldhappiness.report/> [Accessed 6 June 2020].")),
                      )
             )
  )
)

######################################## SERVER STARTS HERE #########################################
server <- function(input, output) {
  # function to select data (2018 or 2019)
  which_year <- function(x){
    if (x == 2018) return(data_2018)
    else return(data_2019)
  }
  ##########################
  ## Code for FIRST PANEL ##
  ##########################
  
  # Datatable output
  output$top_out <- renderTable({
    # request data for input year
    data_selected <- which_year(input$year1)
    # rename vars to make table fit
    names(data_selected) <- c("Overall Rank",	"Country or Region",	"Score", "GDP per capita",	
                    "Social Support",	"Healthy life expectancy",	"Freedom to make life choices",	
                    "Generosity",	"Perceptions of corruption")
    # determine if user is searching
    if(!(input$search == "")){
      # check if reverse ordering is true
      if(input$reverse_order == TRUE){
        # low values first
        dat <- data_selected[order(data_selected[, input$var], decreasing = FALSE),]
        dat <- cbind("Variable Rank" = 1:156, dat)
        # select countries matching the search
        a <- grep(input$search, dat[, 3], ignore.case = TRUE)
        dat[a,]
        }
      else {
        # high values first
        dat <- data_selected[order(data_selected[, input$var], decreasing = TRUE),]
        dat <- cbind("Variable Rank" = 1:156, dat)
        # select countries matching the search
        a <- grep(input$search, dat[,3], ignore.case = TRUE)
        dat[a,]
        }
      
    } else {
      # check if reverse ordering is true
      if(input$reverse_order == TRUE){
        # low values first
        dat <- data_selected[order(data_selected[, input$var], decreasing = FALSE),] %>%
          head(input$top)
        cbind("Variable Rank" = 1:input$top, dat)}
      else {
        # high values first
        dat <- data_selected[order(data_selected[, input$var], decreasing = TRUE),] %>%
          head(input$top)
        cbind("Variable Rank" = 1:input$top, dat)}
    }
  },bordered = TRUE, hover = TRUE)
  
  ###########################
  ## Code for SECOND PANEL ##
  ###########################
  
  # Stacked bar plot output
  output$barplot_out <- renderPlotly({
    # wait for click
    if(input$render == 0)
      return()
    isolate({
      # get user input countries' data from selected year
      data_tem <- which_year(input$year2) %>%
        filter(Country.or.region %in% input$country)
      # reshape data to fit bar plot
      data_reshaped <- melt(data_tem[, -c(1,3)], id.vars = "Country.or.region")
      # make bar plot with respect to variable values
      p1 <- ggplot(data_reshaped, aes(x = Country.or.region, y = value, fill = variable)) + 
        labs(x = "Countries", y="Sum of Variable Scores") +
        geom_bar(stat = "identity")
      # use plotly to make the plot interactive
      ggplotly(p1)
    })
  })
  
  # gif output 
  output$gif_out <- renderImage({
    # wait for click
    if(input$render == 0)
      return()
    isolate({
      # make a file with gif extension
      plot_outfile <- tempfile(fileext=".gif")
      # get data of countries matching user input
      selected_countries <- data_all %>%
        filter(Country.or.region %in% input$country)
      # plot scores of countries for each year
      p2 <- ggplot(selected_countries, aes(x = Year,y= Score, color = Country.or.region)) +
        geom_line() + geom_point() + transition_reveal(Year)
      # make it animate
      animate(p2, duration = 2, fps = 2, renderer = gifski_renderer())
      # save as gif
      anim_save("plot_outfile.gif")
      # get the gif
      list(src = "plot_outfile.gif",contentType = 'image/gif')
    })
  })
  
  ##########################
  ## Code for THIRD PANEL ##
  ##########################
  
  # plot selected columns and insert linear model line with conf.int
  plot_scatter <- reactive({
    which_year(input$year3) %>%
      ggplot(aes_string(input$x, input$y)) + geom_point() + geom_smooth(method = lm)})
  # Scatter plot output
  output$scatterplot_out <- renderPlotly({
    # use plotly to make the plot interactive
    ggplotly(plot_scatter())
  })
  
  # test correlation of user input columns
  correlate <- reactive({
    data_selected <- which_year(input$year3)
    t <- cor.test(data_selected[, input$x],data_selected[, input$y])
  })
  # Correlation coefficient output
  output$cor_out <- renderText(paste("r =", round(correlate()$estimate,3)))
  # P-value output
  output$pval_out <- renderText(paste("p-value =", round(correlate()$p.value,4)))
  
  ###########################
  ## Code for FOURTH PANEL ##
  ###########################
  
  # predict score of a made up country
  country_score<- reactive({
    # Note: lm() function is used to form this model
    rating_scale <- 10
    (1.7952 + input$gdp*(1.684/rating_scale)*0.7754 + (input$ssupport*(1.624/rating_scale)*1.1242) + 
        (input$lifexp*(1.141/rating_scale)*1.0781) + (input$ftmlc*(0.631/rating_scale)*1.4548) + 
        (input$genr*(0.566/rating_scale)*0.4898) + (input$crrp*(0.453/rating_scale)*0.9723))
  })
  # Score output
  output$score_out <- renderText({
    # make it zero if all user input is zero
    paste("Score:", if(country_score() == 1.7952){0} else country_score())
  })
  
  # calculate rank of user's country
  country_rank <- reactive({
    # take only Country names and scores from the data.
    d <- data.frame("Country" = as.vector(data_2019$Country.or.region), 
                    "Score" = data_2019$Score, stringsAsFactors = FALSE)
    # add user's country
    d <- rbind(d, c(input$CountryName, country_score()))
    # order. high scores first
    d <- d[order(d$Score, decreasing = TRUE),]
    # get index of user's country
    which(d$Country %in% input$CountryName)
  })
  
  # Rank output
  output$rank_out <- renderText({
    if(input$cal == 0)
      return()
    isolate({
      # prompt to name the country if not
      if(input$CountryName == "") paste("Name the country to see the rank!")
      # do not rank if not rated
      else if(country_score() == 1.7952)
        paste("Not ranked: Score is zero!")
      # first place
      else if(country_rank() == 1) paste(input$CountryName, "is the happiest country in the world!")
      # make sure it is not last place
      else if(!(country_rank() == 157))
        paste0(input$CountryName, " is the ", country_rank(), ". happiest country in the world! ",
               "Just between ", data_2019$Country.or.region[country_rank()], " and " ,
               data_2019$Country.or.region[country_rank()+1], ".")
      # last place
      else paste(input$CountryName, "is the least happy country in the world!")
    })
  })
}

shinyApp(ui, server)
