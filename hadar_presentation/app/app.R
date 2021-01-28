#
# https://raw.githubusercontent.com/Sefaria/Sefaria-Export/master/json/Tanakh/Prophets/Isaiah/English/The%20Holy%20Scriptures%20A%20New%20Translation%20JPS%201917.json
#

library(cluster)  # Generate the clusters
library(shinydashboard)  # Make it look better
library(tidyverse)

load(".RData")  # Pull in the source data
source("textBlocks.R")

options(scipen = 999)  # Who like reading scientific notation?

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "ML & Biblical Criticism"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        sidebarMenu(
            menuItem("Exploration", tabName = "exploration", icon = icon("compass")),
            menuItem("Clusters",
                     menuSubItem("Isaish 24-27", tabName = "cluster"),
                     icon = icon("project-diagram"),
                     sliderInput("clusters",
                                 "Number of clusters:",
                                 min = 1,
                                 max = 7,
                                 value = 3)
                        )
                     ),
            menuItem("References", tabName = "reference", icon = icon("book"))
    ),

        # Show a plot of the generated distribution
        dashboardBody(
            tabItems(
                tabItem(tabName = "exploration",
                        h2(textBody$title),
                        h4(textBody$byline),
                        hr(),
                        h4(textBody$overview),
                        HTML("<p><a href='https://github.com/ChaimKesler/isaiah_topics/tree/main/hadar_presentation'>Github link to this presentation</a></p>"),
                        hr(),
                        h2("Example Text: Isaish 24-27"),
                        hr(),
                        h4("Top Words by Chapter"),
                        plotOutput("topWords"),
                        hr(),
                        h4("Term Frequency by Chapter"),
                        hr(),
                        plotOutput("termFrequency")
                        
                        
                ),
                tabItem(tabName = "cluster",
                        h1("K-Means Cluster Analysis"),
                        h2("Plot of Clusters"),
                        plotOutput("clustPlot"),
                        hr(),
                        h2("Clusters by Chapter"),
                        hr(),
                        plotOutput("chapPlot"),
                        hr(),
                        tableOutput("clusterTable")
                        ),
                tabItem(tabName = "reference",
                        h1("References"),
                        hr("Isaiah Text Links"),
                        HTML("<p><a href='https://www.academic-bible.com/en/online-bibles/septuagint-lxx/read-the-bible-text/bibel/text/lesen/stelle/23/240001/249999/ch/b3e077594395fe304c23a04ace3ba137/'>Septuagint Isaiah 24</a></p>"),
                        HTML("<p><a href='https://www.academic-bible.com/en/online-bibles/biblia-hebraica-stuttgartensia-bhs/read-the-bible-text/bibel/text/lesen/stelle/23/240001/249999/ch/dd8d0a75af0e6577fcf9beaf367e6c12/'>MT Hebrew Isaiah 24</a></p>"),
                        HTML("<p><a href='https://www.sefaria.org/Isaiah.24?lang=bi'>Seferia Hebrew/English 24</a></p>"),
                        hr("NLP Programming Documentation"),
                        HTML("<p><a href='https://www.tidytextmining.com/'>R: Tidy Text R Package</a></p>"),
                        HTML("<p><a href='https://www.tidytextmining.com/preface.html'>R: Text Mining Book</a></p>"),
                        HTML("<p><a href='https://smltar.com/'>R: Supervised Machine Learning for Text Analysis in R</a></p>"),
                        HTML("<p><a href='https://github.com/Sefaria/Sefaria-Export'>Bible Text: Full Mongo Database from Sefaria</a></p"),
                        hr(),
                        hr("Related Academic Articles"),
                        HTML("<p><a href='https://aaai.org/ocs/index.php/FLAIRS/FLAIRS17/paper/view/15541)'>What’s Wrong with Hebrew NLP? And How to Make it Right</a></p>"), 
                        HTML("<p><a href='https://www.aclweb.org/anthology/D19-3044.pdf'>Can Natural Language Processing Help Identify the Author(s) of the Book of Isaiah?</a></p>")
                        )
            )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$topWords <- renderPlot({
        plotdf_custom %>%
            filter(n > 2) %>%
            ggplot(aes(n, word, fill=as.factor(chapter))) +
            geom_bar(position="stack",stat = "identity") +
            labs(y = NULL) +
            scale_fill_brewer(palette="Set2")
    })
    
    output$termFrequency <- renderPlot(
        ggplot(plotdf, aes(n/total, fill = chapter)) +
            geom_histogram(show.legend = FALSE, bins = 10) +
            facet_wrap(~chapter, ncol = 2)
    )
    
    renderCluster <- function(){
        model <- kmeans(kmeans.data_custom, input$clusters)
        model
    }

    output$clustPlot <- renderPlot({
        
        model <- renderCluster()
        # generate bins based on input$bins from ui.R
        clusplot(kmeans.data_custom, 
                 model$cluster,
                 color=TRUE,
                 shade=TRUE, 
                 labels=2,
                 lines=0)
    })
    
    output$chapPlot <- renderPlot({
        model <- renderCluster()
        df <- as_tibble(kmeans.data_custom)
        df <- df[,order(names(df))]
        plot <- with(plotdf, pairs(df, col=c(1:input$clusters)[model$cluster]))
        plot})
}

# Run the application 
shinyApp(ui = ui, server = server)
