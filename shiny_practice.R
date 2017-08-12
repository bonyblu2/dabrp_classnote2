library(gapminder)

library(gridExtra)
library(dplyr)
library(ggplot2)
p <- ggplot(gapminder)
plot1 <- p + aes(x=pop) + geom_histogram()
plot2 <- p + aes(x=continent, y=lifeExp) + geom_violin() + stat_summary(color="blue")
continent_freq <- gapminder %>% count(continent)
plot3 <- ggplot(continent_freq, aes(x = continent, y = n)) + geom_bar(stat = "identity")
jCountries <- c("Canada", "Rwanda", "Cambodia", "Mexico")
plot4 <- gapminder %>% filter(country %in% jCountries) %>%
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line() + geom_point()
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)

plot4
summary(gapminder$country)

library(shiny)

ui<-fluidPage(
  titlePanel("실습2"),
  sidebarPanel(
    checkboxGroupInput("dataSelect",
                       "나라 선택", 
                       choices=list("Canada"="Canada",
                                    "Rwanda"="Rwanda",
                                    "Cambodia"="Cambodia",
                                    "Mexico"="Mexico"),
                       selected = "Canada")
  ),
  mainPanel(
    plotOutput("out1")
  )
)
server<-function(input, output){
  output$out1 <- renderPlot({
    plot4 <- gapminder %>% filter(country %in% input$dataSelect) %>%
      ggplot(aes(x = year, y = lifeExp, color = country)) +
      geom_line() + geom_point()
    plot4
  })
}

shinyApp(ui,server)