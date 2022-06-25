library(shiny)
library(shinyWidgets)
library(tidyverse)
library(fontawesome)
library(echarts4r)
library(here)

# Source helper functions
source(here("R", "samplers.R"))

ui <- fluidPage(
    # Head tags and styles
    tags$head(
        tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Mono", rel = "stylesheet"),
        includeCSS("www/styles.css")
    ),

    # Sidebar layout
    sidebarLayout(
        sidebarPanel(
            id = "sidebar",
            h1("MCMC Visualizer"),
            br(),
            p(paste0("Markov Chain Monte Carlo (MCMC) methods are primarily used for ",
                     "calculating numerical approximations of multi-dimensional integrals, ",
                     " for example in Bayesian statistics, computational physics, among others fields.",
                     " Markov chain Monte Carlo methods create samples from a continuous random variable,",
                     " with probability density proportional to a known function. These samples can be used to",
                     " evaluate an integral over that variable, as its expected value or variance.",
                     " Various algorithms exist for constructing chains, including:")),
            HTML("<li>Metropolis-Hashting</li><li>Gibbs</li><li>Hamiltonian</li>"),
            br(),
            p(paste0("The purpose of this Shiny App is to visualize how different samplers works",
                     " in an interactive way to help to grasp the idea behind each method.")),
            selectInput(
                inputId = "method",
                label = "Sampler Method",
                choices = c("Select method...", "Metropolis", "Gibbs")
            ),
            div(style="color: darkgray; width: 335px; align: center; margin-left: -5px;",
                a(fa("github", fill = "darkgray", height = "2em"), href = "https://github.com/AFEScalante"),
                a(fa("twitter", fill = "darkgray", height = "2em"), href = "https://twitter.com/angelfscal")
            ),
            width = 4
        ),
        mainPanel(
            conditionalPanel(condition = "input.method == 'Metropolis'", offset = 4,
                             br(),
                             h2("Metropolis sampler"),
                             setSliderColor(c("#2a9d8f", "#2a9d8f"), c(1, 2)),
                             column(width = 3,
                                    selectInput(inputId = "metro_func", label = "Objective function",
                                                choices = c("Exponential", "Normal"))
                             ),
                             column(width = 4,
                                    sliderInput(inputId = "metro_samples", label = "Number of samples",
                                                min = 10, max = 3000, value = 500)
                             ),
                             column(width = 4,
                                    sliderInput(inputId = "metro_step_sd", label = "SD proposal",
                                                min = 0.1, max = 10, value = 1)
                             ),
                             fluidRow(
                                 column(width = 1),
                                 column(width = 8,
                                        withMathJax(),
                                        uiOutput('math_expr'),
                                        actionButton(inputId = "simulate", "Simulate!")
                                 )
                             ),
                             fluidRow(
                                 br(),
                                 echarts4rOutput(outputId = "sims_metro_hist", height = '250%'),
                                 echarts4rOutput(outputId = "sims_metro_plot", height = '250%')
                             )
            ),
            conditionalPanel(condition = "input.method == 'Gibbs'",
                             br(),
                             h2("Gibbs Sampler")),
            width = 8
        )
    )
)

# Server function
server <- function(input, output) {

    # Math expressions
    output$math_expr <- renderUI({
        if (input$metro_func == "Exponential") {
            withMathJax(helpText(paste0('Objective math expression: ',
                                        '$$X \\sim \\Gamma(\\lambda = 1), \\quad ',
                                        'f(x)=\\lambda e^{-\\lambda x}$$')))
        } else if(input$metro_func == "Normal") {
            withMathJax(helpText(paste0('Objective math expression:  ',
                                       '$$X \\sim \\mathcal{N}(\\mu = 1, \\sigma = 0), \\quad ',
                                       'f(x)=\\dfrac{1}{\\sigma \\sqrt{2\\pi}}e^{ -\\frac{1}{2} (\\dfrac{x - \\mu}{\\sigma})^{2}}$$')))
        }
    })

    data_sim <- eventReactive(input$simulate, {
        metro_func <-
            switch (input$metro_func,
                    "Exponential" = log_exp_target,
                    "Normal" = log_normal_target
            )

        tibble(
            n_iter = 1:input$metro_samples,
            sims = metropolis_sampler(
                log_target = metro_func,
                n_iter = input$metro_samples,
                startval = 1,
                proposalsd = input$metro_step_sd
            )
        )
    })

    output$sims_metro_plot <- renderEcharts4r({
        data_sim() |>
            e_charts(x = n_iter) |>
            e_color("#14213d", background = "#e5e5e5") |>
            e_tooltip() |>
            e_x_axis(n_iter) |>
            e_line(sims)
    })

    output$sims_metro_hist <- renderEcharts4r({
        data_sim() |>
            e_charts() |>
            e_histogram(sims, name = "histogram") |>
            e_density(sims,
                      areaStyle = list(opacity = .4),
                      smooth = TRUE,
                      name = "density",
                      y_index = 1) |>
            e_color(c("#14213d", "#2a9d8f"), background = "#e5e5e5") |>
            e_tooltip(trigger = "axis")
    })

}

# Run the application
shinyApp(ui = ui, server = server)
