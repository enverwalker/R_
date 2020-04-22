#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

nameWidth = 3
inputWidth = 9

makeSlider = function(id, min, max, value) {
    sliderInput(
        inputId = id,
        label = NULL,
        min = min,
        max = max,
        value = value
    )
}

makeCountRow = function(id, min, max, value) {
    fluidRow(
        column(3, h5("Количество")),
        column(9, makeSlider(id, min, max, value))
    )
}

makeCovMatrixRow = function(idIn, min, max, values) {
    fluidRow(
        fluidRow(
            column(4, h5("Ков. м-ца X:")),
            column(8, makeSlider(paste0(idIn, "X"), min, max, values[1]))
        ),
        fluidRow(
            column(4, h5("Ков. м-ца Y:")),
            column(8, makeSlider(paste0(idIn, "Y"), min, max, values[2]))
        )
    )
}

makeExpectedRow = function(idIn, min, max, values) {
    fluidRow(
        fluidRow(
            column(4, h5("Mu X:")),
            column(8, makeSlider(paste0(idIn, "X"), min, max, values[1]))
        ),
        fluidRow(
            column(4, h5("Mu Y:")),
            column(8, makeSlider(paste0(idIn, "Y"), min, max, values[2]))
        )
    )
}
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6, checkboxInput("drawAda", "Адаптивный линейный элемент", TRUE)),
                column(6, checkboxInput("drawHeb", "Правило Хэбба", TRUE))
            ),
            fluidRow(
                column(6, checkboxInput("iterAda", "Промежуточные линии")),
                column(6, checkboxInput("iterHeb", "Промежуточные линии"))
            ),
            fluidRow(
                column(6, h3("Класс 1", style = "color: gold; text-align: left")),
                column(6, makeSlider("n1", 10, 1000, 100))
            ),
            makeCovMatrixRow("cov1In", 1, 20, c(2, 3)),
            makeExpectedRow("mu1In", -10, 10, c(0, 0)),
            fluidRow(
                column(6, h3("Класс 2", style = "color: blue; text-align: left")),
                column(6, makeSlider("n2", 10, 1000, 100))
            ),
            makeCovMatrixRow("cov2In", 1, 20, c(2, 3)),
            makeExpectedRow("mu2In", -10, 10, c(10, 0))
            
        ),
        
        mainPanel(
            
            plotOutput(outputId = "plot", height = "600px"),
            
            fluidRow(
                column(2, h4("Входные:")),
                column(2,tableOutput("imu1")),
                column(2,tableOutput("imu2")),
                column(3,tableOutput("icov1")),
                column(2,tableOutput("icov2"))
            )
        )
    )
    
)


library(MASS)

normalize = function(xl) {
    for (i in 1:dim(xl)[2]) {
        xl[, i] = (xl[, i] - min(xl[, i])) / (max(xl[, i]) - min(xl[, i]))
    }
    return(xl)
}

ada_L = function(m) (1 - m) ^ 2

heb_L = function(m) max(-m, 0)

ada_upd = function(w, eta, xi, yi) w - eta * (sum(w * xi) - yi) * xi

heb_upd = function(w, eta, xi, yi) w + eta * yi * xi


server = function(input, output) {
    generateData = function() {
        # nomber of instances
        n1 = input$n1
        n2 = input$n2
        
        covar1 = matrix(c(input$cov1InX, 0, 0, input$cov1InY), 2, 2)
        covar2 = matrix(c(input$cov2InX, 0, 0, input$cov2InY), 2, 2)
        
        mu1 = c(input$mu1InX, input$mu1InY)
        mu2 = c(input$mu2InX, input$mu2InY)
        ind <- round(cbind(mu1,mu2,covar1[,1],covar1[,2],covar2[,1],covar2[,2]),1)
        imu1 <- matrix(mu1,2,1)
        imu2 <- matrix(mu2,2,1)
        icov1 <- cbind(covar1[,1],covar1[,2])
        icov2 <- cbind(covar2[,1],covar2[,2])
        colnames(imu1) <- c("mu1")
        colnames(imu2) <- c("mu2")
        colnames(icov1) <- c("Sigma","1")
        colnames(icov2) <- c("Sigma","2")
        output$imu1 <- renderTable(imu1, digits = 0
                                   ,spacing="xs",bordered = TRUE)
        output$imu2 <- renderTable(imu2, digits = 0
                                   ,spacing="xs",bordered = TRUE)
        output$icov1 <- renderTable(icov1, digits = 0
                                    ,spacing="xs",bordered = TRUE)
        output$icov2 <- renderTable(icov2, digits = 0
                                    ,spacing="xs",bordered = TRUE)
        
        xy1 = mvrnorm(n1, mu1, covar1)
        xy2 = mvrnorm(n2, mu2, covar2)
        
        classes = c(rep(-1, n1), rep(1, n2))
        normdata = normalize(rbind(xy1,xy2))
        normdata = cbind(normdata,classes)
        colnames(normdata) = c("x","y","class")
        return(normdata)
    }
    drawLine = function(w, color, lwd=2) {
        x = y = seq(-2, 2, len = 100)
        z = outer(x, y, function(x,y) w[1]*x + w[2]*y + w[3])
        contour(x, y, z, levels = FALSE, drawlabels = FALSE, lwd = lwd, col = color,
                add = TRUE)
    }
    
    # Стохастический градиент
    sgd = function(xl, classes, L, updateRule, drawIters, eps=1e-5, eta=1/6) {
        rows = dim(xl)[1]
        xl = cbind(xl,seq(from=1,to=1,length.out=rows))
        cols = dim(xl)[2]
        w = runif(cols, -1 / (2 * cols), 1 / (2 * cols))
        lambda = 1 / rows
        
        Q = 0
        for (i in 1:rows) Q = Q + L(sum(w * xl[i,]) * classes[i])
        Q0 = Q
        
        iter = 0
        repeat {
            iter = iter + 1
            
            margins = rep(0, rows)
            for (i in 1:rows) {
                xi = xl[i,]
                yi = classes[i]
                margins[i] = sum(w * xi) * yi
            }
            errors = which(margins <= 0)
            
            if (length(errors) == 0) {
                break;
            }
            
            rnd_err = sample(errors, 1)
            xi = xl[rnd_err,]
            yi = classes[rnd_err]
            
            margin = sum(w * xi) * yi
            error = L(margin)
            
            w = updateRule(w, eta, xi, yi)
            
            Q = (1 - lambda) * Q + lambda * error
            if (abs(Q0 - Q) / abs(max(Q0, Q)) < eps) break;
            Q0 = Q
            if (iter == 30000)  break;
            if(drawIters) drawLine(w, "black", 1)
        }
        
        return(w)
    }
    drawPoints = function(x) {
        colors = c("gold", "red", "blue")
        for(i in 1:dim(x)[1]) x[i,3] = x[i,3] + 2
        lab = "ADALINE и правило Хэбба"
        plot(x[, 1], x[, 2], pch = 21, col = "darkred", bg = colors[x[,3]],
             main = lab, asp = 1, xlab = "X", ylab = "Y")
    }
    output$plot = renderPlot({
        data = generateData()
        drawPoints(data)
        ada = sgd(data[,1:2], data[,3], ada_L, ada_upd, input$iterAda)
        heb = sgd(data[,1:2], data[,3], heb_L, heb_upd, input$iterHeb)
        if(input$drawAda) drawLine(ada, "darkred", 4)
        if(input$drawHeb) drawLine(heb, "darkgreen", 4)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
