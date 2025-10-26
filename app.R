
#R shiny
library(shiny)
library(tidyverse)
library(caret)
library(pROC)
library(MASS)     # lda/qda
library(e1071)    # naiveBayes
library(DT)

diab <- read.csv("diabetes.csv")
cost <- read.csv("medical_cost.csv")

# Basic prep
if ("Outcome" %in% names(diab)) diab$Outcome <- factor(diab$Outcome, levels = c(0,1))
num_cols_diab <- names(diab)[sapply(diab, is.numeric)]
pred_cols_diab <- setdiff(names(diab), c("Outcome"))

charges_col <- if ("charges" %in% names(cost)) "charges" else names(cost)[1]  # fallback
pred_cols_cost <- setdiff(names(cost), charges_col)

ui <- navbarPage(
  "Diabetes & Medical Cost Explorer",
  tabPanel("Diabetes (Classification)",
           sidebarLayout(
             sidebarPanel(
               selectInput("clf", "Classifier",
                           c("Logistic" = "logit", "LDA" = "lda",
                             "QDA" = "qda", "Naive Bayes" = "nb")),
               sliderInput("split", "Train fraction", min = 0.5, max = 0.9, value = 0.7, step = 0.05),
               sliderInput("thr", "Decision threshold (for Logistic/NB probs)", min = 0.05, max = 0.95, value = 0.5, step = 0.05),
               checkboxGroupInput("feat_diab", "Features", choices = pred_cols_diab, selected = pred_cols_diab),
               numericInput("seed", "Seed (for split)", value = 527)
             ),
             mainPanel(
               h4("Confusion Matrix & Metrics"),
               verbatimTextOutput("cm"),
               h4("ROC Curve & AUC"),
               plotOutput("rocplot"),
               verbatimTextOutput("auc")
             )
           )
  ),
  tabPanel("Medical Cost (Regression)",
           sidebarLayout(
             sidebarPanel(
               sliderInput("split2", "Train fraction", min = 0.5, max = 0.9, value = 0.7, step = 0.05),
               checkboxGroupInput("feat_cost", "Features", choices = pred_cols_cost, selected = pred_cols_cost),
               numericInput("seed2", "Seed (for split)", value = 527)
             ),
             mainPanel(
               h4("Model Summary"),
               verbatimTextOutput("lmcoef"),
               h4("Hold-out Performance"),
               verbatimTextOutput("regperf"),
               DTOutput("lmhead")
             )
           )
  )
)

server <- function(input, output, session) {
  
  # ----- Diabetes: train/test split + model -----
  diab_split <- reactive({
    req(input$feat_diab, "Outcome" %in% names(diab))
    set.seed(input$seed)
    idx <- createDataPartition(diab$Outcome, p = input$split, list = FALSE)
    list(train = diab[idx, c("Outcome", input$feat_diab), drop = FALSE],
         test  = diab[-idx, c("Outcome", input$feat_diab), drop = FALSE])
  })
  
  diab_fit <- reactive({
    d <- diab_split()
    form <- as.formula(paste("Outcome ~", paste(input$feat_diab, collapse = " + ")))
    switch(input$clf,
           logit = glm(form, data = d$train, family = binomial()),
           lda   = lda(form, data = d$train),
           qda   = qda(form, data = d$train),
           nb    = naiveBayes(form, data = d$train)
    )
  })
  
  diab_pred <- reactive({
    d <- diab_split(); fit <- diab_fit()
    if (input$clf == "logit") {
      pr <- predict(fit, newdata = d$test, type = "response")
      cls <- factor(ifelse(pr >= input$thr, 1, 0), levels = c(0,1))
      list(prob = pr, cls = cls, ref = d$test$Outcome)
    } else if (input$clf %in% c("lda","qda")) {
      pr <- predict(fit, newdata = d$test)
      # LDA/QDA give posterior probs and class labels directly
      prob1 <- pr$posterior[, which(colnames(pr$posterior) %in% c("1","Yes","Positive"))[1]]
      cls <- factor(pr$class, levels = levels(d$test$Outcome))
      list(prob = prob1, cls = cls, ref = d$test$Outcome)
    } else { # nb
      pr <- predict(fit, newdata = d$test, type = "raw")
      # ensure column for positive class exists
      pos_col <- which(colnames(pr) %in% c("1","Yes","Positive"))[1]
      prob1 <- pr[, pos_col]
      cls <- factor(ifelse(prob1 >= input$thr, 1, 0), levels = c(0,1))
      list(prob = prob1, cls = cls, ref = d$test$Outcome)
    }
  })
  
  output$cm <- renderPrint({
    p <- diab_pred()
    confusionMatrix(p$cls, p$ref, positive = "1")
  })
  
  output$rocplot <- renderPlot({
    p <- diab_pred()
    req(!is.null(p$prob))
    rocobj <- roc(response = p$ref, predictor = as.numeric(p$prob))
    plot.roc(rocobj, legacy.axes = TRUE, print.thres = TRUE)
  })
  
  output$auc <- renderPrint({
    p <- diab_pred()
    req(!is.null(p$prob))
    rocobj <- roc(response = p$ref, predictor = as.numeric(p$prob))
    cat("AUC:", round(auc(rocobj), 3))
  })
  
  # ----- Medical cost: simple lm on selected features -----
  cost_split <- reactive({
    req(input$feat_cost)
    set.seed(input$seed2)
    idx <- createDataPartition(cost[[charges_col]], p = input$split2, list = FALSE)
    list(train = cost[idx, c(charges_col, input$feat_cost), drop = FALSE],
         test  = cost[-idx, c(charges_col, input$feat_cost), drop = FALSE])
  })
  
  cost_fit <- reactive({
    d <- cost_split()
    form <- as.formula(paste(charges_col, "~", paste(input$feat_cost, collapse = " + ")))
    lm(form, data = d$train)
  })
  
  output$lmcoef <- renderPrint({
    fit <- cost_fit()
    print(coef(summary(fit)))
  })
  
  output$regperf <- renderPrint({
    d <- cost_split(); fit <- cost_fit()
    pred <- predict(fit, newdata = d$test)
    y <- d$test[[charges_col]]
    rmse <- sqrt(mean((y - pred)^2))
    r2   <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)
    cat("RMSE:", round(rmse, 2), "\nR²:", round(r2, 3))
  })
  
  output$lmhead <- renderDT({
    datatable(head(cost, 10))
  })
}

shinyApp(ui, server)
