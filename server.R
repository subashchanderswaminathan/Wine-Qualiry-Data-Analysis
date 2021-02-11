#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(recipes)
library(caret)
library(plotly)
library(ggplot2)


wineQualityReds <- read_csv("wineQualityReds.csv")

wineQualityReds$X1 <- NULL
wineQualityReds$quality = as.factor(wineQualityReds$quality)


df <- as.data.frame(wineQualityReds) 
df1 <- df
df2 <- df

set.seed(1234)


partition_data <- createDataPartition(df$quality, p=0.7, list = FALSE)

train_df <- df[partition_data,]
test_df <- df[-partition_data,]


# Density plot YJ transformation
yj_estimates <- recipe(quality ~. , data = train_df) %>%
  step_YeoJohnson(all_numeric()) %>%
  prep(data = train_df) 

yj_estimates$steps[[1]]$lambdas

yj_te <- bake(yj_estimates, test_df)


# Box-Plot YJ Transforamtion

#Boxplot Before and After:

check_numeric1 <- unlist(lapply(test_df, is.numeric)) 
scaling <- scale(test_df[,check_numeric1, drop = FALSE], center = TRUE, scale = TRUE)
boxplotfin <- tidyr::gather(as.data.frame(scaling))



check_numeric2 <- unlist(lapply(yj_te, is.numeric)) 
ascaling <- scale(yj_te[,check_numeric2, drop = FALSE], center = TRUE, scale = TRUE)
aboxplotfin <- tidyr::gather(as.data.frame(ascaling))


outlier1 = TRUE
outlier2 = TRUE




# Distance based outlier detection:

recipe1 <- recipe(quality ~. , data = train_df) %>%
  step_naomit(everything()) %>% # remove obs that have missing values or impute as shown in next line
  step_knnimpute(everything(), neighbors = 5) %>% # use knn imputation 
  step_rm(all_nominal()) %>%  #remove nominals if any present 
  step_nzv(all_predictors()) %>%  # remove near zero variance predictor variables
  step_lincomb(all_predictors()) %>%   # remove predictors that are linear combinations of other predictors
  #step_YeoJohnson(all_predictors()) %>% # transform all remaining predictors
  prep(data = train_df)

processed1 <- bake(recipe1, test_df)
processed1

recipe2 <- recipe(quality ~. , data = train_df) %>%
  step_naomit(everything()) %>% # remove obs that have missing values or impute as shown in next line
  step_knnimpute(everything(), neighbors = 5) %>% # use knn imputation 
  step_rm(all_nominal()) %>%  #remove nominals if any present 
  step_nzv(all_predictors()) %>%  # remove near zero variance predictor variables
  step_lincomb(all_predictors()) %>%   # remove predictors that are linear combinations of other predictors
  step_YeoJohnson(all_predictors()) %>% # transform all remaining predictors
  prep(data = train_df)

processed2 <- bake(recipe2, test_df)
processed2

recipe3 <- recipe(alcohol ~. , data = train_df) %>%
  step_naomit(everything()) %>% # remove obs that have missing values or impute as shown in next line
  step_knnimpute(everything(), neighbors = 5) %>% # use knn imputation 
  step_rm(all_nominal()) %>%  #remove nominals if any present 
  step_nzv(all_predictors()) %>%  # remove near zero variance predictor variables
  step_lincomb(all_predictors()) %>%   # remove predictors that are linear combinations of other predictors
  #step_YeoJohnson(all_predictors()) %>% # transform all remaining predictors
  prep(data = train_df)

processed3 <- bake(recipe3, test_df)
processed3


recipe4 <- recipe(alcohol ~. , data = train_df) %>%
  step_naomit(everything()) %>% # remove obs that have missing values or impute as shown in next line
  step_knnimpute(everything(), neighbors = 5) %>% # use knn imputation 
  step_rm(all_nominal()) %>% #remove nominals if any present 
  step_nzv(all_predictors()) %>%  # remove near zero variance predictor variables
  step_lincomb(all_predictors()) %>%   # remove predictors that are linear combinations of other predictors
  step_YeoJohnson(all_predictors()) %>% # transform all remaining predictors
  prep(data = train_df)

processed4 <- bake(recipe4, test_df)
processed4



# Mahalanobis Distance Before and After YJ transform:

varianceMat1 <- var(processed1) # calculate the covariance matrix
colMean1 <- colMeans(processed1) # calculate variable means 
mhd1 <- mahalanobis(x = processed1, center = colMean1, cov = varianceMat1)

threshold1 <- qchisq(p = 0.999, df = ncol(processed1))  # calculate a 99.9% threshold



varianceMat2 <- var(processed2) # calculate the covariance matrix
colMean2 <- colMeans(processed2) # calculate variable means 
mhd2 <- mahalanobis(x = processed2, center = colMean2, cov = varianceMat2)

threshold2 <- qchisq(p = 0.999, df = ncol(processed2))  # calculate a 99.9% threshold


# Cook's Distance Before and After YJ transform:

lmodel1 <- glm(formula = alcohol ~ ., data = processed3, family = gaussian)
dc1 <- cooks.distance(lmodel1)
thresh1 <- 4 * mean(dc1)  # this is an empirical way to assign a threshold
dfcd1 <- data.frame(dc1, id1 = 1:length(dc1)/length(dc1) )



lmodel2 <- glm(formula = alcohol ~ ., data = processed4, family = gaussian)
dc2 <- cooks.distance(lmodel2)
thresh2 <- 4 * mean(dc2)  # this is an empirical way to assign a threshold
dfcd2 <- data.frame(dc2, id2 = 1:length(dc2)/length(dc2) )

# DBSCAN Before and After YJ transform:

rec5 <- recipe(fixed.acidity ~. , data = train_df) %>%
  step_naomit(everything()) %>% # remove obs that have missing values or impute as shown in next line
  step_knnimpute(everything(), neighbors = 5) %>% # use knn imputation 
  step_rm(all_nominal()) %>%  #remove nominals if any present 
  step_nzv(all_predictors()) %>%  # remove near zero variance predictor variables
  step_lincomb(all_predictors()) %>%   # remove predictors that are linear combinations of other predictors
  #step_YeoJohnson(all_predictors()) %>% # transform all remaining predictors
  prep(data = train_df)

processed5 <- bake(rec5, test_df)
processed5


rec6 <- recipe(fixed.acidity ~. , data = train_df) %>%
  step_naomit(everything()) %>% # remove obs that have missing values or impute as shown in next line
  step_knnimpute(everything(), neighbors = 5) %>% # use knn imputation 
  step_rm(all_nominal()) %>% #remove nominals if any present 
  step_nzv(all_predictors()) %>%  # remove near zero variance predictor variables
  step_lincomb(all_predictors()) %>%   # remove predictors that are linear combinations of other predictors
  step_YeoJohnson(all_predictors()) %>% # transform all remaining predictors
  prep(data = train_df)

processed6 <- bake(rec6, test_df)
processed6


server <- shinyServer(function(input, output, session) {
  
  output$dropdown <- renderDropdownMenu({
    dropdownMenu(messageItem("Assignment 2", "Outlier", icon = "info"),
                 messageItem("Student Name", "Subash Chander Swaminathan", color = "teal", style = "min-width: 370px"),
                 messageItem("Student ID", "69873432", color = "teal", icon = "portrait"))
    
  })
  
  output$Data <- renderDataTable(df)
  
  output$sum <- renderPrint({
    summary(df)
  })
  
  output$Structure <- renderPrint({
    glimpse(df)
  })  
  
  output$fixed_acidity <- renderPlot({
    plot(density(test_df$fixed.acidity),main = "Fixed Acidity")
  })  
  output$Afixed_acidity <- renderPlot({
    plot(density(yj_te$fixed.acidity),main = "Fixed Acidity")
  })   
  output$volatile_acidity <- renderPlot({
    plot(density(test_df$volatile.acidity),main = "volatile Acidity")
  })  
  output$Avolatile_acidity <- renderPlot({
    plot(density(yj_te$volatile.acidity),main = "volatile Acidity")
  })  
  
  output$citric_acid <- renderPlot({
    plot(density(test_df$citric.acid),main = "Citric Acid")
  })  
  output$Acitric_acid <- renderPlot({
    plot(density(yj_te$citric.acid),main = "Citric Acid")
  })  
  
  output$residual_sugar <- renderPlot({
    plot(density(test_df$residual.sugar),main = "Residual Sugar")
  })  
  output$Aresidual_sugar <- renderPlot({
    plot(density(yj_te$residual.sugar),main = "Residual Sugar")
  })  
  
  output$chlorides <- renderPlot({
    plot(density(test_df$chlorides),main = "Chlorides")
  })  
  output$Achlorides <- renderPlot({
    plot(density(yj_te$chlorides),main = "Chlorides")
  })  
  
  output$free_sulfur_dioxide <- renderPlot({
    plot(density(test_df$free.sulfur.dioxide),main = "Free Sulfur Dioxide")
  })  
  output$Afree_sulfur_dioxide <- renderPlot({
    plot(density(yj_te$free.sulfur.dioxide),main = "Free Sulfur Dioxide")
  })  
  
  output$total_sulfur_dioxide <- renderPlot({
    plot(density(test_df$total.sulfur.dioxide),main = "Total Sulfur Dioxide")
  })  
  output$Atotal_sulfur_dioxide <- renderPlot({
    plot(density(yj_te$total.sulfur.dioxide),main = "Total Sulfur Dioxide")
  })  
  
  output$density <- renderPlot({
    plot(density(test_df$density),main = "Density")
  })  
  output$Adensity <- renderPlot({
    plot(density(yj_te$density),main = "Density")
  })  
  output$pH <- renderPlot({
    plot(density(test_df$pH),main = "pH")
  })  
  output$ApH <- renderPlot({
    plot(density(yj_te$pH),main = "pH")
  }) 
  
  output$sulphates <- renderPlot({
    plot(density(test_df$sulphates),main = "Sulphates")
  })  
  output$Asulphates <- renderPlot({
    plot(density(yj_te$sulphates),main = "Sulphates")
  })
  
  output$boxplot <- renderPlot({
    if (input$outliers1 == FALSE)
    {
      outlier1 = NA
    }
    else
    {
      outlier1 = TRUE
    }
    ggplot(mapping = aes(x = boxplotfin$key, y = boxplotfin$value, fill = boxplotfin$key)) +
      geom_boxplot(coef = input$range1, outlier.colour = "red",outlier.shape = outlier1) +
      labs(title = paste("Raw Uni-variable boxplots at IQR multiplier of", 1.5),
           x = "Standardised variable value", y = "Std Value") +
      coord_flip()
  })
  
  
  output$Aboxplot <- renderPlot({
    if (input$outliers2 == FALSE)
    {
      outlier2 = NA
    }
    else
    {
      outlier2 = TRUE
    }
    ggplot(mapping = aes(x = aboxplotfin$key, y = aboxplotfin$value, fill = aboxplotfin$key)) +
      geom_boxplot(coef = input$range2, outlier.colour = "red",outlier.shape = outlier2) +
      labs(title = paste("YJ transformed Uni-variable boxplots at IQR multiplier of", 1.5),
           x = "Standardised variable value", y = "Std Value") +
      coord_flip()
  })
  
  
  output$mah <- renderPlot({
    
    ggplot(mapping = aes(y = mhd1, x = (1:length(mhd1))/length(mhd1))) +
      geom_point() +
      scale_y_continuous(limits = c(0, max(mhd1)*1.1)) +
      labs(y = "Mahalanobis distance squared", x = "Complete Observations", title = "Outlier pattern") +
      geom_abline(slope = 0, intercept = threshold1, color = "red") +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      theme(legend.position = "bottom")
    
  })
  
  output$Amah <- renderPlot({
    
    ggplot(mapping = aes(y = mhd2, x = (1:length(mhd2))/length(mhd2))) +
      geom_point() +
      scale_y_continuous(limits = c(0, max(mhd2)*1.1)) +
      labs(y = "Mahalanobis distance squared", x = "Complete Observations", title = "Outlier pattern") +
      geom_abline(slope = 0, intercept = threshold2, color = "red") +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      theme(legend.position = "bottom")
    
  })
  
  output$cook <- renderPlot({
    
    ggplot(data = dfcd1, mapping = aes(y = dc1, x = id1)) +
      geom_point() +
      scale_y_continuous(limits = c(0, max(dfcd1$dc1)*1.1)) +
      labs(y = "Cook's distance", x = "Complete Observations", title = "Outlier pattern") +
      geom_abline(slope = 0, intercept = thresh1, color = "red") +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      theme(legend.position = "bottom")
    
  })
  
  output$Acook <- renderPlot({
    
    ggplot(data = dfcd2, mapping = aes(y = dc2, x = id2)) +
      geom_point() +
      scale_y_continuous(limits = c(0, max(dfcd2$dc2)*1.1)) +
      labs(y = "Cook's distance", x = "Complete Observations", title = "Outlier pattern") +
      geom_abline(slope = 0, intercept = thresh2, color = "red") +
      scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0%", "50%", "100%")) +
      theme(legend.position = "bottom")
    
  })
  
  output$DBscan <- renderPlot({
    
    dbscan::kNNdistplot(processed5, k = 1)
    abline(h = input$scan, lty = 2)
  })
  
  output$ADBscan <- renderPlot({
    
    dbscan::kNNdistplot(processed6, k = 1)
    abline(h = input$Ascan, lty = 2)
  })
  
  output$lof <- DT::renderDataTable({
    
    dat_m <- as.matrix(df1[, 1:11])
    dis <- dbscan::lof(dat_m, k = 4)
    df1$distance <- dis
    df1 <- df1[order(dis, decreasing = TRUE),]
    df1_lof <-df1[df1$distance > 2,]
    DT::datatable(data = as.data.frame(df1_lof))
    
  })
  
  output$SVM <- DT::renderDataTable({
    
    dat_mat <- as.matrix(df2[,1:11])
    model <- e1071::svm(dat_mat, y = NULL, type ="one-classification", nu = 0.10, scale = TRUE, kernel = "radial")
    good <- predict(model,dat_mat)
    notgood <- df[!good,]
    DT::datatable(data = as.data.frame(notgood))
  })

})

