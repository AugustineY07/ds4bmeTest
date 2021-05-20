library(shiny)
library(tidyverse)
library(LiblineaR)
library(scales)

#load data
train = read.csv('https://raw.githubusercontent.com/anujdutt9/Disease-Prediction-from-Symptoms/master/dataset/training_data.csv')
test = read.csv('https://raw.githubusercontent.com/anujdutt9/Disease-Prediction-from-Symptoms/master/dataset/test_data.csv')
train = train[,1:133]
all = rbind(train, test)
#clear symbols in column names
names(all) <- gsub("_", " ", names(all))
names(all) <- gsub("\\.", "", names(all))

x = all %>% select(-prognosis)
y = all %>% select(prognosis)
all_symptom = colnames(x)
length = length(x)

samples = sample(1:nrow(all),size = nrow(all)*0.95,replace=F)
train = all[samples,]  
train_data = train %>% select(-prognosis)
train_label = train %>% select(prognosis)
test = all[-samples,]
test_data = test %>% select(-prognosis)
test_label = test %>% select(prognosis)

#model fitting with log regression
regfit = LiblineaR(train_data, train_label, type = 0, cost = 1)
prediction = predict(regfit, test_data, proba=TRUE)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Disease Prediction with Symptoms"),
    h5('This app gives possible diagnostics with symptoms you selected.'),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("Symptom", label = h3("Select your symptoms:"), 
                               choices = c("None", colnames(train_data)),
                               selected = 'None'),
            submitButton('Submit')
        ),
        mainPanel(
                h5('Your diagnostics result is: '),
                verbatimTextOutput("disease"),
                h5('with'),
                verbatimTextOutput("probability"),
                h5('of confidence')
        )
    )
)




# Define server logic 
server <- function(input, output) {
    result = reactive({
        empty = replicate(length, 0)
        symp_index = which(input$Symptom == all_symptom) #index of chosen symptoms
        
        for(i in 1:length(symp_index)) {
            empty[symp_index[i]] = 1
        }
        
        #convert to data frame and predict
        user_symptoms = as.data.frame(t(empty))
        colnames(user_symptoms) = colnames(train_data)
        prediction_user = predict(regfit, user_symptoms, proba=TRUE)
        
        #find most probable disease and associated probability 
        disease_in = which.max(prediction_user$probabilities) #find max index
        disease = as.character(prediction_user$predictions[1]) #possible disease
        possibility = prediction_user$probabilities[disease_in]
        #set threshold for disease
        if (possibility >= 0.5) {
            prob = percent(possibility, accuracy = 0.01) 
            combine = list(d = disease, p = prob)
        }else{
            prob = percent(1-possibility, accuracy = 0.01) 
            combine = list(d = "healthy", p = prob)
        }
        return(combine) 
        })
    

    output$disease = renderPrint({
        diagnostics = result()
        diagnostics$d
    })
    output$probability = renderPrint({
        diagnostics = result()
        diagnostics$p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
