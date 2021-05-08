library(shiny)
library(GGally)
library(lattice)
library(plot3D)
source("get_data.R")
library(ggplot2)
library(rlang)
library(rgrUtils)
library(psd)
library(OceanView)
library(plot3D)
library(misc3d)
library(corrplot)

#library(rglUtils)
#par(mar=c(1,1,1,1))
# Define UI for application that draws a histogram

visualizationMethods = c("Preview",
                         "Exploratory: Pairwise Plots (2D per pair) w Corr Matrix",
                         "Exploratory: Corr Plot w Corr Matrix",#Classified/ non-classified"Exploratory: Summary by Columns Hist&Density(1D for each Col w Summary)",
                         "Exploratory: Summary by Columns Box Whisker",
                         "Exploratory: Summary by Columns Hist Density",
                         "ParallelCoordinate/Andrew\'s Plot w table",#Classified/ non-classified
                        "1D Summary w Pivot Tables",#Classified/ non-classified
                        "2D (w Model(s))",#Classified/ non-classified
                        "3D Plot (w model(s))",#Classified/ non-classified
                         "3D Pairwise Plots",
                         "1D_Comparision w. AOV")
ui <- fluidPage(

    
    
    titlePanel("Multi-var Dataset Previewer"),
    plotOutput("Plot",width = "100%",
               height = "500px"),

    
    fluidRow(
        column(2,
            selectInput("dataSet","Select Dataset",
                        choices = get_data_names(),selected = "mtcars"),
            selectInput("method",
                        "Select Visualization Method",
                        choices = visualizationMethods),
            selectInput("nonVarMethod",
                        "Method to Treat Non-Variables",
                        choices = c("Ignore","Keep")),
            selectInput("NAMethod",
                        "Method to Treat Missing Values",
                        choices = c("Ignore","Keep","Fill With AVG")),
            ),
        
        column(2,
            textInput("cont1","Continuous Variable 1"),
            checkboxInput("considerCont1","Consider the effect of this variable", value= FALSE),
            textInput("cont2","Continuous Variable 2"),
            checkboxInput("considerCont2","Consider the effect of this variable", value= FALSE),
            textInput("cont3","Continuous Variable 3"),
            checkboxInput("considerCont3","Consider the effect of this variable", value= FALSE),
        ),
        column(2,
            textInput("disc1","Discrete Variable 1"),
            checkboxInput("considerDisc1","Consider the effect of this variable", value= FALSE),
            textInput("disc2","Discrete Variable 2"),
            checkboxInput("considerDisc2","Consider the effect of this variable", value= FALSE),
            textInput("disc3","Discrete Variable 3"),
            checkboxInput("considerDisc3","Consider the effect of this variable", value= FALSE),
            textInput("disc4","Discrete Variable 4"),
            checkboxInput("considerDisc3","Consider the effect of this variable", value= FALSE),
            textInput("disc5","Discrete Variable 5"),
            checkboxInput("considerDisc3","Consider the effect of this variable", value= FALSE),
        ),
        column(2,
            #Predictive Models
            textInput("target","Target Variable"),
            selectInput("model","Predictive Models towards target var",
                        choices = c("noModel","linear","loess")),
            #Predictor Selections
            checkboxInput("useAllVarsAsPredictors","Use All Vars As Predictors", value = FALSE),
        ),
        column(2,
               )
    ),
    
    
    tabsetPanel(type = "tabs",
                
                tabPanel("Variable Summary",fluidRow(
                    column(5,verbatimTextOutput("Summary")),
                    column(5,verbatimTextOutput("Overview")))
                ),
                tabPanel("Data and Model Data", tableOutput('data')),
                tabPanel("Considerations", tableOutput('considerations')),
                tabPanel("Code", tableOutput('code'))
    ),
    

)


server <- function(input, output) {
    reactiveDataFrame <- reactive({
        getAnywhere(input$dataSet)$objs[[1]]
    })
    
    reactiveMethod <- reactive({
        input$method
    })
    

    
    output$Summary <- renderPrint({
        summary(reactiveDataFrame())
        })
    output$Overview <- renderPrint({
        print("----------------Type of variable------------------------")
        dataSet = reactiveDataFrame()    
        
        vartype = 1:ncol(dataSet)
        for(i in 1:ncol(dataSet)){
            vartype[i] = as.character(class(dataSet[,i]))
        }
        #for(i in 1:ncol(dataSet)) print(paste(colnames(dataSet[i]),vartype[i],sep = " is "))
        n_cat = 0
        i_cat = c();i_num=c()
        for(i in 1:ncol(dataSet)){
            if(vartype[i]=="character" | vartype[i] == "factor"){
                n_cat = n_cat+1
                i_cat =  c(i_cat,i)
            }else{
                i_num = c(i_num,i)
            }
        }
        n_num =as.integer(ncol(dataSet)-n_cat)
        print(paste("There are",n_num,"numerical variables:"))
        print(paste(colnames(dataSet)[i_num],collapse = '||'))
        print(paste("; and ",n_cat,"categorical variables."))
        print(paste(colnames(dataSet)[i_cat],collapse = '||'))
        print("----------------Data Structure------------------------")
        print(str(dataSet))
    })
    
    output$Plot <- renderPlot({
        
        
        dataSet = reactiveDataFrame()
        
        #
        #VISUALIZATION
        #
        if(input$method == "Preview"){
            if(is.data.frame(dataSet)){
                plot(dataSet,main = input$dataSet)
            }
            else if(is.ts(dataSet)){
                pspectrum(dataSet,plot=TRUE)
            }else if(is.matrix(dataSet)){
                ugr_filled_contour.matrix(dataSet)
            #else if(is.array(dataSet) & length(dim(dataSet))==3){
            }else{    plot(dataSet)
            }
        }else{
            
            #PRE_PROCESSING
            #------------------------------------------------------
            
            is_Time_Series = is.ts(dataSet)
            
            var_names = colnames(dataSet)
            nvar = ncol(dataSet)
            vartype = character(nvar)
            
            for(i in 1:nvar) vartype[i] = as.character(class(dataSet[,i]))
            
            
            #detect NA, fill with avg or delete
            #
            if(input$NAMethod=="Ignore"){
                dataSet <- dataSet[complete.cases(dataSet),]
            }else if(input$NAMethod=="Fill With AVG"){
                for(i in 1:ncol(dataSet)){
                    if(is.numeric(dataSet[,i])) dataSet[,i][is.na(dataSet[,i])] <- mean(dataSet[,i], na.rm = TRUE)
                }
            }
            
            #
            
            #detect non_variables
            is_variable = logical(nvar)
            for(i in 1:nvar){
                is_variable[i] = TRUE
                if(all(duplicated(dataSet[,i])[-1L])) is_non_variables[i]<-FALSE
            }
            
            dataSet_non_var<-dataSet[,is_variable]
            if(input$nonVarMethod=="Ignore") dataSet = dataSet_non_var 
            
            #
            
            
            
            nvar = ncol(dataSet)#must evaluate 2 times
            
            var_names = colnames(dataSet)
            nvar = ncol(dataSet)
            vartype = character(nvar)
            # all-numeric version
            dataSet_num <- dataSet
            for(i in nvar){
                if(is.factor(dataSet_num[,i]) | is.character(dataSet_num[,i])){
                    dataSet_num[,i] = as.numeric(dataSet_num[,i])
                }
            }
            
            #consideration
            dep_cont_var = c()
            dep_disc_var = c()
            if(isTRUE(input$considerCont1)) dep_disc_var = c(dep_cont_var,input$cont1)
            if(isTRUE(input$considerCont2)) dep_disc_var = c(dep_cont_var,input$cont2)
            if(isTRUE(input$considerCont3)) dep_disc_var = c(dep_cont_var,input$cont3)
            
            
            if(isTRUE(input$considerDisc1)) dep_disc_var = c(dep_disc_var,input$disc1)
            if(isTRUE(input$considerDisc2)) dep_disc_var = c(dep_disc_var,input$disc2)
            if(isTRUE(input$considerDisc3)) dep_disc_var = c(dep_disc_var,input$disc3)
            if(isTRUE(input$considerDisc4)) dep_disc_var = c(dep_disc_var,input$disc4)
            if(isTRUE(input$considerDisc5)) dep_disc_var = c(dep_disc_var,input$disc5)
            #------------------------------------------------------
            #END OF PRE_PROCESSING
            
            
            
            
            
            
            
            #DF_VISUALIZATION
            #   
            #------------------------------------------------------
        if(input$method == "Exploratory: Summary by Columns Box Whisker"){
            rgrUtils::ugr_boxplot.data.frame(dataSet)
        }else if(input$method == "Exploratory: Summary by Columns Hist Density"){
            rgrUtils::ugr_hist.data.frame(dataSet)
        }else if(input$method == "Exploratory: Corr Plot w Corr Matrix"){
            corrplot(cor(dataSet_num),shape = "ellipse")
            #title(paste("Linear Corr Btw Continuous vars in", input$dataSet))
            
        }else if(input$method == "Exploratory: Pairwise Plots (2D per pair) w Corr Matrix"){
           if(length(dep_disc_var)==0){
               
           splom(dataSet, title = input$dataSet)
           }else{
               #IDIOTIC DESIGN OF LATTICE!!!
               eval(parse(text = paste0("splom(dataSet, groups =",input$dataSet,"$",dep_disc_var[1],",title = ",input$dataSet,")")))
           }
            
        }else if(input$method=="ParallelCoordinate/Andrew\'s Plot w table"){
            if(length(dep_disc_var)==0){
                
                parallelplot(dataSet, title = input$dataSet)
            }else{
                
                eval(parse(text = paste0("parallelplot(dataSet, groups =",input$dataSet,"$",dep_disc_var[1],",title = ",input$dataSet,")")))
            
            }
        }else if(input$method == "3D Pairwise Plots"){
            message("Calculating Pairwise 3D Plots, may take long")
            n_permutations = nvar*(nvar-1)*(nvar-2)
            n_rc =ceiling(sqrt(n_permutations))
            layout(matrix(1:(n_rc^2),n_rc,n_rc))
            for(i in 1:(nvar-3)){
                for (j in (i+1):(nvar-1)){
                    for (k in (j+1):(nvar)){

                        plot3D::scatter3D(dataSet_num[,i],dataSet_num[,j],
                                          dataSet_num[,k])
                    }
                }
            }
            if(n_permutations!=(n_rc^2)){
                for(i in (n_permutations+1):(n_rc^2)) plot(0:1,0:1,type = "n")
            }
            layout(1)
            }
        
            
            
            
            
        }
        #------------------------------------------------------
        #END OF DF_VISUALIZATION
    })
    
    output$modelInfo = renderText({
        
    })
    
    output$modelParameters = renderText({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
