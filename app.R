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
library(ggthemes)
library(RColorBrewer)
library(factoextra)
library(psych)
library(caret)
library(randomForest)
theme_set(theme_minimal())

#scale_color_distiller(direction=-1,palette = "Spectral")

#library(rglUtils)
#par(mar=c(1,1,1,1))
# Define UI for application that draws a histogram
visualizationMethods = c("Preview",
                         "Exploratory: Pairwise Plots (2D per pair) w Corr Matrix",
                         "Exploratory: Corr Plot w Corr Matrix",#Classified/ non-classified"Exploratory: Summary by Columns Hist&Density(1D for each Col w Summary)",
                         "Exploratory: Summary by Columns Box Whisker",
                         "Exploratory: Summary by Columns Hist Density",
                         "Principle Components",
                         "ParallelCoordinate/Andrew\'s Plot w table",#Classified/ non-classified
                         "1D Summary w Pivot Tables",#Classified/ non-classified
                         "2D (w Model(s))",#Classified/ non-classified
                         "3D Plot (w model(s))",#Classified/ non-classified
                         "3D Pairwise Plots",
                         "1D_Comparision w. AOV",
                         "Model Performance",
                         "Model Insight",
                         "Grouped Model Performance",
                         "Comparing Different Models")
ui <- fluidPage(


    titlePanel("Multi-var Dataset Previewer"),
    plotOutput("Plot",width = "100%",
               height = "500px"),
    
    tabsetPanel(type = "tabs",
                
                tabPanel("Variable Summary",fluidRow(
                  column(5,verbatimTextOutput("Summary")),
                  column(5,verbatimTextOutput("Overview")))
                ),
                tabPanel("Data and Model Data", tableOutput('data')),
                tabPanel("Considerations", tableOutput('considerations')),
                tabPanel("Code", tableOutput('code'))
    ),
    fluidRow(
        column(2,
            selectInput("dataSet","Select Dataset",
                        choices = get_data_names(),selected = "mpg"),
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
        
        # column(2,
        #     textInput("cont1","Continuous Variable 1"),
        #     checkboxInput("considerCont1","Consider the effect of this variable", value= FALSE),
        #     textInput("cont2","Continuous Variable 2"),
        #     checkboxInput("considerCont2","Consider the effect of this variable", value= FALSE),
        #     textInput("cont3","Continuous Variable 3"),
        #     checkboxInput("considerCont3","Consider the effect of this variable", value= FALSE),
        # ),
        # column(2,
        #     textInput("disc1","Discrete Variable 1"),
        #     checkboxInput("considerDisc1","Consider the effect of this variable", value= FALSE),
        #     textInput("disc2","Discrete Variable 2"),
        #     checkboxInput("considerDisc2","Consider the effect of this variable", value= FALSE),
        #     textInput("disc3","Discrete Variable 3"),
        #     checkboxInput("considerDisc3","Consider the effect of this variable", value= FALSE),
        #     textInput("disc4","Discrete Variable 4"),
        #     checkboxInput("considerDisc3","Consider the effect of this variable", value= FALSE),
        #     textInput("disc5","Discrete Variable 5"),
        #     checkboxInput("considerDisc3","Consider the effect of this variable", value= FALSE),
        # ),
        column(2, uiOutput("secondSelection")),
        column(2,
            #Predictive Models
            textInput("target","Target Variable"),
            selectInput("model","Predictive Models towards target var",
                        choices = c("noModel","linear","loess",
                                    "NN","Bagging","")),
            #Predictor Selections
            checkboxInput("useAllVarsAsPredictors","Use All Vars As Predictors", value = FALSE),
        ),
        column(2,
               )
    ),
    
    
    
 
    

)


server <- function(input, output) {
    #REACT PREPARATION 
    #-----------------------------------------------------------
    
    
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
        

        #for(i in 1:ncol(dataSet)) print(paste(colnames(dataSet[i]),vartype[i],sep = " is "))
        n_cat = 0
        i_cat = logical(ncol(dataSet))

        for(i in 1:ncol(dataSet)) i_cat[i] = (is.factor(dataSet[,i,drop=TRUE]) | is.character(dataSet[,i,drop=TRUE]))
        i_num = !i_cat
        n_cat = length((1:ncol(dataSet))[i_cat])
        n_num =as.integer(ncol(dataSet)-n_cat)
        print(paste("There are",n_num,"numerical variables:"))
        print(paste(colnames(dataSet)[i_num],collapse = '||'))
        print(paste("; and ",n_cat,"categorical variables."))
        print(paste(colnames(dataSet)[i_cat],collapse = '||'))
        print("----------------Data Structure------------------------")
        print(str(dataSet))
    })
    
    reactiveVars = reactive({
        
        #consideration
        #dep_cont_var = c()
        # dep_disc_var = c()
        # if(isTRUE(input$considerCont1)) dep_disc_var = c(dep_cont_var,input$cont1)
        # if(isTRUE(input$considerCont2)) dep_disc_var = c(dep_cont_var,input$cont2)
        # if(isTRUE(input$considerCont3)) dep_disc_var = c(dep_cont_var,input$cont3)
        # 
        # 
        # if(isTRUE(input$considerDisc1)) dep_disc_var = c(dep_disc_var,input$disc1)
        # if(isTRUE(input$considerDisc2)) dep_disc_var = c(dep_disc_var,input$disc2)
        # if(isTRUE(input$considerDisc3)) dep_disc_var = c(dep_disc_var,input$disc3)
        # if(isTRUE(input$considerDisc4)) dep_disc_var = c(dep_disc_var,input$disc4)
        # if(isTRUE(input$considerDisc5)) dep_disc_var = c(dep_disc_var,input$disc5)
        # 
        # dep_var = c(dep_cont_var,dep_disc_var)
        #list(dep_cont_var,dep_disc_var,dep_var)
        
        as.vector(colnames(reactiveDataFrame()))
    })
    
    #--------------------------------------------------------------
    #KEY VARS
    #---------------------------------------------------------------
    
    output$secondSelection = renderUI({
        
        #fluidRow(
            column(10,
                   selectInput("x_var","x",choices = c("NULL",reactiveVars()),selected ="NULL"),
           # ),
           # column(2,
                   selectInput("y_var","y",choices = c("NULL",reactiveVars()),selected ="NULL"),
           # ),
           # column(2,
                   selectInput("z_var","z",choices = c("NULL",reactiveVars()),selected ="NULL"),
           # ),,
            #column(2,
                   
            selectInput("col_var","Color",choices = c("NULL",reactiveVars()),selected ="NULL"),
           # ),
           # column(2,
            selectInput("shape_var","shape_var",choices = c("NULL",reactiveVars()),selected ="NULL"),
            #),
            #column(2,
                   selectInput("group_var1","Group 1",choices = c("NULL",reactiveVars()),selected ="NULL"),
            #),
            #column(2,
                   selectInput("group_var2","Group 2",choices = c("NULL",reactiveVars()),selected ="NULL"),
            )
       # )
                #selectInput("size_var","z",choices = reactiveVars(),selected = reactiveVars[[1]][3])
        
    })
    
    #-----------------------------------------------------------
    #END OF REACT PREPARATION
    
    output$Plot <- renderPlot({
        
        
        dataSet = reactiveDataFrame()
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
        dataSet_original_num = dataSet[,i_num]# origionally numeric
        dataSet_num <- dataSet# coerced numeric
        for(i in nvar){
          if(is.factor(dataSet_num[,i]) | is.character(dataSet_num[,i])){
            dataSet_num[,i] = as.numeric(dataSet_num[,i])
          }
        }
        dataSet_original_num = Filter(is.numeric, dataSet)
        
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
        
        dep_var = c(dep_cont_var,dep_disc_var)
        
        
        x_var = input$x_var
        col_var = input$col_var
        y_var = input$y_var
        z_var = input$z_var
        shape_var = input$shape_var
        group_var1 = input$group_var1
        group_var2 = input$group_var2
        group_var =c(group_var1,group_var2)
        gg_setaes=parse(text=paste0("g=ggplot(data=dataSet,aes(",ifelse(x_var=="NULL","",paste0("x=",x_var)),",",
                                    ifelse(y_var=="NULL","",paste0("y=",y_var)),",",
                                    ifelse(col_var=="NULL","",paste0("fill=",col_var)),",",
                                    ifelse(col_var=="NULL","",paste0("colour=",col_var)),",",
                                    ifelse(shape_var=="NULL","",paste0("shape=",shape_var)),"))"))
        dataName = input$dataSet
        #------------------------------------------------------
        #END OF PRE_PROCESSING
      #---------------------------------------------  
        #
        #VISUALIZATION
        #
        if(input$method == "Preview"){
            if(is.data.frame(dataSet)){
              
              
              
              
              eval(parse(text=paste
                         
                         ("ggscatmat(",dataName,ifelse(col_var=="NULL","",paste0(",color = \"",col_var,"\"")),", alpha=0.5)")))
              
            }
            else if(is.ts(dataSet)){
                pspectrum(dataSet,plot=TRUE)
            }else if(is.matrix(dataSet)){
                ugr_filled_contour.matrix(dataSet)
            #else if(is.array(dataSet) & length(dim(dataSet))==3){
            }else{    plot(dataSet)
            }
        }else{
            
           
            #DF_VISUALIZATION
            #   
            #------------------------------------------------------
        if(input$method == "Exploratory: Summary by Columns Box Whisker"){
            rgrUtils::ugr_boxplot.data.frame(dataSet)
        }else if(input$method == "Exploratory: Summary by Columns Hist Density"){
            rgrUtils::ugr_hist.data.frame(dataSet)
        }else if(input$method == "Exploratory: Corr Plot w Corr Matrix"){
            corrplot(cor(dataSet_original_num),shape = "ellipse")
            #title(paste("Linear Corr Btw Continuous vars in", input$dataSet))
        }else if(input$method == "Principle Components"){
            PCA = prcomp(dataSet_original_num,center = T,scale.= T)
            fviz_pca_ind(PCA, col.ind="contrib")  +scale_color_distiller(direction=-1,palette = "Spectral")
            
            #http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining
            
        }else if(input$method == "Exploratory: Pairwise Plots (2D per pair) w Corr Matrix"){
          eval(parse(text=paste
                     
                     ("ggscatmat(",dataName,ifelse(col_var=="NULL","",paste0(",color = \"",col_var,"\"")),", alpha=0.5)")))
          
          
            
        }else if(input$method=="ParallelCoordinate/Andrew\'s Plot w table"){
            if(input$col_var=="NULL"){
                
                parallelplot(dataSet, title = dataName)
            }else{
                
                eval(parse(text = paste0("parallelplot(dataSet, groups =",dataName,"$",col_var,",title = ",input$dataSet,")")))
            }
          
        }else if(input$method == "1D Summary w Pivot Tables"){
          eval(gg_setaes)
          g=g + geom_boxplot() + geom_rug(alpha = .65) +scale_color_distiller(direction=-1,palette = "Spectral")
          
          if(!identical(group_var,c("NULL","NULL"))){
          eval(parse(text=paste0("g=g+facet_grid(",ifelse(group_var2!="NULL",".",group_var1),"~",
                                 ifelse(group_var1=="NULL",".",group_var1),")")))
          }
          g
        }else if(input$method=="2D (w Model(s))"){
          eval(gg_setaes)
          g = g+geom_point(alpha=.5)+ geom_rug(alpha = .65) +scale_color_distiller(direction=-1,palette = "Spectral")
          if(!identical(group_var,c("NULL","NULL"))){
            eval(parse(text=paste0("g=g+facet_grid(",ifelse(group_var2!="NULL",".",group_var1),"~",
                                   ifelse(group_var1=="NULL",".",group_var1),")")))          }
          g
          
        }else if(input$method=="3D Plot (w model(s))"){
          eval(parse(text = paste0("scatter3D(x=",dataName,"$",x_var,
                                   ",y=",dataName,"$",y_var,
                                   ",z=",dataName,"$",z_var,
                                   ifelse(col_var=="NULL","",paste0(",colvar=",dataName,"$",col_var)),
                                   ",xlab = \"",x_var,
                                   "\",ylab =\"",y_var,
                                   "\",zlab = \"",z_var,"\")")))
        }else if(input$method=="Model Performance"||"Model Insight"){
          # See applied predictive modeling and visreg
          input$Mode
          
          
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
