shinyServer(function(input, output, session) {
  fps <- 10
  aps <- 5
  

  f <- grand_tour()
  
  rv <- reactiveValues()

  observeEvent(input$restart_random,
              {
                
                p <- length(input$variables)
                b <- matrix(runif(2*p), p, 2)
               
                rv$tour <- new_tour(as.matrix(rv$d[input$variables]),
                                  choose_tour(input$type, input$guidedIndex, c(rv$class[[1]])),
                                 b)
               })
  
  observeEvent(input$speed, rv$aps <- input$speed)
  
  observeEvent(input$density, {
    
    dens = 1
    
    if (input$density == "Off") {
      dens = 0
    }

    
    session$sendCustomMessage("density", toJSON(dens))
    
  })
  
  observeEvent(input$dataset, {
    rv$d <- switch (input$dataset,
                    "Gaussian" = data.frame( x1=rnorm(100), x2=rnorm(100), x3=c(rnorm(50, -2), rnorm(50, 2)), x4=c(rnorm(50, -2), rnorm(50, 2)), x5=rnorm(100), group=c(rep("A", 50), rep("B", 50)), stringsAsFactors = FALSE),
                    "Geozoo" = read.csv("geozoo.csv", stringsAsFactors = FALSE),
                    "Cognostics" = read.csv("tigs_music_seismic.csv", stringsAsFactors = FALSE),
    )
    
    nums <- sapply(rv$d, is.numeric)
    groups <- sapply(rv$d, is.character)
    
    updateCheckboxGroupInput(
      session, "variables",
      choices = names(rv$d[nums]),
      selected = names(rv$d[nums])[1:3]
    )

    updateSelectInput(session, "class", choices = names(rv$d[groups]))
    updateSelectizeInput(session, "class", selected = names(rv$d[groups])[1])
    
  })
  
  
  observeEvent(c(input$type, input$variables, input$guidedIndex, input$class),
               {

                 session$sendCustomMessage("debug", paste("Changed tour type to ", input$type))
                 if (length(input$variables) == 0) {
                   rv$mat <- rescale(as.matrix(rv$d[names(rv$d[nums])[1:3]]))
                   rv$class <- unname(rv$d[names(rv$d[groups])[1]])
                 } else {
                   
                   rv$mat <- rescale(as.matrix(rv$d[input$variables]))
                  rv$class <- unname(rv$d[input$class])
                 }


                 cl <- c(rv$class[[1]])

                 session$sendCustomMessage("newcolours", unique(cl))
                 
                 #browser()
                 
                 rv$tour <-
                   new_tour(as.matrix(rv$d[input$variables]),
                            choose_tour(input$type, input$guidedIndex, cl),
                            NULL)
               })
  
  
  holes_ <- function() {
    function(mat) {
      n <- nrow(mat)
      d <- ncol(mat)
      
      num <- 1 - 1/n * sum(exp(-0.5 * rowSums(mat ^ 2)))
      den <- 1 - exp(-d / 2)
      
      val <- num / den
      return(val)
    }
  }
  
  cmass_ <- function() {
    function(mat) {
      n <- nrow(mat)
      d <- ncol(mat)
      
      num <- 1 - 1/n * sum(exp(-0.5 * rowSums(mat ^ 2)))
      den <- 1 - exp(-d / 2)
      
      val <- num / den
      return(1 - val)
    }
  }
  
  
  
  observe({
    tour <- rv$tour
    aps <- rv$aps
    
    step <- tour(aps / fps)
    if (!is.null(step)) {
      invalidateLater(1000 / fps)
      
      j <- center(rv$mat %*% step$proj)
      j <- cbind(j, class = rv$class)
      colnames(j) <- NULL
      
      session$sendCustomMessage(type = "data", message = toJSON(j))
      
    }
    
    #browser()
    else{
      #browser()
      if (length(rv$mat[1, ]) < 3) {
        session$sendCustomMessage(type = "debug", message = "Error: Need >2 variables.")
      } else {
        session$sendCustomMessage(type = "debug", message = "Guided tour finished: no better bases found.")
      }
    }
  })
  
  
  
  choose_tour <- function(type,
                          subtype = "",
                          group_variable = ""
  )

  {

    
    if (type == "Grand")
    {
      tourType <- grand_tour()
    }
    else if (input$type == "Little") {
      tourType <- little_tour()
      
    } else
     
    {
      if (subtype == "Holes") {
        #browser()
        tourType <- guided_tour(holes_())
      } else if (subtype == "Centre Mass") {
        #browser()
        tourType <- guided_tour(cmass_())
      }
      else if (subtype == "LDA") {
        tourType <- guided_tour(lda_pp(group_variable))
      } else {
        tourType <- guided_tour(pda_pp(group_variable))
      }
      
    }
    
    return(tourType)
  }
  
})
