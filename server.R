function(input, output, session) {

# help modals
  
  # help for specifying the prior
  observeEvent(input$priorhelp, {
    showModal(modalDialog(
      title = "How to specify a prior:",
      "The prior represents how you would bet on the association between 
      CYP2D6 impairment and breast cancer recurrence before seeing any new data. 
      Here, you can specify the upper and lower 95% confidence limits for that association.
      The application will then derive the mean and variance of the corresponding normal distribution,
      which will be used as the prior for the Bayesian meta-analysis.
      The mean and variance of the prior and a plot of the normal density function 
      will appear at the bottom of the left pane after you click 'Run analysis'."
      ))
  })
  
  # help for fixed/random conventional
  observeEvent(input$metahelp, {
    showModal(modalDialog(
      title = "Selecting a meta-analysis model:",
      tags$div("This application offers a choice between a fixed or random-effects
      meta-analysis model. A fixed effects model assumes that all of the
      contributing studies are estimating a single, true association. 
      A random effects model is more appropriate when individual studies might
      be estimating different true effects (for example, in different populations). 
      More details and relevant literature citations are provided in
      the 'metafor'", tags$a(href='https://cran.r-project.org/web/packages/metafor/metafor.pdf', "package documentation."))
    ))
  })
  
  # help for conventional/Bayesian
  observeEvent(input$whatmeta, {
    showModal(modalDialog(
      title = "Conventional or Bayesian?",
      "Select a Bayesian meta-analysis if you would like to incorporate prior information
      about the strength of association between impaired CYP2D6 function and tamoxifen failure.
      Select a Conventional meta-analysis if you would just like to summarize the selected studies
      without incorporating prior information. For example, if you feel results from preclinical studies
      indicate that CYP2D6 impairment would increase the rate of recurrence among tamoxifen-treated women,
      you should select 'Bayesian' and specify a prior distribution for the likely effect size."
    ))
  })

  # help for selecting the tau prior  
  observeEvent(input$whattau, {
    showModal(modalDialog(
      title="Selecting an appropriate tau prior:",
      tags$div("For guidance on selecting an appropriate prior for the heterogeneity parameter (tau), please
               refer to the 'bayesmeta'", tags$a(href='https://cran.r-project.org/web/packages/bayesmeta/bayesmeta.pdf', "package documentation."))
    ))
    })
  
  # calculate prior parameters from numeric inputs in UI
    observeEvent(input$mkitso_b, {
    priormean <- (log(input$priormax) + log(input$priormin))/2
    exppriormean <- round(exp(priormean),digits=2)
    output$selpriormean <- renderText({paste("Prior mean RR =", exppriormean)})
    priorse <- (log(input$priormax) - log(input$priormin))/3.92
    priorvar <- priorse^2
    output$selpriorvar <- renderText({paste("Prior variance =", round(priorvar,3) )})
    x <- seq(-10,10,length=500) * priorse + priormean
    expx <- exp(x)
    y <- dnorm(x,priormean,priorse)
    priordf <- as.data.frame(cbind(x,expx,y))

    output$priorplot <- renderPlot({
      ggplot(priordf, aes(expx,y)) +
        geom_line(color="blue", size=1) +
        scale_x_log10(limits=c(0.1, 10)) + 
        geom_vline(xintercept=1, color="gray") +
        xlab("Relative risk") +
        ylab("Probability density") +
        theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
    })
    
    # create subset based on user-selected studies
    if (input$groupselect=="manualpick") {
      include_in_analysis<-input$incl_manualpick
    } else if (input$groupselect=="implausible"){
      include_in_analysis<-input$incl_implausible
    } else if (input$groupselect=="onlystarfour") {
      include_in_analysis<-input$incl_onlystarfour
    } else if (input$groupselect=="onlystarten") {
      include_in_analysis<-input$incl_onlystarten
    } else if (input$groupselect=="nonneo") {
      include_in_analysis<-input$incl_nonneo
    } else if (input$groupselect=="tumor") {
      include_in_analysis<-input$incl_tumor
    }
    
    
      metaSubset <- subset(metainput, author_year %in% include_in_analysis)
      
      metatable <- data.frame(metaSubset$author_year,
                              metaSubset$url,
                              metaSubset$dnasource,
                              metaSubset$variants,
                              metaSubset$rrpoint,
                              round(exp(metaSubset$lnlcl95),2),
                              round(exp(metaSubset$lnucl95),2))
      
      names(metatable) <- c("Study", "PubMed PMID", "DNA source", "CYP2D6 variants", "Relative risk", "LCL95", "UCL95")
      
      xmetatable <- DT::datatable(data=metatable, class='cell-border stripe', rownames = FALSE, escape = FALSE)
      
      output$studytable_b <- DT::renderDataTable({xmetatable})
      
      bma <- bayesmeta(y = metaSubset$logrr,
                       sigma = metaSubset$se_deriv,
                       labels = metaSubset$author_year,
                       tau.prior = input$tauprior,
                       mu.prior.mean = priormean,
                       mu.prior.sd = priorse)
      
      #Forest plot of the bayesmeta object
      txtparms <- fpTxtGp(cex=1.2, xlab = gpar(cex=1.2), ticks=gpar(cex=1.2))
      output$forestb <- renderPlot({
        forestplot.bayesmeta(x=bma,
                           exponentiate=TRUE,
                           shrinkage = FALSE,
                           prediction = FALSE,
                           txt_gp=txtparms,
                           digits=2,
                           graph.pos=2,
                           #lineheight=unit(1,"cm"),
                           #hrzl_lines=TRUE,
                           xlog=TRUE,
                           xticks=c(0.5, 1.0, 2.0, 5.0),
                           xlab="Relative risk, 95% CI",
                           clip=c(0.1,20)
                           )
        }, height=850)

      #Funnel plot of bmeta studies
      output$bmetafunnel <- renderPlot({
          funnel.bayesmeta(x=bma, main="")
      }, height=600)
      
      #Summary output
      output$bmetastats <- renderPrint(exp(bma$summary))
      output$bmetatime <- renderPrint(bma$init.time)
      
      #Posterior and whatnot and so forth
      output$jointdens <- renderPlot({
        plot.bayesmeta(x=bma, main="", which=2)
      }, height=600)
      
      output$margpost <- renderPlot({
        plot.bayesmeta(x=bma, main="", which=3, prior=TRUE)
      }, height=600)
    })

    
### Conventional meta-analysis and output objects

observeEvent(input$mkitso_c, {
  
    # create subset based on user-selected studies
      if (input$groupselect=="manualpick") {
      include_in_analysis<-input$incl_manualpick
    } else if (input$groupselect=="implausible"){
      include_in_analysis<-input$incl_implausible
    } else if (input$groupselect=="onlystarfour") {
      include_in_analysis<-input$incl_onlystarfour
    } else if (input$groupselect=="onlystarten") {
      include_in_analysis<-input$incl_onlystarten
    } else if (input$groupselect=="nonneo") {
     include_in_analysis<-input$incl_nonneo
    } else if (input$groupselect=="tumor") {
      include_in_analysis<-input$incl_tumor
    }
    
    # cat(file=stderr(),"input value", include_in_analysis,"\n")

  
    metaSubset <- subset(metainput, author_year %in% include_in_analysis)
    metaSubset <- metaSubset[order(metaSubset$rrpoint),]

    metatable <- data.frame(metaSubset$author_year,
                            metaSubset$url,
                            metaSubset$dnasource,
                            metaSubset$variants,
                            metaSubset$rrpoint,
                            round(exp(metaSubset$lnlcl95),2),
                            round(exp(metaSubset$lnucl95),2))
    
    names(metatable) <- c("Study", "PubMed PMID", "DNA source", "CYP2D6 variants", "Relative risk", "LCL95", "UCL95")
    
    xmetatable <- DT::datatable(data=metatable, class='cell-border stripe', rownames = FALSE, escape = FALSE)
    
    output$studytable <- DT::renderDataTable({xmetatable})
    
    # define vectors of log effect estimates (yi), variances (vi), and author (id)
    yi <- log(metaSubset$rrpoint)
    vi <- metaSubset$var_deriv
    id <- metaSubset$author_year
    
    # carry out the meta-analysis
    if (input$metamethod=="Fixed effects") {
      metakind <- "FE"
    } else if (input$metamethod=="Random effects") {
      metakind <- "DL"
    }

    outmeta <- rma.uni(yi,
                       vi,
                       method=metakind,
                       measure="GEN")
    
    # funnel plot of studies
    output$rmafunnel <- renderPlot({
      funnel(x=outmeta)
    }, height=600)
  
    output$metastats <- renderPrint(outmeta)

    output$forest_conv <- renderPlot({ 
      forest.rma(x=outmeta,
             showweights=FALSE,
             refline=0,
             slab=id,
             atransf=exp,
             xlab="Relative risk, 95% CI",
             mlab="Summary RR from meta-analysis:",
             #ylim=c(0,68),
             annosym=c("(",",",")"),
             col="blue",
             cex=1.2,
             annotate=TRUE)
    }, height=850)
  })
}