library(shiny)
source("callin.R")

fluidPage(theme = shinytheme("cosmo"),
	
	titlePanel("Meta-analysis of CYP2D6 impairment and tamoxifen failure",
             windowTitle = "CYP2D6 META"),
	
	shiny::actionLink("startvid", label=h5("Watch introductory video"), width='100%', onclick="window.open('2d6meta_intro.mp4')"),
	
	busyIndicator(text="Calculations in progress...", wait=500),
	
	sidebarLayout(position="left",
	  sidebarPanel(
  	    shiny::actionButton("background", label=h4("Background and search criteria"), width='100%', onclick = "window.open('Background.pdf')"),
  	    
	      radioButtons("metatype", 
	                   label=h3("Select type of meta-analysis:"), 
	                   choices=c("Conventional", "Bayesian"), 
	                   selected="Conventional"),
  	    shiny::actionLink("whatmeta", label=h5("Help me choose")),
  	    hr(),
  	    
	    	conditionalPanel(condition="input.metatype=='Bayesian'",
	    	               h3("Specify your prior:"),
	    	               actionLink("priorhelp", label=h5("How do I specify a prior?")),
	                     numericInput(inputId="priormin", 
	                                 label="Lower 95% limit of relative risk:", 
	                                 value=0.25,
	                                 min=NA,
	                                 max=NA,
	                                 step=0.05
	                                 ),
                    	 numericInput(inputId="priormax", 
                    	             label="Upper 95% limit of relative risk:", 
                    	             value=4,
                    	             min=NA,
                    	             max=NA,
                    	             step=0.05
                    	             ),
	    	               h5("Your prior will be displayed below, after you click 'Run analysis'"),
	    	               hr(),
	    	               radioButtons("tauprior",
	    	                            label=h4("Select prior for tau (heterogeneity parameter):"),
	    	                            choices = c("Jeffreys", "uniform", "conventional", "shrinkage"),
	    	                            selected="Jeffreys"),
	    	               shiny::actionLink("whattau", label=h5("Help me choose"))
	    	),
	    	
	    	conditionalPanel(condition="input.metatype=='Conventional'",
	    	                 radioButtons("metamethod",
	    	                              label=h3("Select model:"),
	    	                              choices = c("Fixed effects", "Random effects"),
	    	                              selected="Fixed effects"),
	    	                 shiny::actionLink("metahelp", label=h5("Help me choose"))
	    	                 
	    	),
	   hr(),

		  selectInput(inputId="groupselect", label=h3("Select studies to include:"),
        c("All studies" = "manualpick", 
          "Studies with RR<=2" = "implausible",
          "Studies of CYP2D6*4 among Caucasians" = "onlystarfour",
          "Studies of CYP2D6*10 among Asians" = "onlystarten",
          "Studies genotyping non-neoplastic DNA" = "nonneo",
          "Studies genotyping tumor DNA" = "tumor"), 
          selected="manualpick"),
	        h5("Studies may be added or removed individually by clicking their checkboxes"),
	        h5("(scroll down to run the analysis)"),
      		    
		      conditionalPanel(condition="input.groupselect == 'manualpick'",
		      checkboxGroupInput(inputId="incl_manualpick", 
		                       label="", 
		                       choices=metainput$author_year,
		                       selected=metainput$author_year)
		      ),
		  
		      conditionalPanel(condition="input.groupselect == 'implausible'",      
          checkboxGroupInput(inputId="incl_implausible", 
                          label="", 
                          choices=metainput$author_year,
                          selected=plausibles)
		      ),
      
		      conditionalPanel(condition="input.groupselect == 'onlystarfour'",
          checkboxGroupInput(inputId="incl_onlystarfour",
                          label="",
                          choices=metainput$author_year,
                          selected=starfour)
		      ),
		  
		      conditionalPanel(condition="input.groupselect == 'onlystarten'",
		      checkboxGroupInput(inputId="incl_onlystarten",
                          label="",
                          choices=metainput$author_year,
                          selected=starten)
		      ),
		      conditionalPanel(condition="input.groupselect == 'nonneo'",
		      checkboxGroupInput(inputId="incl_nonneo",
		                      label="",
		                      choices=metainput$author_year,
		                      selected=normaldna)
		      ),
      
          conditionalPanel(condition="input.groupselect == 'tumor'",
          checkboxGroupInput(inputId="incl_tumor",
                             label="",
                             choices=metainput$author_year,
                             selected=tumordna)
          ),
	   
	        conditionalPanel(condition="input.metatype=='Conventional'",
	                         shiny::actionButton("mkitso_c", label=h4("Run analysis"), width='100%')
	                         ),
	        conditionalPanel(condition="input.metatype=='Bayesian'",
	                         hr(),
	                         shiny::actionButton("mkitso_b", label=h4("Run analysis"), width='100%'),
	                         hr(),
	                         h4("Summary of specified prior:"),
	                         h5(textOutput(outputId="selpriormean")),
	                         h5(textOutput(outputId="selpriorvar")),
	                         plotOutput(outputId="priorplot")
	                         
	        ),
	   hr(),
	   a(shiny::actionButton(inputId = "email1", 
	                  label = "Contact us!", 
	                  icon = icon("envelope", lib = "font-awesome")),
	     href="mailto:02tahern@med.uvm.edu")
		),
	  
		mainPanel(
		  fluidRow(
		  conditionalPanel(condition="input.metatype=='Conventional'",
		    tabsetPanel(tabPanel("Forest plot", plotOutput("forest_conv", height="auto")),
		                tabPanel("Funnel plot", plotOutput("rmafunnel")),
		                tabPanel("Study information", DT::dataTableOutput("studytable")),
		                tabPanel("Meta-analysis statistics", verbatimTextOutput("metastats"))
		               )
		              ),
		  conditionalPanel(condition="input.metatype=='Bayesian'",
		                   tabsetPanel(tabPanel("Forest plot", 
		                                        plotOutput("forestb", height="auto")),
		                               tabPanel("Funnel plot", plotOutput("bmetafunnel")),
		                               tabPanel("Study information", DT::dataTableOutput("studytable_b")),
		                               tabPanel("Meta-analysis statistics",
		                                        p("Analysis time:"), 
		                                        verbatimTextOutput("bmetatime"),
		                                        p("Summary statistics:"),
		                                        verbatimTextOutput("bmetastats")),
		                               tabPanel("Joint posterior", plotOutput("jointdens", height=600),
		                                        p("Note: effect (mu) is on the log scale"),
		                                        p("Red contours: 2-dimensional credible regions"),
		                                        p("Red cross: posterior mode"),
		                                        p("Green lines: marginal posterior medians and 95% credible interval"), 
		                                        p("Blue lines: conditional mean effect (solid) and 95% credible interval (dashed)")),
		                               tabPanel("Effect posterior", plotOutput("margpost", height=600),
		                                        p("Note: effect (mu) is on the log scale"),
		                                        p("Dashed line: the specified prior distribution"),
		                                        p("Vertical line: posterior median"),
                                            p("Dark shading: 95% credible region"))
		                   ),
		            )
	 )
  )
 )
)
