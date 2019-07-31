#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#### To-do list ####
## TODO: 
## TODO:  
## TODO: 

# Define UI for application
#Set up ui.R formatting
shinyUI(fluidPage(
  theme=shinytheme("superhero"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    HTML('<link rel="icon" href="favicon.ico">'), 
    # tags$link(rel="shortcut icon", href="favicon.ico"),
    tags$style(
      # Extra CSS goes in here. Try to put it in CSS file but if it does not 
      # work, put in here.
      HTML(
        ".tab-content p {
          font-size: 18px;
          text-align: center;
          background-color:#4e5d6c;
          margin-left: 5em;
          margin-right: 5em;
        }"
      )
    )
  ),
  
  # Application title
  #Header and title: has logo and brief website description
  #fluidRow refers to where on the page content should be placed as if split by rows and columns; refer to more on application layout guides about shiny
  #images should be kept in a folder in the same work directory as the R files for easy reference throughout
  #"tags" used throughout correspond with HTML tags; refer to HTML tags for specifics on formatting the website
  fluidRow(
    column(2,
           tags$br(),
           tags$img(src="Seal_of_Worcester.svg", width="100px", height="99px")),
    column(10,
      titlePanel("Exploration of Worcester Data"), 
      tags$h4("WPI 2019 Data Science REU"),
      tags$i("Explore data visualizations about service requests from data provided 
        by the Worcester Department of Public Works and Department of Inspectional Services."))
    
  ),
  tags$br(),
  #tabsetPanel and tabPanel refers to a new tab; keep all content intended to be in that tab within the tabsetPanel
  tabsetPanel(
    #### Overview Tab ####
    #The home page of the Shiny app; contains the slideshow and introductory information on the website
    tabPanel("Overview",
    fluidRow(column(12,slickROutput("slideshow"))),
    tags$br(),
    tags$br(),
    #Sources for photos from slideshow
    tags$div(tags$p("Sources", style="background-color:transparent;"),
             tags$ul(
             tags$li("WorcesterMA.gov"),
             tags$li("WorcesterMA.gov"),
             tags$li("Wikipedia User Terageorge | CC BY-SA 3.0"),
             tags$li("Boston Magazine | Shutterstock")
             ),
             style="font-size:10px;"),
      
      tags$hr(),
      
      # Adding Description to Overview
      tags$h4("The Department of Inspectional Services (DIS) and the Department of Public Works (DPW) provided information in hopes of making it more accessible to the public."),
      tags$br(),
      tags$h4("DIS deals with the following:"),
      tags$ul(
        tags$li("Building and Zoning"),
        tags$li("Housing and Health Inspections"),
        tags$li("Permits and Licensing")
      ),
      tags$br(),
      tags$h4("DPW deals with the following:"), 
      tags$ul(
        tags$li("Residential Drop-Off Center"),
        tags$li("Online Customer Service"),
        tags$li("Pothole Reports"),
        tags$li("Clean Worcester Water")
      ),
      tags$br(),
      tags$h4("The tabs at the top show statistical information about the departments' services. 
              Click through them to see what's going on in Worcester."),
      
      # Sources
      tags$br(),
      tags$hr(),
      tags$div(tags$p("Sources", style="background-color:transparent;"),
        tags$ul(
          tags$li("WorcesterMA.gov"),
          tags$li("WorcesterMA.gov"),
          tags$li("Wikipedia User Terageorge | CC BY-SA 3.0"),
          tags$li("Boston Magazine | Shutterstock")
        ),
      style="font-size:10px;")
    
    ),
    #END OVERVIEW TAB#  

    #### Tickets Tab ####
    #Contains general plots and information on the tickets overall
    #use plotlyOutput to select plots and other graphs to pull from the server.R file to display
    tabPanel("Tickets",
      tags$hr(),
      tags$p("When a complaint or request comes into the department, a ticket is formed. 
              The graphs below show the general statistics regarding the tickets."),
      fluidRow(column(12, plotlyOutput("aniPie"))),
      tags$br(),
      tags$p("If you press play, you can see how the proportion of open tickets versus closed tickets changes with time."),
      fluidRow(column(12,plotlyOutput("openTickets"))),
      tags$div(tags$p("The above plot represents how many tickets were open on a given day. 
                       Hover your mouse to see the number of tickets open on a given day.")),
      fluidRow(
        column(12,
               tags$br(),
          plotlyOutput("overviewOC")
        )
      ),
      tags$p("This plot displays the number of tickets submitted versus the number completed. 
              Hover your mouse to see the number of tickets open during a given month."),
      tags$br()
    ),
    #END TICKETS TAB#  

    #### Year Tab ####
    #Contains plots and information on tickets categorized by Year
    #the navbarMenu line sets up the Breakdown tab in which year/dept/priority/day is categorized
    #each tabPanel indicates the category selected and which plots go under that category
    #plots are referred to from the server.R file which is where they're made
    navbarMenu("Breakdown",
    tabPanel("By Year",
      tags$hr(),
      tags$p("This tab breaks up the ticket information by its intake or close year. 
              You can use the drop-down bar to select what year you're interested in."),
      fluidRow(column(12, plotlyOutput("yearPie"))),
      fluidRow(
        column(12,
        selectInput("year", "Select Year:", choices = sort(unique(w.data$Year)),
                      selected = sort(unique(w.data$Year))[1])
      )
      ),
      tags$h3("Tickets by Department"),
      fluidRow(column(12, plotlyOutput("departmentCount")),
      tags$p("This plot shows the number of tickets taken in and completed by DIS and DPW through the months by year. 
              To hide a specific category, click on it in the legend.")
      ),
      tags$br(),
      fluidRow(
        column(12, plotlyOutput("typeRequest")),
       tags$p("This plot shows the number of tickets with either DIS or DPW or both by Division. 
               To select an option, fill in the bubble below.") 
        ),
      fluidRow(
        column(12, radioButtons("yearRequestRadio", "Department:", inline=TRUE,
                               choices = list("DPW" = "dpw", "DIS" ="dis", "Both"="both")))
      ),
      tags$br()
    ),
    #END YEAR TAB#
    
    #### Department Tab ####
    #Contains plots and information on tickets categorized by Department
    #selectInput are the setup for drop-downs on the website in which users can select what department/year/etc they would like to look at
    #radioButtons look like little buttons in which users can select one or the other.
    tabPanel("By Department",
      tags$hr(),
      tags$p("Within each department is a division. 
              The drop-down will allow you to choose the division your interested in."),
      fluidRow(column(12, plotlyOutput("deptPie"))),
      fluidRow(
        column(5,
          selectInput("dept", "Select Department", 
                      choices = sort(unique(w.data$Division_Name_VC)))
        ),
        column(5,
          radioButtons("radYorM", "Display by Year or Month?", inline=TRUE,
                       choices = list("Year"="y", "Month"="m"), selected = "y"
          )
        )
      ),
      fluidRow(
        column(12,
          plotlyOutput("numRequestYear")
        ),
        tags$p("This plot shows the number of tickets throughout years or months by department. 
                To select year or month, click the bubble above.")
      ),
      fluidRow(
        column(12,
               plotlyOutput("overviewTicketCount"))
      ),
      fluidRow(
        column(12, 
               radioButtons("radioCount", "Type for Ticket Counts:", inline=TRUE,
                            choices = list("Opened", "Active", "Closed")))),
      tags$p("These are the number of tickets by department are plotted over time.
              To hide a division, click on its name in the legend. 
              To look at a different status, received, opened, or closed, select the bubble next to its name above."),
      tags$br(),
      fluidRow(
        column(12,
          plotlyOutput("streetDepartment")
        ),
      tags$p("This plot shows the number of tickets for each streets. 
              Adjust how many streets are displayed using the slider below.")
      ),
      fluidRow(
        column(12, sliderInput("streetSlide", "Number of Streets", min=5, max=55, value = 25))
      )
    ),
    #END DEPARTMENT TAB#
    
    #### Priority Tab ####
    #Contains plots and information on tickets categorized by Priority
    tabPanel("By Priority",
                  tags$hr(),

      tags$p("Each department uses priority statuses to distinguish between how urgent a complaint or request is. 
              Emergency depicts a ticket which needs immediate assistance. 
              Standard deals with tickets that do not need immediate attention.
              ADA/Spec Needs stands for American Disability Act Special Needs."),
      fluidRow(plotlyOutput("priorityPie")),
      tags$p("This pie chart shows the percentage of tickets that are Standard, Emergency, and ADA/Spec Needs."),
      fluidRow(column(12, plotlyOutput("priorityOverTime")))
      
    ),
    #END PRIORITY TAB#
    
    #### Day Tab ####
    #Contains plots and information on tickets categorized by Day of the Week
    
    tabPanel("By Day of Week",hr(),
      tags$p("Here, you can look at the activity of each day of the week. 
              The drop-down bar can be used to look at different days of the week."),
      fluidRow(
        column(12,
               selectInput("day","Select Day of the Week",
                           choices = days.of.week, selected = days.of.week[2])
        )
      ),
      fluidRow(
        column(12,
          plotlyOutput("dayPieChart")
        ),
        tags$p("These pie charts show the percentage of tickets that are open and closed for each day of the week. ")
      ),
      tags$br(),
      fluidRow(
        column(12,
          plotlyOutput("dayBarGraph")),
        tags$p("This plot shows the number of tickets that are taken in and the number of tickets that are closed during the hours of the day
               by each day of the week. Use the drop-down below to change the day you're looking at.")
        
      ),
      tags$br(),
      fluidRow(
        column(12,
          plotlyOutput("completeDay")
        ),
        tags$p("This boxplot shows the number of days to complete a ticket for each day of the week."),
        column(12,
               sliderInput("upp.lim", 'Set upper limit for boxplot',
                           min = 350, max=10000, value = 1500)
        )
      )
    )),
    #END DAY OF WEEK TAB#
    
    #### Map Tab ####
    #A map feature on the website that conatins information on what/when tickets have been completed based on their location
    #Users click on areas to zoom in on the map until a specific location is reached to view what tickets have been completed in their location
    #leafletOutput refers to the setup of the leaflet map feature
    tabPanel("Map",hr(),
      leafletOutput("map", height = 500),
      leafletOutput(("nhoodMap"), height = 500)
    ),
    #END MAP TAB#
    
    #### Outlier Tab ####
    #A tab on strange tickets and other outliers that have been found within the data set.
    tabPanel("Outlier Investigation",
      tags$hr(),
      fluidRow(column(12, plotlyOutput("meanTime"))),
      # tags$p("Outliers can be defined as blah blah blah"),
      fluidRow(column(12, align="center",dataTableOutput("meanTable"))),
      fluidRow(column(12, plotlyOutput("basicOutlier"))),
      tags$div(fluidRow(column(12, align="center",sliderInput("qSlide","Percentage cut off:", min=.800,max=.999,
                                      value=.975, step=.001, width="95%"))),
               style="background-color:#4e5d6c;"),
      fluidRow(column(12, plotlyOutput("pieOutlier"))),
      tags$p("Above is a breakdown of where the outliers are coming from."),
      fluidRow(column(12, plotlyOutput("columnOutlier", height = 800))),
      tags$hr(),
      tags$h2("November 2010"),
      tags$p("In November of 2010, over 8,000 tickets were closed. 
             The average number of tickets closed per month is about 3430 tickets.
             That means in November, ---percent more were closed than usual."),
      fluidRow(column(12, plotlyOutput("novemberBar"))),
      tags$p("As shown above, 4278 tickets were closed on Monday, Novemeber 15th. 
             The average number of tickets closed per day is 123.
             If we can assume it was a normal 8 hour work day, that means about 535 tickets had to be closed every hour.")
    ),
    #END OUTLIER TAB#
    
    #### About Tab ####
    #Tab that contains information on the REU team and acknowledgments
    tabPanel("About",hr(),
      tags$h1("The Project"),
      tags$p("We are looking into data from the Worcester City Government. We are analyzing tickets from the",
            tags$a(href="http://www.worcesterma.gov/dpw","Department of Public Works"),
            "and the",
            tags$a(href="http://www.worcesterma.gov/inspections","Department of Inspectional Services."), 
            "We are exploring the data for trends and correlations while also looking into anomalies in the duration a ticket 
            takes to be completed."),
      tags$br(),
      tags$p("This project is a part of the 2019", tags$a(href="https://www.wpi.edu/academics/departments/data-science/reu-program",
           "WPI REU in Data Science"), "funded by the", tags$a(href="https://www.nsf.gov/","National Science Foundation.")),
      tags$div(h1("The Team"),
      #REU Project Team
      fluidRow(column(8,
        tags$h3("Jack J Amend"),
        tags$ul(
          tags$li("School: Elon University"),
          tags$li("Major: Computer Science and Applied Mathematics"),
          tags$li("Minor: Statistics"),
          tags$li("Year: Senior")
        )),
        column(4, img(src="jackcrop.JPG", height=250, width=250))
      ),
      tags$br(),
      fluidRow(column(8,
        tags$h3("Elizabeth Hennen"),
        tags$ul(
          tags$li("School: Roger Williams University"),
          tags$li("Major: Biomedical Engineering"),
          tags$li("Minor: Mathematics, Biology, and Chemistry"),
          tags$li("Year: Junior")              
        )),

        column(4, img(src="elicrop.jpg", height=250, width=250))
      ),
      tags$br(),
      fluidRow(column(8,
      tags$h3("Megan Resurreccion"),
      tags$ul(
        tags$li("School: University of Illinois at Urbana-Champaign"),
        tags$li("Major: Creative Writing"),
        tags$li("Minor: Informatics and Statistics"),
        tags$li("Year: Senior"),
        tags$li(tags$a("Personal Website",href="https://junkdrawer760937947.wordpress.com/"))
      )),
        column(4,img(src="megan.jpeg", height=250, width=250))
      ),
      tags$br(),
      fluidRow(column(8,
      tags$h3("Mentor: Dr. Yanhua Li"),
      tags$ul(
        tags$li("School: Worcester Polytechnic Institute"),
        tags$li("Department: Computer Science"),
        tags$li(tags$a("Website",href="https://www.wpi.edu/people/faculty/yli15"))
                      
      )),
        column(4,img(src="drli.jpeg", height=250, width = 250))
      ),
      style=""),
      
      tags$hr(),
      tags$br(),
      tags$p("If you would like to give some feedback regarding the effectiveness of this website, please feel free to take this survey: ",
             tags$a(href="https://forms.gle/Fitp5R7zmLWEa3uy5", "Survey Link.")
             )
            
    )
    
  )
  
))
