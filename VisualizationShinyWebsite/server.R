#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#### To-do list ####
## TODO: 

#### Server Function ####
shinyServer(function(input, output) {
  
  #### Reactive Functions ####
  #' Reactive functions
  #' reactive basically lets the Shiny app instantly update itself 
  #' whenever the user makes a change
  
  by.year <- reactive({
    year.data <- w.data[year(w.data$Intake_Date_DT) == input$year,]
    return(year.data)
  }) 
  
  full.year <- reactive({
    ## This is how to make it on the spot
    year.data <- w.data[year(w.data$Intake_Date_DT) == input$year | year(w.data$Closed_DT) == input$year,]
    year.data
  })
  
  by.dept <- reactive({
    the.dept <- w.data[w.data$Division_Name_VC == input$dept, ]
    return(the.dept)
  })

  #### Overview Tab ####
  #' Images for the slideshow
  #' Images are contained in a different file
  #' renderSlickR uses the slickR package, and in this case, sets up the slideshow for the ui.R file
  #' "www/slideshow-img/" is the file that has the images
  output$slideshow <- renderSlickR({
    imgs <- list.files("www/slideshow-img/", full.names=TRUE)
    slickR(imgs)
  })
#END OVERVIEW TAB#
  
  #### Ticket Tab ####
  #' Plots for the Ticket Tab created here
  #' Animated pie chart (has play button) of open/closed tickets by month
  #' renderPlotly uses the plotly package to create a visualization
  #' type is the kind of plot you're using; in this case it is a pie chart
  output$aniPie <- renderPlotly({
    plot_ly(melted, labels=~variable, values=~value, frame=~Date, 
        type='pie', sort=FALSE) %>%
      animation_slider(steps=steps.list)  %>%
      layout(title="Percentage of Opened and Closed Tickets by Month", margin=m.top)
  })
  
  #' Line graph of number of open tickets throughout the data
  #' "openTickets" is the name of the plot to be used and referenced in ui.R
  #' filter is used to to section off specific data points in the original data set
  #' in this case, Days with data points with dates before 2019-01-01 are selected
  #' plot_ly is the main function for creating the plot
  #' x is the the value of the x axis of the visualizaiton
  #' y is the value of the y axis of the visualization
  #' type is the kind of plot you're using
  #' mode is how you would like the data points to look; in this case it is 'lines', which makes it a line graph
  #' layout parameters determine the other aesthetics of the graph such as title and axis labels
  output$openTickets <- renderPlotly({
    ticket.day %>%
      filter(Day < date("2019-01-01")) %>%
      plot_ly(x = ~Day, y = ~Active, type = 'scatter', mode = 'lines', name='Active') %>%
      layout(title = "Number of Tickets by Type", xaxis = list(title="Date"), 
             yaxis = list(title="Count"), margin=m.top) 
  })
  
  #' In versus Out line graph of Intake vs completed tickets by month
  #' "group_by" converts and transforms tbls
  #' "summarise" is used on grouped data
  #' "add_trace" adds traces to a visualization
  output$overviewOC <- renderPlotly({
    ticket.day %>%
      filter(Day < date("2019-01-01")) %>%
      group_by(Month=floor_date(Day, "month")) %>%
      summarise(OpenSum=sum(Opened), CloseSum=sum(Closed)) %>%
      plot_ly(x=~Month, y=~OpenSum, type='scatter', mode='lines',name='Opened') %>%
      add_trace(y=~CloseSum, type='scatter', mode='lines',name='Closed') %>%
      layout(title="Opened versus Closed", yaxis=list(title="Count"), margin=m.top)
  })
  
  #Ticket count by department. Line graph of ticket count by each department.
  output$overviewTicketCount <- renderPlotly({
    dept.day.tickets %>%
      group_by(Dept, Month=floor_date(Day, "month")) %>%
      summarise(Count=sum(!!sym(input$radioCount), na.rm=TRUE)) %>%
      plot_ly(x=~Month, y=~Count, color=~Dept, colors='Paired', type='scatter', mode='lines') %>%
      layout(title="Open Tickets by Month and Department", margin=m.top)
  })
#END TICKET TAB#
   
  #### Year Tab ####
  #Plots for the Year tab created here; graphs vary by the year that is selected
  #Pie chart of active tickets by each year
  output$yearPie <- renderPlotly({
    year.table <- data.frame(table(Year=w.data$Year))
    year.table <- year.table[order(as.numeric(year.table$Year)),]
    plot_ly(year.table, labels=~Year, values=~Freq, type='pie', sort=FALSE,
            hoverinfo=~paste("Year:", Year)) %>%
      layout(title="Years Pie Chart", margin=m.top)
  })
  
  #Tickets by department bar graph.
  output$departmentCount <- renderPlotly({
    year <- by.year()
    fullyear <- full.year()
    dis <- year[year$Type == "DIS",]
    dpw <- year[year$Type == "DPW",]
    
    #filtering data by either DIS or DPW
    closedyear <- w.data[year(w.data$Closed_DT)==input$year,]
    disCl <- closedyear[closedyear$Type == "DIS",]
    dpwCl <- closedyear[closedyear$Type == "DPW",]
    
    #formatting intake/closed data of dpw and dis to include in the bar graph below
    x <- month.name
    y1.dis <- Fill.Table(table(month(dis$Intake_Date_DT)))
    y2.dpw <- Fill.Table(table(month(dpw$Intake_Date_DT)))
    y3.dis <- Fill.Table(table(month(disCl$Closed_DT)))
    y4.dpw <- Fill.Table(table(month(dpwCl$Closed_DT)))
    
    year.df <- data.frame(x, y1.dis, y2.dpw, y3.dis, y4.dpw)
    
    year.df$x <- factor(year.df$x, levels = month.name)
    
    #Number of intake tickets and closed tickets bar graph by DIS or DPW
    plot_ly(year.df, x=~x, y=~y1.dis, type='bar', name='DIS - In') %>%
      add_trace(y=~y2.dpw, name="DPW - In") %>%
      add_trace(y=~y3.dis, name="DIS - Out") %>%
      add_trace(y=~y4.dpw, name="DPW - Out") %>%
      layout(title="Tickets", yaxis=list(title="Count"), xaxis=list(title="Month"), 
             barmode='group', margin=m.top)
  })
  #Side bar graph of number of tickets by division. There's a choice to either view it through DIS, DPW, or both as well.
  output$typeRequest <- renderPlotly({
    year <- by.year()
    #If else statement to choose between DIS, DPW, or both for the bar graph.
    if (input$yearRequestRadio == 'dpw') {
      dept <- year[year$Type == "DPW",]
    } else if (input$yearRequestRadio == 'dis') {
      dept <- year[year$Type == "DIS",]
    } else if (input$yearRequestRadio == 'both') {
      dept <- year
    } else {
      stop("Option in the yearReqyestRadio does not exist. Fix in server.")
    }
    #Plot code for number of tickets by division
    div <- data.frame(table('Division'=dept$Division_Name_VC))
    div$Division <- factor(div$Division, levels = rev(div$Division))
    plot_ly(data=div, x=~Freq, y=~Division, type="bar")  %>% 
      layout(title="Ticket Count by Division", margin=m.top, xaxis=list(title="Count"))
  })
#END YEAR TAB#
  
  #### Department Tab ####
  #Plots for the Department tab created here; graphs vary by selected department
  #Pie chart of number of tickets by department
  output$deptPie <- renderPlotly({
    dept.table <- data.frame(table(Dept=w.data$Division_Name_VC))
    dept.table <- dept.table[order(dept.table$Dept),]
    plot_ly(dept.table, labels=~Dept, values=~Freq, type='pie', sort=FALSE) %>%
      layout(title="Department Pie Chart", margin=m.top)
  })
  
  #Bar graph of number of tickets by department by either year or month. Department can be selected with drop down button.
  output$numRequestYear <- renderPlotly({
    dept <- by.dept()
    #If else statement for selecting either to view graph by year or month.
    if (input$radYorM == "y") {
      dpt <- data.frame(table(Date=year(dept$Intake_Date_DT)))
    } else if (input$radYorM == "m") {
      dpt <- data.frame(table(Month=month(dept$Intake_Date_DT), Year=year(dept$Intake_Date_DT)))
      dpt$Date <- as.Date(paste(dpt$Year,dpt$Month, "1",sep="-"), format = "%Y-%m-%d")
      dpt <- select(dpt, c(Date, Freq))
    } else {
      stop("Error with radYorM radio button. Look at server")
    }
    
    plot_ly(dpt, x=~Date, y=~Freq, type="bar")  %>% 
      layout(title="Ticket Count by Year", margin=m.top, yaxis=list(title="Count"))
  })
  
  #Bar graph of number of tickets by street. Number of streets to be viewed can be adjusted. 25 streets is the default setting.
  output$streetDepartment <- renderPlotly({
    dept <- by.dept()
    street.df <- data.frame(table(Street=dept$Street_Name_VC))
    street.df <- street.df[order(-street.df$Freq),][1:input$streetSlide,]
  street.df$Street <- factor(street.df$Street)

    plot_ly(street.df, x=~Street, y=~Freq, type="bar")  %>% 
      layout(title="Ticket Count by Street", margin=m.top, yaxis=list(title="Count"))
  })
#END DEPARTMENT TAB#
  
  #### Priority Tab ####
  #Plots for the Priority tab created here
  #Pie chart of number of tickets by priority (ADA/Spec Needs, Emergency, or Standard)
  output$priorityPie <- renderPlotly({
    #Sets up data frame by priority status
    prior.df <- data.frame(table(Priority=w.data$Priority_CH))
    
    plot_ly(prior.df, labels=~Priority,values=~Freq,type='pie')  %>% 
      layout(title="Priority Pie Chart", margin=m.top)
  })

  #Line graph of number of tickets by priority over time. ADA/Spec needs is not included due to a low frequency of them.
  #filtering standard data first
  output$priorityOverTime <- renderPlotly({
    stand.data <- w.data %>% 
      filter(Priority_CH == 'STANDARD') %>%
      group_by(Month=month(Intake_Date_DT), Year=year(Intake_Date_DT)) %>%
      tally()
  #filtering emergency data next
    emerg.data <- w.data %>% 
      filter(Priority_CH == 'EMERGENCY') %>%
      group_by(Month=month(Intake_Date_DT), Year=year(Intake_Date_DT)) %>%
      tally()
    #merging standard and emergency data sets together
    prio.data <- merge(stand.data, emerg.data, by=c("Month", "Year"))
    prio.data$Date <- as.Date(paste(prio.data$Year,prio.data$Month, "1",sep="-"), format = "%Y-%m-%d")
    prio.data <- select(prio.data, c(Date, n.x, n.y))
    prio.data <- prio.data[order(prio.data$Date),]
    
    plot_ly(prio.data, x=~Date, y=~n.x, name="STANDARD", type='scatter', mode='lines') %>%
      add_trace(y=~n.y, name="EMERGENCY") %>%
      layout(title="Number of Request by Priority over Time", margin=m.top, 
             yaxis=list(title="Count"))
  })
  #Plot not used in the ui.R file
  #Assumed to be box plot of emergency versus standard time to complete data
  output$priorityBoxTime <- renderPlotly({
    stand <- select(filter(w.data, Priority_CH == 'STANDARD' & !is.na(Time_to_Complete)), Time_to_Complete)
    emerg <- select(filter(w.data, Priority_CH == 'EMERGENCY' & !is.na(Time_to_Complete)), Time_to_Complete)
    
    plot_ly(data=stand,y=~Time_to_Complete, type='box', name='STANDARD') %>%
      add_trace(data=emerg, y=~Time_to_Complete, name='EMERGENCY') %>%
      layout(yaxis=list(title='Time to Complete',range=c(0,1000)), margin=m.top) 
  })
#END PRIORITY TAB#
    
  #### Day Tab ####
  #Plots for the Day of the Week tab created here
  #Graphs may vary depending on the day of the week that is selected
  #Two pie charts in one visualization: number of open tickets by day of the week and number of closed tickets by day of the week
  output$dayPieChart <- renderPlotly({
    inpie.day.df <- data.frame(table(Day=w.data$Intake_Day_of_Week))
    outpie.day.df <- data.frame(table(OutDay=w.data$Closed_Day_of_Week))

    plot_ly(inpie.day.df,labels=~Day,values=~Freq, type='pie', name='Opened', title='Open',domain = list(row = 0, column = 0)) %>%
      add_pie(data=outpie.day.df,labels=~OutDay,values=~Freq, name='Closed', title='Closed', domain = list(row = 0, column = 1)) %>%
      layout(title="Pie Chart by Day of the Week", margin=m.top,
             grid=list(rows=1, columns=2)
      )
  })
  
  #Bar graph of selected day of the week's frequency intake time and close time
  output$dayBarGraph <- renderPlotly({
    day <- filter(w.data,Intake_Day_of_Week==input$day)
    #filtering and combining data of intake time by day of week and close time by day of week
    in.day.df <- data.frame(table(hour(day$Intake_Date_DT)))
    out.day.df <- data.frame(table(hour(day$Closed_DT)))
    time.inout <- merge(in.day.df, out.day.df, by="Var1")
    
    plot_ly(time.inout, x=~Var1, y=~Freq.x, type='bar', name='Ticket Intake') %>%
      add_trace(y=~Freq.y, name='Ticket Closed') %>%
      layout(title="Time Frequency Intake by Day of Week",yaxis=list(title="Count"),
             xaxis=list(title="Hour"), margin=m.top)
  })
  #Box plot of the time to complete a ticket by the selected day of the week. 
  #The upper limit for the box plot (aka the upper limit for days to complete a ticket) can be adjusted
  output$completeDay <- renderPlotly({
    day <- filter(w.data,(Intake_Day_of_Week==input$day))
    plot_ly(day, y=~Time_to_Complete, type='box', name="Time to Complete") %>%
      layout(title="Time to Complete by Day of Week", yaxis=list(title="Days",range=c(0,input$upp.lim)), margin=m.top)
  })
#END DAY OF WEEK TAB#  
  
  #### Map Tab ####
  #Implementing the map feature here
  #Other code on setting up map is not in this file
  output$map <- renderLeaflet({
    wor.map
  })
  
  output$nhoodMap <- renderLeaflet({
    
  })
#END MAP TAB#
  
  #### Outlier Tab ####
  #Plots for the Outlier tab created here (some of these may not be used)
  #Line graph of average/mean time to close tickets by month
  output$meanTime <- renderPlotly({
    w.data %>% 
      filter(Closed_BT) %>%
      group_by(Date=floor_date(Closed_DT,"month")) %>%
      summarise(Mean=mean(Time_to_Complete)) %>%
      plot_ly(x=~Date, y=~Mean, type='scatter', mode='lines', name='closed mean') %>%
      layout(title="Mean of Time to Close", margin=m.top)
  })
  #Box plot (presumably) of time to complete tickets by month
  output$meanTable <- renderDataTable({
    time.by.cl.dt <- w.data %>% 
      filter(Closed_BT) %>%
      group_by(Date=floor_date(Closed_DT,"month")) %>%
      summarise(Mean=mean(Time_to_Complete), Min=min(Time_to_Complete), 
                Q1=quantile(Time_to_Complete, .25), Median=median(Time_to_Complete), 
                Q3=quantile(Time_to_Complete, .75), Max=max(Time_to_Complete))
  }, options = list(dom = 'ftp', pageLength = 10))
  #bar graph of tickets closed on November 1, 2010. That day had many tickets close at the same time.
  # "count" counts the number of occurrences/observations specified in the function
  output$novemberBar <- renderPlotly({
    w.data %>%
      filter(floor_date(Closed_DT, "month") == as.Date("2010/11/1")) %>%
      count(Day=floor_date(Closed_DT, "day")) %>%
      plot_ly(x=~Day, y=~n, type='bar')
  })
  #general number of outliers bar graph based on time to complete a ticket 
  output$basicOutlier <- renderPlotly({
    w.data %>%
      filter(Closed_BT) %>%
      count(Outlier=Time_to_Complete > quantile(Time_to_Complete, input$qSlide)) %>%
      plot_ly(x=~Outlier, y=~n, type='bar') %>%
      layout(title="Number of Outliers", margin=m.top)
  })
  #Pie charts of time to complete tickets based on Type, Year, Department/Division, and Priority. Outliers are distributed through these 4 categories
  output$pieOutlier <- renderPlotly({
    sub.data <- w.data %>%
      filter(Closed_BT) %>%
      filter(Time_to_Complete > quantile(Time_to_Complete, input$qSlide))
    
    pie1 <- sub.data %>%
      count(Type)
    
    pie2 <- sub.data %>%
      count(Year) 
    
    pie3 <- sub.data %>%
      count(Dept=Division_Name_VC)
    
    pie4 <- sub.data %>% 
      count(Priority=Priority_CH)
    
    plot_ly() %>%
      add_pie(data=pie1, labels=~Type, sort=F, text="", textinfo='text', values=~n, title='Type', name='Type', domain=list(row = 0, column = 0)) %>%
      add_pie(data=pie2, labels=~Year, sort=F, text="", textinfo='text',  values=~n, text="", textinfo='text', title='Year', name='Year',domain=list(row = 0, column = 1)) %>%
      add_pie(data=pie3, labels=~Dept, sort=F, text="", textinfo='text', values=~n, title='Dept',name='Dept', domain=list(row = 1, column = 1)) %>%
      add_pie(data=pie4, labels=~Priority, sort=F, text="", textinfo='text', values=~n, title='Priority',name='Priority', domain=list(row = 1, column = 0)) %>%
      layout(title="Distribution of Outliers", margin=list(t=75,b=75), 
             grid=list(rows=2, columns=2), showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
             )
  })
  #Bar charts by year, department, type, and priority of time to complete tickets; shows outliers by variables
  output$columnOutlier <- renderPlotly({
    sp1 <- w.data %>%
      filter(Closed_BT) %>%
      filter(Time_to_Complete > quantile(Time_to_Complete, input$qSlide)) %>%
      count(Year) %>%
      plot_ly(x=~Year, y=~n, type='bar', name="Year")
    
    sp2 <- w.data %>%
      filter(Closed_BT) %>%
      filter(Time_to_Complete > quantile(Time_to_Complete, input$qSlide)) %>%
      count(Dept=Division_Name_VC) %>%
      plot_ly(x=~Dept, y=~n,  type='bar', name="Dept")
    
    sp3 <- w.data %>%
      filter(Closed_BT) %>%
      filter(Time_to_Complete > quantile(Time_to_Complete, input$qSlide)) %>%
      count(Type) %>%
      plot_ly(x=~Type, y=~n,  type='bar',name="Type")
    
    sp4 <- w.data %>%
      filter(Closed_BT) %>%
      filter(Time_to_Complete > quantile(Time_to_Complete, input$qSlide)) %>%
      count(Priority=Priority_CH) %>%
      plot_ly(x=~Priority, y=~n, type='bar', name="Priority")
    
    subplot(sp3, sp1, sp4, sp2, nrows = 4) %>%
      layout(title="Outliers by Variables", margin=m.top, showlegend=F)
  })
#END OUTLIER TAB#
  
  #### Old Functions ####
  #Plots that were created but were not used in the final draft of the website
  #Many of these plots used ggplot (from the ggplot2 package) as opposed to plot_ly from the plotly package
  output$deptTypeReq <- renderPlot({
    year <- by.year()
    ggplot(year, aes(Division_Name_VC, fill=Type)) + 
      geom_bar() +
      coord_flip() +
      scale_fill_manual(values=c("green","purple"))
  })
  
  output$tableRequest <- renderTable({
    year <- by.year()
    types <- count(year, Division_Name_VC)
    names(types) <- c("Division", "Count")
    types
  })
  
  output$hourSubmitted <- renderPlotly({
    year <- by.year()
    # summarize(groupby(year, hour(Intake_Date_DT)), count=n())
    ggplot(year, aes(x=hour(Intake_Date_DT))) + 
      geom_bar() +
      labs(title = "Number of request by hour", x="Hour", y="Count")
  })
  
  output$opened.closed <- renderPlotly({
    year <- full.year()
    temp <- melt(year[,c("Service_Request_ID","Intake_Date_DT","Closed_DT")], id="Service_Request_ID")
    
    ggplot(temp, aes(x=month(value), fill=variable)) +
      geom_bar(position=position_dodge())
  })
  
  output$tableHour <- renderTable({
    year <- by.year()
    types <- count(year, hour(Intake_Date_DT))
    names(types) <- c("Hour", "Count")
    types
  })
  
})
