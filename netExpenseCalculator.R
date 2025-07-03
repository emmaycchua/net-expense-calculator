library(shiny)

ui <- fluidPage(
  titlePanel("Net Expense Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("label", "Label your entry:", placeholder = "e.g. Groceries, Dinner"),
      dateInput("date", "Select date:", weekstart = 1),
      numericInput("amount", "Enter amount (negative for expense, positive for income):", value = 0),
      selectInput("category", "Select category:",
                  choices = c("Food", "Transport", "Shopping", "Other")),
      actionButton("add", "Add Entry"),
      br(), br(),
      selectInput("xvar", "Choose x-axis variable for the bar plot:",
                  choices = c("Month", "Category")),
      br(), br(),
      actionButton("delete", "Delete Selected Entry")
    ),
    
    mainPanel(
      DT::dataTableOutput("entryTable"),
      # above line allows for dates to be properly formatted
      # also adds search box, entries per page dropdown, column sorting,
      # and different pages for each entry
      h3("Net Total:"), # plain text
      textOutput("netTotal"), # placeholder for reactive text output
      h3("Expenses By Month and Category:"),
      plotOutput("barPlot"),
    )
  )
)

server <- function(input, output, session) {
  # Table displayed in mainPanel
  data <- reactiveVal(data.frame(
    # column header = data type
    Date = as.Date(character()),
    Label = character(),
    Amount = numeric(),
    Category = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$add, { 
    # line above is a reactive trigger - watches for change in input$add
    # which is what happens when "Add Entry" is clicked
    # when button is clicked, the following code is run
    new_entry <- data.frame(
      Date = as.Date(input$date),
      Label = input$label,
      Amount = input$amount,
      Category = input$category,
      stringsAsFactors = FALSE
    )
    
    # appends new entry to the main table
    data(rbind(data(), new_entry))
    
    # resets inputs after clicking "Add Entry"
    updateTextInput(session, "label", value = "")
    updateDateInput(session, "date", value = Sys.Date())
    updateNumericInput(session, "amount", value = 0)
    updateSelectInput(session, "category", selected = "Food")
  })
  
  output$entryTable <- DT::renderDataTable({
    data()
  }, selection = "single")
  
  output$netTotal <- renderText({
    total <- sum(data()$Amount)
    paste0("$", round(total, 2))
  })
  
  output$barPlot <- renderPlot({
    df <- data()
    
    # ensures there is data
    if (nrow(df) == 0) return(NULL)
    
    # generates Month column
    df$Month <- format(df$Date, "%Y-%m")
    
    if (input$xvar == "Month") {
      ggplot(df, aes(x = Month, y = -Amount, fill = Category)) +  # -Amount to show expenses upward
        geom_bar(stat = "identity") +
        labs(x = "Month", y = "Expenses") +
        theme_minimal()
    } else if (input$xvar == "Category") {
      ggplot(df, aes(x = Category, y = -Amount, fill = Month)) +
        geom_bar(stat = "identity") +
        labs(x = "Category", y = "Expenses") +
        theme_minimal()
    }
  })
  
  observeEvent(input$delete, {
    selected <- input$entryTable_rows_selected
    if (length(selected)) {
      current_data <- data()
      new_data <- current_data[-selected, ]  # remove selected row
      data(new_data)  # update the reactiveVal
    }
  })
  
}

shinyApp(ui = ui, server = server)
