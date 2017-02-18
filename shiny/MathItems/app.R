library(shiny)
library(gdata)
library(markdown)
library(git2r)

repo.url <- 'https://github.com/DAACS/DAACS-Website'
items.dir <- 'repo/assessments/mathematics/items/'

# Download the assessment repo
path <- file.path('repo')
if(dir.exists(path)) {
	unlink(path, recursive = TRUE, force = TRUE)
}
dir.create(path, recursive=TRUE)
repo <- clone(repo.url, path)

# Copy the image files to the www directory
dir.create(file.path('www'), recursive = TRUE)
file.copy(paste0('repo/assessments/mathematics/items/',
				 list.files('repo/assessments/mathematics/items', pattern='*.png')),
		  paste0('www/',
		  	     list.files('repo/assessments/mathematics/items', pattern='*.png'))
)
file.copy(paste0('repo/assessments/mathematics/figures/',
				 list.files('repo/assessments/mathematics/figures', pattern='*.png')),
		  paste0('www/',
 		   	     list.files('repo/assessments/mathematics/figures', pattern='*.png'))
)

items <- read.xls('repo/assessments/mathematics/MathItems.xlsx', stringsAsFactors=FALSE, verbose = FALSE)
items <- items[items$Domain != '',]

domains <- unique(items$Domain)
domains <- domains[domains != '']

items$Filename <- paste0(items$State, '-', items$Year, '-',
						 formatC(items$Month, width=2, flag='0'), '-',
						 formatC(items$ItemNum, width=2, flag='0'), '.md')

feedback <- list()
for(i in items$Filename) { # Read the feedback MD files
	if(file.exists(paste0(items.dir, i))) {
		 feedback[[i]] <- paste0(scan(paste0(items.dir, i),
		 							 what = character(),
		 							 sep = '\n',
		 							 blank.lines.skip = FALSE,
		 							 quiet = TRUE),
		 						collapse = '\n')
	}
}

#items <- items[items$Filename %in% names(feedback),]

ui <- fluidPage(
	withMathJax(),

	titlePanel("DAACS Mathematics Item Viewer"),

	fluidRow(
		column(width = 3, selectInput('domain', 'Domain', domains),
			              checkboxInput('itemsWithFeedback', 'Items with feedback only', value=TRUE)),
		column(width = 2, div(actionButton('previousItem', 'Previous')), style='text-align:right'),
		column(width = 3, div(uiOutput('itemSelect')), style='text-align:center'),
		column(width = 2, actionButton('nextItem', 'Next'))
	),
	hr(),
	fluidRow(column(width = 12,
	    	h4(htmlOutput('stem'))
		)
	),
	fluidRow(column(width = 6,
					h5('A) '), htmlOutput('choiceA')),
			 column(width = 6,
			 	    h5('B) '), htmlOutput('choiceB'))
	),
	fluidRow(column(width = 6,
					h5('C) '), htmlOutput('choiceC')),
			 column(width = 6,
			 	    h5('D) '), htmlOutput('choiceD'))
	), hr(),
	fluidRow(column(width = 12,
			h3(htmlOutput('answer')),
			uiOutput('editbuttonui'),
			h3('Feedback:'),
			#uiOutput('feedbackUI'),
	      	htmlOutput('feedback')
		)
	)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
	observeEvent(input$nextItem, {
		choices <- items[items$Domain == input$domain,]
		if(input$itemsWithFeedback) {
			choices <- choices[choices$Filename %in% names(feedback),]
		}
		choices <- choices$Filename
		pos <- which(choices == input$item) + 1
		if(pos > length(choices)) { pos <- 1 }
		updateSelectInput(session, 'item', selected = choices[pos])
	})
	
	observeEvent(input$previousItem, {
		choices <- items[items$Domain == input$domain,]
		if(input$itemsWithFeedback) {
			choices <- choices[choices$Filename %in% names(feedback),]
		}
		choices <- choices$Filename
		pos <- which(choices == input$item) - 1
		if(pos < 1) { pos <- length(choices) }
		updateSelectInput(session, 'item', selected = choices[pos])
	})

	output$itemSelect <- renderUI({
		req(input$domain)
		choices <- items[items$Domain == input$domain,]
		if(input$itemsWithFeedback) {
			choices <- choices[choices$Filename %in% names(feedback),]
		}
		selectInput('item', NULL, choices = choices$Filename)
	})

	output$stem <- renderText({
		html <- items[items$Filename == input$item,]$Stem
		html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
		return(html)
	})

	output$choiceA <- renderText({
		html <- items[items$Filename == input$item,]$A
		html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
		return(html)
	})

	output$choiceB <- renderText({
		html <- items[items$Filename == input$item,]$B
		html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
		return(html)
	})

	output$choiceC <- renderText({
		html <- items[items$Filename == input$item,]$C
		html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
		return(html)
	})

	output$choiceD <- renderText({
		html <- items[items$Filename == input$item,]$D
		html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
		return(html)
	})

	output$answer <- renderText({
		paste0('Answer: ', items[items$Filename == input$item,]$Answer)
	})

	output$feedbackUI <- renderUI({
		req(input$item)
		textAreaInput('feedback', 'Feedback source', value = feedback[[input$item]],
					  width = '600px', height = '200px')
	})

	output$editbuttonui <- renderUI({
		fname <- input$item
		url <- paste0(repo.url, '/edit/master/assessments/mathematics/items/', fname)
		shiny::actionButton(inputId='editbutton', label="Edit Feedback", 
							icon = icon("pencil"), 
							onclick =paste0("window.open('", url, "', '_blank')"))
	})

	output$feedback <- renderText({
		req(input$item)
		# req(input$feedback)
		# html <- markdownToHTML(text = input$feedback, options=c('fragment_only'))
		if(input$item %in% names(feedback)) {
			html <- markdownToHTML(text = feedback[[input$item]], options=c('fragment_only'))
			#if(grep('$$', feedback[[131]], fixed=TRUE) | grep('\\(', feedback[[131]], fixed=TRUE)) {
				html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
			#}
		} else {
			html <- "No feedback found."
		}
		return(html)
	})
}

# Run the application
shinyApp(ui = ui, server = server)

