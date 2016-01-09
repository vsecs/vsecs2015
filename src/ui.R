shinyUI
(
	fluidPage
	(
		titlePanel("A Vertical Search Engine on Cancer Genomics"),

		sidebarLayout
		(
			sidebarPanel
			(
				textInput("kw", label = h3("Keyword"), value = NULL),
				numericInput("pg", label = h3("Page"), value = NA),
				br(),
				actionButton("srh", "Search"),
				br(),
        br(),
				actionButton("prv", "Previous"),
				actionButton("nxt", "Next")
			),

			mainPanel
			(
			  tableOutput("table")
			)
		)
	)
)