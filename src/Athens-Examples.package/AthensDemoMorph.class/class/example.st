examples
example
	"self example"
	
	| demo model window |
	demo := AthensDemoMorph new openInWorld. 
	(model := DynamicComposablePresenter new)
		instantiatePresenters: #(backButton ButtonPresenter forwardButton ButtonPresenter).
	model backButton label: 'Previous Figure'; action: [ demo prevFigure ].
	model forwardButton label: 'Next Figure'; action: [ demo nextFigure ].
	window := model openWithSpecLayout: (SpecLayout composed
 				   newColumn: [: c | c add: #backButton. c add: #forwardButton ];
    				   yourself).
	window title: 'Athens demos'