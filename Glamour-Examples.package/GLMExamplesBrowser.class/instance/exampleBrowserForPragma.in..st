private
exampleBrowserForPragma: each in: aClass
	| exampleBrowser wrapperBrowser |
	exampleBrowser := aClass new perform: each selector.

	wrapperBrowser := GLMTabulator new.
	wrapperBrowser column: #theOuterPane.
	wrapperBrowser transmit
		fromOutsideEntityPort; 
		to: #theOuterPane;
		andShow: [:a | 
			a custom: exampleBrowser ].
	^ wrapperBrowser startOn: (Smalltalk compiler evaluate: (each argumentAt: 2) logged: false)