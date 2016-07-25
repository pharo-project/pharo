event-listening
logMethodRemoved: announcement
	announcement methodClass acceptsLoggingOfCompilation ifTrue: [
		self logChange: 
			announcement methodClass name, ' removeSelector: #', announcement selector
	]
	