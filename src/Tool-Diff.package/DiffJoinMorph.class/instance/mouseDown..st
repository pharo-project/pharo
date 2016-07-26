event handling
mouseDown: evt
	"Check for a click."

	self mappings
		detect: [ :j | j containsPoint: evt position - self topLeft ]
		ifFound: [ :cj | 
			cj clicked.
			self triggerEvent: #joinClicked ].
	super mouseDown: evt