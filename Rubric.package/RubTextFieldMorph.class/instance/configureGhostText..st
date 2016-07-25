configure
configureGhostText: aTextArea
	super configureGhostText: aTextArea.
	self hasKeyboardFocus
		ifTrue: [ 
			aTextArea left: self left]