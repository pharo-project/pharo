event-listening
logClassRemoved: annoucement
	annoucement classRemoved acceptsLoggingOfCompilation ifTrue: [
		self logChange: 'Smalltalk globals removeClassNamed: #', annoucement classRemoved name	
	].
