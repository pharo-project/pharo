commands
default
	Smalltalk isHeadless 
		ifFalse: [ ^ self noQuit ].
	
	^ super default