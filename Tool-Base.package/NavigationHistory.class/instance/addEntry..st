protocol
addEntry: anEntry
	
	self isPaused ifTrue: [ ^ self ].
	
	self basicAdd: anEntry