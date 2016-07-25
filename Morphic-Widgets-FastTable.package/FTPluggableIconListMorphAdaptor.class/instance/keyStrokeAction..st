event handling
keyStrokeAction: event
	| result |
	
	result := self model
		perform: self keystrokeSelector
		withEnoughArguments: { event. self }.
		
	^ result = true