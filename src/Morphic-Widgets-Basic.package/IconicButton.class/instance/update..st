updating
update: aSelector
	
	aSelector = self interactSelector
		ifTrue: [ ^ self doButtonAction ].
		
	super update: aSelector