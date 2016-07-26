model access
keystrokeAction: event 
	| returnValue |
	
	keystrokeSelector ifNil: [ ^ nil ].

	returnValue := model 
						perform: keystrokeSelector
						withEnoughArguments: { event. self }.
	
	^ returnValue = true