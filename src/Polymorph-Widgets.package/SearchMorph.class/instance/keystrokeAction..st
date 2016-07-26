event handling
keystrokeAction: event 
	| args returnValue |
	keystrokeSelector ifNil: [ ^ nil ].
	args := keystrokeSelector numArgs.
	args = 1 ifTrue: [returnValue := model perform: keystrokeSelector with: event].
	args = 2 
		ifTrue: 
			[returnValue := model 
				perform: keystrokeSelector
				with: event
				with: self].
	returnValue ifNotNil: [
			^ returnValue = true ].
	
	self error: 'keystrokeSelector must be a 1- or 2-keyword symbol'.
	^ false