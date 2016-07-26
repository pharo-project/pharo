widget API
stateSelector

	^ self state
		ifNil: [ nil ]
		ifNotNil: [ #state ]