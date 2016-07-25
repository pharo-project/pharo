updating
updateState

	( target perform: stateSelector )
		ifTrue: [ self state: #on ]
		ifFalse:[ self state: #off ]