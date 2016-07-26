label-events
onTextAccepted: ann
	|isChanged|
	
	isChanged := (self text asString = ann morph text asString) not.
	ann morph text isEmpty
		ifFalse: [ self accept ]
		ifTrue: [ self cancel ].
	isChanged ifTrue: [ self announce: #onAccepted ]