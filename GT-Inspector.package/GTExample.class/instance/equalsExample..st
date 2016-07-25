testing
equalsExample: anExampleOrExampleProxy
	^ anExampleOrExampleProxy isGTExample
		ifTrue: [ self class = anExampleOrExampleProxy class and: [ self = anExampleOrExampleProxy ] ]
		ifFalse: [ anExampleOrExampleProxy equalsExample: self ]