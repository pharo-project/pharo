utilities
iconFor: project
	^ (Smalltalk includesKey: project packageName asSymbol)
		ifTrue: [ self iconNamed: #configIconLoaded ]
		ifFalse: [ self iconNamed: #configIcon ]