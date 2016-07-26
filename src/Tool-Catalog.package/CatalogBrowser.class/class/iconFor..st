utilities
iconFor: project
	^ (Smalltalk includesKey: project packageName asSymbol)
		ifTrue: [ Smalltalk ui icons iconNamed: #configIconLoaded ]
		ifFalse: [ Smalltalk ui icons iconNamed: #configIcon ]