utilities
iconFor: project
	^ (Smalltalk globals includesKey: project packageName asSymbol)
		ifTrue: [ self iconNamed: #configLoaded ]
		ifFalse: [ self iconNamed: #config ]