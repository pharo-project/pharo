commands
add: modelGetter withSpec: aSymbol 
	elementAdded 
		ifTrue: [ Error signal: ('{0} can only hold 1 element' format: { self class name }) ]
		ifFalse: [ super add: modelGetter withSpec: aSymbol. elementAdded := true. ]