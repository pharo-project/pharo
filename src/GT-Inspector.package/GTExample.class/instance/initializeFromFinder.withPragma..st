initializing
initializeFromFinder: aFinder withPragma: aPragma
	finder := aFinder.
	pragma := aPragma.
	source := aFinder source.
	provider := aFinder provider.	
	subjects := aFinder subjects.
	exceptions := ExceptionSet new.
	children := OrderedCollection new.
	parents := Set new.
	properties := IdentityDictionary new.
	
	aPragma method pragmas do: [ :p |
		(self class canPerform: p keyword) 
			ifTrue: [ self perform: p keyword withArguments: p arguments ] ].