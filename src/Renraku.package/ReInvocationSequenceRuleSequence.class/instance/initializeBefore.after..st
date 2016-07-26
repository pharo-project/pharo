initialization
initializeBefore: beforeSymols after: afterSymbols

	before :=
		 (beforeSymols isCollection and:
		[ beforeSymols isString not ])
			ifFalse: [ Array with: beforeSymols asSymbol ]
			ifTrue: [ beforeSymols ].
	after := 
		(afterSymbols isCollection and:
		[ afterSymbols isString not ])
			ifFalse: [ Array with: afterSymbols asSymbol ]
			ifTrue: [ afterSymbols ].