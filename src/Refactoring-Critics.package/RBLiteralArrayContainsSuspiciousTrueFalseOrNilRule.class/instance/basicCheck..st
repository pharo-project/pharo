running
basicCheck: aMethod 
	| compiledLits parsedLits |
	compiledLits := aMethod allLiterals
		inject: OrderedCollection new
		into: [ :collection :literal |
			collection
				addAll: (self literalTrueFalseOrNilSymbolsIn: literal);
				yourself ].
	compiledLits size strictlyPositive ifTrue: [ 
		parsedLits := OrderedCollection new.
		matcher 
			executeTree: aMethod ast
			initialAnswer: parsedLits.
		^ compiledLits size ~= parsedLits size ].
	
	^ false