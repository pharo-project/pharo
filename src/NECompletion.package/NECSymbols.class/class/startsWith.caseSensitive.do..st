query
startsWith: aChar caseSensitive: aBoolean do: aBlock 
	| char caseInSensitive firstChar |
	caseInSensitive := aBoolean not.
	firstChar := caseInSensitive 
		ifTrue: [ aChar asLowercase ]
		ifFalse: [ aChar ].
	Symbol allSymbolTablesDo: 
		[ :each | 
		| size |
		size := each size.
		char := size > 0 ifTrue: [ each first ].
		(char notNil 
			and: [ (char == firstChar or: [ caseInSensitive and: [ char asLowercase == firstChar ] ])
			and: [ (each 
				findAnySubstring: '- '
				startingAt: 2) > size ] ]) ifTrue: [ aBlock value: each ] ]