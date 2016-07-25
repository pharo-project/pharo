private
computeNewSelector
	| keywords |
	oldSelector numArgs == 0
		ifTrue: [ self refactoringFailure: 'This method contains no arguments' ].
	oldSelector isInfix
		ifTrue: [ self refactoringFailure: 'Cannot remove parameters of infix selectors' ].
	keywords := oldSelector keywords asOrderedCollection.
	keywords size = 1
		ifTrue: [ ^ (keywords first copyWithout: $:) asSymbol ].
	keywords removeAt: parameterIndex.
	^ (String streamContents: [ :str | keywords do: [ :each | str nextPutAll: each ] ]) asSymbol