*GT-SpotterExtensions-Core-private
allReferencesTo: aLiteral do: aBlock
	| symbol |
	symbol := aLiteral isSymbol.
	self allBehaviorsDo: [ :class |
		symbol
			ifFalse: [ class withMethodsReferTo: aLiteral do: aBlock ]
			ifTrue: [ class withThorougMethodsReferTo: aLiteral do: aBlock ] ]