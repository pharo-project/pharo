shout
hasBindingOf: aString
	^ interactionModel ifNil: [ false ] ifNotNil: [ interactionModel hasBindingOf: aString ]