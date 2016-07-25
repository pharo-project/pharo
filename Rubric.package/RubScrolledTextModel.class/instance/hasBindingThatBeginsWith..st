shout
hasBindingThatBeginsWith: aString 
	^ interactionModel ifNil: [ false ] ifNotNil:  [ interactionModel hasBindingThatBeginsWith: aString  ]