*GT-SpotterExtensions-Core-private
allSendersToString: aString do: aBlock
	aString isEmptyOrNil ifTrue: [ ^ self ].
	aString asSymbol ifNotNil: [ :symbol |
		self allBehaviorsDo: [ :class | 
			class withMethodSender: symbol do: aBlock ] ]