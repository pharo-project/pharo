private
interestingSymbolsDo: aBlock

	Symbol allSymbolTablesDo:[:symbol |
		symbol isEmpty
			ifFalse: [
				(symbol first isUppercase and:[ Smalltalk globals includesKey: symbol ])
					ifTrue:[ aBlock value: symbol ]
					ifFalse: [ symbol isSelectorSymbol ifTrue: [ aBlock value: symbol ]]]].