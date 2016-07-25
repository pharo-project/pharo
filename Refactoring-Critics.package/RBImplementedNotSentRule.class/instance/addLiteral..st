private
addLiteral: aLiteral

	aLiteral isSymbol
		ifTrue: [ literals add: aLiteral ]
		ifFalse: [
			aLiteral class == Array ifTrue: [
				aLiteral do: [ :each |
					self addLiteral: each ] ] ]