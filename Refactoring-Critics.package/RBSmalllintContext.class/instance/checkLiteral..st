private
checkLiteral: aLiteral 
	(aLiteral isSymbol or: [aLiteral isVariableBinding])
		ifTrue: [literals add: aLiteral]
		ifFalse: 
			[aLiteral class == Array
				ifTrue: [aLiteral do: [:each | self checkLiteral: each]]]