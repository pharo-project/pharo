literals
literalStrings
	"Return a list of strings or symbols corresponding to the class references and message sent.
	The result also contains the name of the class"
	
	| litStrs |
	litStrs := OrderedCollection new: self numLiterals.
	self literalsDo:
		[:lit | 
		(lit isVariableBinding)
			ifTrue: [litStrs addLast: lit key]
			ifFalse: [(lit isSymbol)
				ifTrue: [litStrs addAll: lit keywords]
				ifFalse: [litStrs addLast: lit printString]]].
	^ litStrs