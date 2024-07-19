Extension { #name : 'RGClassDefinition' }

{ #category : '*Deprecated12' }
RGClassDefinition >> classVarNamed: aString [
	self 
		deprecated: 'Use #classVariableNamed: instead.' 
		transformWith: '`@rcv classVarNamed: `@arg' -> '`@rcv classVariableNamed: `@arg'.

	^self classVariableNamed: aString
]
