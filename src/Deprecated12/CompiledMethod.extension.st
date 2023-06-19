Extension { #name : #CompiledMethod }

{ #category : #'*Deprecated12' }
CompiledMethod >> tagWith: aSymbol [

	self deprecated: 'Use #protocol: instead.' transformWith: '`@rcv tagWith: `@arg' -> '`@rcv protocol: `@arg'.
	self protocol: aSymbol
]
