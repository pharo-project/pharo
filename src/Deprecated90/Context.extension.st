Extension { #name : #Context }

{ #category : #'*Deprecated90' }
Context >> lookupSymbol: aSymbol [
	self 
		deprecated: 'use #readVariableNamed:' 
		transformWith: '`@receiver lookupSymbol: `@arg' -> '`@receiver readVariableNamed: `@arg'.
	^ self readVariableNamed: aSymbol
]
