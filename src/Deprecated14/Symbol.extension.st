Extension { #name : 'Symbol' }

{ #category : '*Deprecated14' }
Symbol >> isSelectorSymbol [

	self deprecated:  'Use isUsedAsSelectorSymbol'
		on: '02/02/2025' 
		in:  'pharo13'
		transformWith: 
			'`@rec isSelectorSymbol' -> '`@rec isUsedAsSelectorSymbol'.

	^ self isUsedAsSelectorSymbol
]
