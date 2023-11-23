Extension { #name : 'RGBehavior' }

{ #category : '*Deprecated12' }
RGBehavior >> addMethodTag: aSymbol [

	self deprecated: 'Use #addProtocol: instead.' transformWith: '`@rcv addMethodTag: `@arg' -> '`@rcv addProtocol: `@arg'.

	self addProtocol: aSymbol
]

{ #category : '*Deprecated12' }
RGBehavior >> isDefinedInPackage: aPackage [

	self
		deprecated: 'This method will be removed because it is really specific and easy to inline. Use `receiver package = argument` instead.'
		transformWith: '`@rcv isDefinedInPackage: `@package' -> '`@rcv package = `@package'.
	^ self package = aPackage
]
