Extension { #name : #RGBehavior }

{ #category : #'*Deprecated12' }
RGBehavior >> addMethodTag: aSymbol [

	self deprecated: 'Use #addProtocol: instead.' transformWith: '`@rcv addMethodTag: `@arg' -> '`@rcv addProtocol: `@arg'.

	self addProtocol: aSymbol
]
