Extension { #name : #CompiledMethod }

{ #category : #'*Deprecated12' }
CompiledMethod >> tagWith: aSymbol [

	self deprecated: 'Use #protocol: instead.' transformWith: '`@rcv tagWith: `@arg' -> '`@rcv protocol: `@arg'.
	self protocol: aSymbol
]

{ #category : #'*Deprecated12' }
CompiledMethod >> untagFrom: aSymbol [

	self
		deprecated:
		'This method is missleading letting the user thing that there can be more than one protocol on a method. Most of the cases should just use #protocol: to update the protocol or #unclassify.'
		transformWith: '`@rcv untagFrom: `@arg' -> '`@rcv protocolName = `@arg ifTrue: [ `@rcv unclassify ]'.
	self protocolName = aSymbol ifTrue: [ self unclassify ]
]
