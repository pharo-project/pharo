Extension { #name : 'ClyMethodsInProtocolQuery' }

{ #category : '*Deprecated12' }
ClyMethodsInProtocolQuery >> tag [

	self deprecated: 'Use #protocol instead.' transformWith: '`@rcv tag' -> '`@rcv protocol'.
	^ self protocol
]

{ #category : '*Deprecated12' }
ClyMethodsInProtocolQuery >> tag: anObject [

	self deprecated: 'Use #protocol: instead.' transformWith: '`@rcv tag: `@arg' -> '`@rcv protocol: `@arg'.
	self protocol: anObject
]
