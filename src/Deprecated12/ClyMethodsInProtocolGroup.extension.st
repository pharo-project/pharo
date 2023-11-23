Extension { #name : 'ClyMethodsInProtocolGroup' }

{ #category : '*Deprecated12' }
ClyMethodsInProtocolGroup >> tag [

	self deprecated: 'Use #protocol instead' transformWith: '`@rcv tag' -> '`@rcv protocol'.
	^ self protocol
]
