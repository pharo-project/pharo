Extension { #name : 'MethodChunk' }

{ #category : '*Deprecated12' }
MethodChunk >> category [

	self deprecated: 'Use #protocol instead' transformWith: '`@rcv category' -> '`@rcv protocol'.
	^ protocol
]

{ #category : '*Deprecated12' }
MethodChunk >> category: aProtocol [

	self deprecated: 'Use #protocol: instead' transformWith: '`@rcv category: `@arg' -> '`@rcv protocol: `@arg'.
	^ protocol := aProtocol
]
