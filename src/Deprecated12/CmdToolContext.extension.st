Extension { #name : 'CmdToolContext' }

{ #category : '*Deprecated12' }
CmdToolContext >> requestSingleMethodTag: queryTitle [

	self deprecated: 'Use #requestProtocol: instead' transformWith: '`@rcv requestSingleMethodTag: `@arg' -> '`@rcv requestProtocol: `@arg'.
	^ self requestProtocol: queryTitle
]

{ #category : '*Deprecated12' }
CmdToolContext >> requestSingleMethodTag: queryTitle suggesting: suggestedTag [

	self
		deprecated: 'Use #requestProtocol:suggesting: instead'
		transformWith: '`@rcv requestSingleMethodTag: `@arg1 suggesting: `@arg2' -> '`@rcv requestProtocol: `@arg1 suggesting: `@arg2'.
	^ self requestProtocol: queryTitle suggesting: suggestedTag
]
