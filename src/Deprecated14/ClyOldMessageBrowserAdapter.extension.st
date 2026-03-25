Extension { #name : 'ClyOldMessageBrowserAdapter' }

{ #category : '*Deprecated14' }
ClyOldMessageBrowserAdapter >> messages [

	self deprecated: 'Use #methods instead' transformWith: '`@rcv messages' -> '`@rcv methods'.
	^ methods
]

{ #category : '*Deprecated14' }
ClyOldMessageBrowserAdapter >> messages: anObject [

	self deprecated: 'Use #messages: instead' transformWith: '`@rcv messages: `@arg' -> '`@rcv methods: `@arg'.
	methods := anObject
]
