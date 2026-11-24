Extension { #name : 'ClyOldMessageBrowserAdapter' }

{ #category : '*Deprecated14' }
ClyOldMessageBrowserAdapter >> messages [

	self
		deprecated: 'Use #methods instead'
		in: #Pharo14
		transformWith: '`@rcv messages' -> '`@rcv methods'.
	^ methods
]

{ #category : '*Deprecated14' }
ClyOldMessageBrowserAdapter >> messages: anObject [

	self deprecated: 'Use #methods: instead' in: #Pharo14 transformWith: '`@rcv messages: `@arg' -> '`@rcv methods: `@arg'.
	methods := anObject
]
