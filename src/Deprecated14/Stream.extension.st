Extension { #name : 'Stream' }

{ #category : '*Deprecated14' }
Stream >> basicNext [

	self deprecated: 'Use #next' transformWith: '`@rcv basicNext' -> '`@rcv next'.
	^ self next
]

{ #category : '*Deprecated14' }
Stream >> basicNext: anAmount putAll: aCollection startingAt: startIndex [

	self
		deprecated: 'Use #next:putAll:startingAt: instead'
		transformWith: '`@rcv basicNext: `@arg1 putAll: `@arg2 startingAt: `@arg3' -> '`@rcv next: `@arg1 putAll: `@arg2 startingAt: `@arg3'.
	^ self next: anAmount putAll: aCollection startingAt: startIndex
]

{ #category : '*Deprecated14' }
Stream >> basicNextPut: anObject [

	self deprecated: 'Use #nextPut:' transformWith: '`@rcv basicNextPut: `@arg1' -> '`@rcv nextPut: `@arg1'.
	^ self nextPut: anObject
]

{ #category : '*Deprecated14' }
Stream >> basicNextPutAll: aCollection [

	self deprecated: 'Use #nextPutAll:' transformWith: '`@rcv basicNextPutAll: `@arg1' -> '`@rcv nextPutAll: `@arg1'.
	^ self nextPutAll: aCollection
]
