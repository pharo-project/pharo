Extension { #name : 'Stream' }

{ #category : '*Deprecated14-Kernel' }
Stream >> basicNext [

	self
		deprecated: 'Use #next'
		in: #Pharo14
		transformWith: '`@rcv basicNext' -> '`@rcv next'.
	^ self next
]

{ #category : '*Deprecated14-Kernel' }
Stream >> basicNext: anAmount putAll: aCollection startingAt: startIndex [

	self
		deprecated: 'Use #next:putAll:startingAt: instead'
		in: #Pharo14
		transformWith: '`@rcv basicNext: `@arg1 putAll: `@arg2 startingAt: `@arg3' -> '`@rcv next: `@arg1 putAll: `@arg2 startingAt: `@arg3'.
	^ self next: anAmount putAll: aCollection startingAt: startIndex
]

{ #category : '*Deprecated14-Kernel' }
Stream >> basicNextPut: anObject [

	self
		deprecated: 'Use #nextPut:'
		in: #Pharo14
		transformWith: '`@rcv basicNextPut: `@arg1' -> '`@rcv nextPut: `@arg1'.
	^ self nextPut: anObject
]

{ #category : '*Deprecated14-Kernel' }
Stream >> basicNextPutAll: aCollection [

	self
		deprecated: 'Use #nextPutAll:'
		in: #Pharo14
		transformWith: '`@rcv basicNextPutAll: `@arg1' -> '`@rcv nextPutAll: `@arg1'.
	^ self nextPutAll: aCollection
]
