Extension { #name : 'BaselineOf' }

{ #category : '*Deprecated14' }
BaselineOf >> os [

	self
		deprecated: 'Inline directly this method'
		in: #Pharo14
		transformWith: '`@rcv os' -> 'OSPlatform current'.
	^ OSPlatform current
]
