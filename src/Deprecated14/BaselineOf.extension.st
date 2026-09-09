Extension { #name : 'BaselineOf' }

{ #category : '*Deprecated14' }
BaselineOf >> os [

	self deprecated: 'Inline directly this method' transformWith: '`@rcv os' -> 'OSPlatform current'.
	^ OSPlatform current
]
