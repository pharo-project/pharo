Extension { #name : 'ZnUrl' }

{ #category : '*Deprecated14' }
ZnUrl >> asUrl [
	self deprecated: 'Use asZnUrl instead' transformWith: '`@rcv asUrl' -> '`@rcv asZnUrl'.
	^ self
]
