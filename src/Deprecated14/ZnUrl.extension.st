Extension { #name : 'ZnUrl' }

{ #category : '*Deprecated14' }
ZnUrl >> asUrl [
	self
		deprecated: 'Use asZnUrl instead'
		in: #Pharo14
		transformWith: '`@rcv asUrl' -> '`@rcv asZnUrl'.
	^ self
]
