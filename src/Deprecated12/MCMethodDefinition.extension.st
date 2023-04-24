Extension { #name : #MCMethodDefinition }

{ #category : #'*Deprecated12' }
MCMethodDefinition >> category [

	self deprecated: 'Use #protocol instead.' transformWith: '`@rcv category' -> '`@rcv protocol'.
	^ self protocol
]
