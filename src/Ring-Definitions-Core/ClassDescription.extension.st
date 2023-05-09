Extension { #name : #ClassDescription }

{ #category : #'*Ring-Definitions-Core' }
ClassDescription >> protocols [

	self
		deprecated: 'Use #protocolNames instead because I should return real protocols and not just names.'
		transformWith: '`@rcv protocols' -> '`@rcv protocolNames'.
	^ self organization protocolNames copy
]
