Extension { #name : #ChangeRecord }

{ #category : #'*Deprecated12' }
ChangeRecord >> category [

	self deprecated: 'Use #protocol instead' transformWith: '`@rcv category' -> '`@rcv protocol'.
	^ protocol
]
