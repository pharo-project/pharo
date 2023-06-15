Extension { #name : #CompiledMethod }

{ #category : #'*Deprecated12' }
CompiledMethod >> category [

	self deprecated: 'Use #protocolName instead.' transformWith: '`@rcv category' -> '`@rcv protocolName'.
	^ self protocolName
]
