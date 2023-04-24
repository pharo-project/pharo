Extension { #name : #ProtocolRenamed }

{ #category : #'*Deprecated12' }
ProtocolRenamed >> newProtocolName [

	self deprecated: 'Use #newProtocol instead' transformWith: '`@rcv newProtocolName' -> '`@rcv newProtocol'.
	^ self newProtocol
]

{ #category : #'*Deprecated12' }
ProtocolRenamed >> oldProtocolName [

	self deprecated: 'Use #oldProtocol instead' transformWith: '`@rcv oldProtocolName' -> '`@rcv oldProtocol'.
	^ self oldProtocol
]
