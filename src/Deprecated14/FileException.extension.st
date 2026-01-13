Extension { #name : 'FileException' }

{ #category : '*Deprecated14' }
FileException >> reference [

	self deprecated: 'Use #entity instead' transformWith: '`@rcv reference' -> '`@rcv entity'.
	^ self entity
]
