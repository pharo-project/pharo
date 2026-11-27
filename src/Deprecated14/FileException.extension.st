Extension { #name : 'FileException' }

{ #category : '*Deprecated14' }
FileException >> reference [

	self
		deprecated: 'Use #entity instead'
		in: #Pharo14
		transformWith: '`@rcv reference' -> '`@rcv entity'.
	^ self entity
]
