Extension { #name : 'FileException' }

{ #category : '*Deprecated14' }
FileException >> fileName: aFileName [

	self deprecated: 'Use #entity: instead' transformWith: '`@rcv fileName: `@arg' -> '`@rcv entity: `@arg'.
	self entity: aFileName
]

{ #category : '*Deprecated14' }
FileException >> reference [

	self deprecated: 'Use #entity instead' transformWith: '`@rcv reference' -> '`@rcv entity'.
	^ self entity
]
