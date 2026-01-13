Extension { #name : 'FileException' }

{ #category : '*Deprecated14-Kernel' }
FileException >> fileName: aFileName [

	self deprecated: 'Use #entity: instead' transformWith: '`@rcv fileName: `@arg' -> '`@rcv entity: `@arg'.
	self entity: aFileName
]
