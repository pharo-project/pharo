Extension { #name : 'FileException' }

{ #category : '*Deprecated14-Kernel' }
FileException >> fileName: aFileName [

	self
		deprecated: 'Use #entity: instead'
		in: #Pharo14
		transformWith: '`@rcv fileName: `@arg' -> '`@rcv entity: `@arg'.
	self entity: aFileName
]
