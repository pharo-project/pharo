Extension { #name : 'MetacelloProjectRegistry' }

{ #category : '*Deprecated14' }
MetacelloProjectRegistry >> baselineProjectSpecs [

	self deprecated: 'Use #projects instead.' transformWith: '`@rcv baselineProjectSpecs' -> '`@rcv projects'.
	^ self projects
]

{ #category : '*Deprecated14' }
MetacelloProjectRegistry >> baselineRegistry [

	self deprecated: 'Use #projects instead.' transformWith: '`@rcv baselineProjectSpecs' -> '`@rcv projects'.
	^ self projects
]

{ #category : '*Deprecated14' }
MetacelloProjectRegistry >> projectSpecs [

	self deprecated: 'Use #projects instead' transformWith: '`@rcv projectSpecs' -> '`@rcv project'.
	^ self projects
]
