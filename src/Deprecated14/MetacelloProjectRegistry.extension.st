Extension { #name : 'MetacelloProjectRegistry' }

{ #category : '*Deprecated14' }
MetacelloProjectRegistry >> baselineProjectSpecs [

	self deprecated: 'Use #projectSpecs instead.' transformWith: '`@rcv baselineProjectSpecs' -> '`@rcv projectSpecs'.
	^ self projectSpecs
]

{ #category : '*Deprecated14' }
MetacelloProjectRegistry >> baselineRegistry [

	self deprecated: 'Use #projectSpecs instead.' transformWith: '`@rcv baselineProjectSpecs' -> '`@rcv projectSpecs'.
	^ self projectSpecsRegistry
]
