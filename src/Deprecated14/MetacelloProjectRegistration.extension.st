Extension { #name : 'MetacelloProjectRegistration' }

{ #category : '*Deprecated14' }
MetacelloProjectRegistration >> baselineProjectSpec [

	self deprecated: 'Use #projectSpec instead.' transformWith: '`@rcv baselineProjectSpec' -> '`@rcv projectSpec'.
	^ self projectSpec
]

{ #category : '*Deprecated14' }
MetacelloProjectRegistration class >> baselineProjectSpecs [

	self deprecated: 'Use #projectSpecs instead.' transformWith: '`@rcv baselineProjectSpecs' -> '`@rcv projectSpecs'.
	^ self projectSpecs
]
