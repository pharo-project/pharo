Extension { #name : 'Float' }

{ #category : '*Deprecated14' }
Float class >> epsilon [

	self
		deprecated: 'Use machineEpsilon instead'
		transformWith: '`@receiver epsilon' -> '`@receiver machineEpsilon'.
	^ self machineEpsilon
]
