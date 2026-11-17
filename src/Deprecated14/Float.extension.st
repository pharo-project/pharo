Extension { #name : 'Float' }

{ #category : '*Deprecated14' }
Float class >> epsilon [

	self
		deprecated: 'Use machineEpsilon instead'
		in: #Pharo14
		transformWith: '`@receiver epsilon' -> '`@receiver machineEpsilon'.
	^ self machineEpsilon
]
