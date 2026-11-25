Extension { #name : 'RGMethod' }

{ #category : '*Deprecated14' }
RGMethod >> isMetaSide [
	self
		deprecated: 'Use isMeta instead'
		in: #Pharo14
		transformWith: '`@receiver isMetaSide' -> '`@receiver isMeta'.
	^ self isMeta
]
