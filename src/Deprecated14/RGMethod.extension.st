Extension { #name : 'RGMethod' }

{ #category : '*Deprecated14' }
RGMethod >> isMetaSide [
	self
		deprecated: 'Use isMeta instead'
		transformWith: '`@receiver isMetaSide' -> '`@receiver isMeta'.
	^ self isMeta
]
