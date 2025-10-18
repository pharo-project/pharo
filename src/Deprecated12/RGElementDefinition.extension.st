Extension { #name : 'RGElementDefinition' }

{ #category : '*Deprecated12' }
RGElementDefinition >> isMetaSide [
	self
		deprecated: 'Use isMeta instead'
		transformWith: '`@receiver isMetaSide' -> '`@receiver isMeta'.
	^ self isMeta
]
