Extension { #name : 'OSPlatform' }

{ #category : '*Deprecated14' }
OSPlatform >> environment [

	self deprecated: 'Use `OSEnvironment current` instead'.
	^ OSEnvironment current
]
