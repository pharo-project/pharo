Extension { #name : 'RBBrowserEnvironment' }

{ #category : '*Deprecated13' }
RBBrowserEnvironment >> classesAndTraits [

	self deprecated: 'Use #classes instead since it also returns the traits.' transformWith: '`@rcv classesAndTraits' -> '`@rcv classes'.
	^ self classes
]

{ #category : '*Deprecated13' }
RBBrowserEnvironment >> classesAndTraitsDo: aBlock [

	self deprecated: 'Use #classesDo: instead' transformWith: '`@rcv classesAndTraitsDo: `@arg' -> '`@rcv classesDo: `@arg'.
	self classesDo: aBlock
]
