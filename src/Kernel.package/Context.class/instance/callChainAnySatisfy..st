testing
callChainAnySatisfy: aBlock

	(aBlock value: self) ifTrue: [ ^ true ].
	self sender ifNil: [ ^ false ].
	^ self sender callChainAnySatisfy: aBlock.