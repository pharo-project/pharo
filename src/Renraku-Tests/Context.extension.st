Extension { #name : 'Context' }

{ #category : '*Renraku-Tests' }
Context >> callChainAnySatisfy: aBlock [

	(aBlock value: self) ifTrue: [ ^ true ].
	self sender ifNil: [ ^ false ].
	^ self sender callChainAnySatisfy: aBlock.
]
