Extension { #name : 'EFContext' }

{ #category : '*Deprecated14' }
EFContext >> periodsAtEndOfBlock [

	self
		deprecated: 'Use #periodAtEndOfBlock instead.'
		in: #Pharo14
		transformWith: '`@rcv periodsAtEndOfBlock' -> '`@rcv periodAtEndOfBlock'.
	^ self periodAtEndOfBlock
]

{ #category : '*Deprecated14' }
EFContext >> periodsAtEndOfMethod [

	self
		deprecated: 'Use #periodAtEndOfMethod instead.'
		in: #Pharo14
		transformWith: '`@rcv periodsAtEndOfMethod' -> '`@rcv periodAtEndOfMethod'.
	^ self periodAtEndOfMethod
]
