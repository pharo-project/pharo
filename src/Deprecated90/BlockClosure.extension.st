Extension { #name : #BlockClosure }

{ #category : #'*Deprecated90' }
BlockClosure >> ifError: errorHandlerBlock [

	self deprecated: 'Use #onErrorDo: instead.' transformWith: '`@receiver ifError: `@arg' -> '`@receiver onErrorDo: `@arg'.
	^ self onErrorDo: errorHandlerBlock 
]

{ #category : #'*Deprecated90' }
BlockClosure >> ifErrorDo: errorHandlerBlock [

	self deprecated: 'Use #onErrorDo: instead.' transformWith: '`@receiver ifErrorDo: `@arg' -> '`@receiver onErrorDo: `@arg'.
	^ self onErrorDo: errorHandlerBlock 
]
