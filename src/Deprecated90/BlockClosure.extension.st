Extension { #name : #BlockClosure }

{ #category : #'*Deprecated90' }
BlockClosure >> ifError: errorHandlerBlock [

	errorHandlerBlock numArgs = 0 
		ifTrue: [ self deprecated: 'Use #onErrorDo: instead.' transformWith: '`@receiver ifError: `@arg' -> '`@receiver onErrorDo: `@arg' ]
		ifFalse: [ self deprecated: 'Use #onErrorDo: instead.' ].
		
	^ self on: Error do: [:ex |
		errorHandlerBlock cull: ex]
]

{ #category : #'*Deprecated90' }
BlockClosure >> ifErrorDo: errorHandlerBlock [
 
	errorHandlerBlock numArgs = 0 
		ifTrue: [ self deprecated: 'Use #onErrorDo: instead.' transformWith: '`@receiver ifErrorDo: `@arg' -> '`@receiver onErrorDo: `@arg' ]
		ifFalse: [ self deprecated: 'Use #onErrorDo: instead.' ].

	^ self on: Error do: [:ex |
		errorHandlerBlock cull: ex]
]
