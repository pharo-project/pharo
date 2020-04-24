Extension { #name : #BlockClosure }

{ #category : #'*Deprecated90' }
BlockClosure >> ifError: errorHandlerBlock [

	"This method is called with a block expecting description and receiver as arguments"
	"This method is deprecated in favor of #onErrorDo: that takes the exception object instead of its parts."
	self deprecated: 'Use #onErrorDo: instead.'.
		
	^ self on: Error do: [:ex |
		errorHandlerBlock cull: ex description cull: ex receiver ]
]

{ #category : #'*Deprecated90' }
BlockClosure >> ifErrorDo: errorHandlerBlock [
 
	"This method is called with a block expecting description and receiver as arguments"
	"This method is deprecated in favor of #onErrorDo: that takes the exception object instead of its parts."
	self deprecated: 'Use #onErrorDo: instead.'.

	^ self on: Error do: [:ex |
		errorHandlerBlock cull: ex description cull: ex receiver ]
]
