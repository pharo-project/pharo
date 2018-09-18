Extension { #name : #AbstractTool }

{ #category : #'*necompletion-extensions' }
AbstractTool >> guessTypeForName: aString [
	^nil
]

{ #category : #'*NECompletion' }
AbstractTool >> isCodeCompletionAllowed [ 
	^true
]
