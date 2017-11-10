Extension { #name : #AbstractTool }

{ #category : #'*necompletion-extensions' }
AbstractTool >> guessTypeForName: aString [
	^nil
]

{ #category : #'*necompletion-extensions' }
AbstractTool >> isCodeCompletionAllowed [ 
	^true
]
