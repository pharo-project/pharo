Extension { #name : #NonInteractiveUIManager }

{ #category : #'*Files-Prompt' }
NonInteractiveUIManager >> fileDoesNotExistsDefaultAction: anException [

	^ self nonInteractive: anException 
]

{ #category : #'*Files-Prompt' }
NonInteractiveUIManager >> fileExistsDefaultAction: anException [

	^ self nonInteractive: anException 
]
