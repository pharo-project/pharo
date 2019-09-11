Extension { #name : #UIManager }

{ #category : #'*Files-Prompt' }
UIManager >> fileDoesNotExistsDefaultAction: anException [

	^self subclassResponsibility 
]

{ #category : #'*Files-Prompt' }
UIManager >> fileExistsDefaultAction: anException [

	^self subclassResponsibility 
]
