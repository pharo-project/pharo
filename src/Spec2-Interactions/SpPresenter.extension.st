Extension { #name : #SpPresenter }

{ #category : #'*Spec2-Interactions' }
SpPresenter >> confirm: queryString label: titleString [
	^ UIManager default 
		confirm: queryString label: titleString
]

{ #category : #'*Spec2-Interactions' }
SpPresenter >> request: request initialAnswer: initialAnwser title: title [
	| answer |
	answer := UIManager default 
					request: request
					initialAnswer: initialAnwser
					title: title.
					
	answer "If this is nil, it means that user cancelled the UI."
		ifNil: [ SpInteractionCancelled signal ].
	
	^ answer
]
