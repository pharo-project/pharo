Extension { #name : #UIManager }

{ #category : #'*Versionner-Core-Model' }
UIManager >> multilineRequest: request initialAnswer: initial [
	
	^ self multiLineRequest: request initialAnswer: initial answerHeight: 200
]
