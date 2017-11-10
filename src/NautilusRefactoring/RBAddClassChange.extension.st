Extension { #name : #RBAddClassChange }

{ #category : #'*NautilusRefactoring' }
RBAddClassChange >> nameToDisplay [

	^ self changeString
]

{ #category : #'*NautilusRefactoring' }
RBAddClassChange >> textToDisplay [

	^ self definition
]
