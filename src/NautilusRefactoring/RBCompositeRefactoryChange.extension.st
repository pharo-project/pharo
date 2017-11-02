Extension { #name : #RBCompositeRefactoryChange }

{ #category : #'*NautilusRefactoring' }
RBCompositeRefactoryChange >> whatToDisplayIn: aBrowser [

	^ self changes flatCollect: [:each | each whatToDisplayIn: aBrowser ]
]
