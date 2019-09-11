Extension { #name : #RBRefactoryChange }

{ #category : #'*NautilusRefactoring' }
RBRefactoryChange >> accept: aText notifying: aController [
	^ false
]

{ #category : #'*NautilusRefactoring' }
RBRefactoryChange >> whatToDisplayIn: aChangeBrowser [

	^ { self }
]
