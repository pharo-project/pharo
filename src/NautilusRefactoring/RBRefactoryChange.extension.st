Extension { #name : #RBRefactoryChange }

{ #category : #'*NautilusRefactoring' }
RBRefactoryChange >> accept: aText notifying: aController [
	^ false
]

{ #category : #'*NautilusRefactoring' }
RBRefactoryChange >> nameToDisplay [

	^ self name
]

{ #category : #'*NautilusRefactoring' }
RBRefactoryChange >> oldVersionTextToDisplay [

	^ ''
]

{ #category : #'*NautilusRefactoring' }
RBRefactoryChange >> textToDisplay [

	^ self name
]

{ #category : #'*NautilusRefactoring' }
RBRefactoryChange >> whatToDisplayIn: aChangeBrowser [

	^ { self }
]
