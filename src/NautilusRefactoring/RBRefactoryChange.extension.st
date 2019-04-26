Extension { #name : #RBRefactoryChange }

{ #category : #'*NautilusRefactoring' }
RBRefactoryChange >> accept: aText notifying: aController [
	^ false
]

{ #category : #'*NautilusRefactoring' }
RBRefactoryChange >> classNamed: aClassName [
	^self class environment classNamed: aClassName
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
