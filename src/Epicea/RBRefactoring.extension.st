Extension { #name : #RBRefactoring }

{ #category : #'*Epicea-Refactorings' }
RBRefactoring >> asEpiceaEvent [
	^ (EpGenericRefactoring canAccept: self)
		ifTrue: [ EpGenericRefactoring for: self ]
		ifFalse: [ EpUnknownRefactoring for: self ]
]

{ #category : #'*Epicea-Refactorings' }
RBRefactoring >> propagateTransformation [
	"Perform the *secondary transformation* of the refactoring. Refactorings perform a main 
	transformations and optionally a secondary transformations. 
	For example, in a class rename, the main transformations may be removing the class and adding 
	it with another name. A secondary transformation in this case is fixing the references in 
	methods to the class."
	
	"The default is to do nothing"

	
]
