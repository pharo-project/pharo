Extension { #name : 'ReTransformation' }

{ #category : '*Deprecated14' }
ReTransformation >> asRefactoring [

	self deprecated: 'This method is a no-op and can be removed' transformWith: '`@rec asRefactoring' -> '`@rec'.
	^ self yourself
]

{ #category : '*Deprecated14' }
ReTransformation >> transform [

	self deprecated: 'Use generateChanges instead' transformWith: '`@rec transform' -> '`@rec generateChanges'.
	
	"Performs this transformation in a model,
	 then it adds the refactoring to the manager"

	self checkPreconditions.
	self privateTransform

	"below executes the refactoring without prompt"
	"RBRefactoringManager instance addRefactoring: self"
]
