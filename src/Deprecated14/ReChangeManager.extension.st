Extension { #name : 'ReChangeManager' }

{ #category : '*Deprecated14' }
ReChangeManager >> performChange: aRefactoringChange [

	self
		deprecated: 'Use `performCompositeChange:` if you have CompositeChange or use `performChanges:` if you have a list of changes instead of current one'
		transformWith: '`@rec performChange: `@arg' -> '`@rec performCompositeChange: `@arg'.

	self performCompositeChange: aRefactoringChange.
]

{ #category : '*Deprecated14' }
ReChangeManager class >> undoLastRefactoring [

	self deprecated: 'Use instance side method instead'
		transformWith: '`@rec undoLastRefactoring' -> '`@rec instance undoLastRefactoring'.
	^ self instance undoLastRefactoring 
]
