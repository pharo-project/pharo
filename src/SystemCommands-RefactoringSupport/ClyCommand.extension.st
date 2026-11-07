Extension { #name : 'ClyCommand' }

{ #category : '*SystemCommands-RefactoringSupport' }
ClyCommand >> executeRefactoring [
	self asRefactorings do: [ :each | each execute ]
]

{ #category : '*SystemCommands-RefactoringSupport' }
ClyCommand >> isComplexRefactoring [
	^false
]
