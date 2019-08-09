Extension { #name : #CmdCommand }

{ #category : #'*SystemCommands-RefactoringSupport' }
CmdCommand >> confirmRefactoringInContext: aToolContext by: aCommandActivator [
	| submitDialog |
	submitDialog := SycRefactoringPreview
		for: self
		scopes: aToolContext refactoringScopes.
	submitDialog existChanges ifTrue:[
	submitDialog openModalWithSpec
		okAction: [ submitDialog accept.
			aCommandActivator applyCommandResult ];
		cancelAction: [ submitDialog cancel ]]
	ifFalse: [ submitDialog alert: 'There is no change produced by this refactoring' ].
	CmdCommandAborted signal
]

{ #category : #'*SystemCommands-RefactoringSupport' }
CmdCommand >> executeRefactorings [
	self asRefactorings do: [ :each | each execute ]
]

{ #category : #'*SystemCommands-RefactoringSupport' }
CmdCommand >> isComplexRefactoring [
	^false
]
