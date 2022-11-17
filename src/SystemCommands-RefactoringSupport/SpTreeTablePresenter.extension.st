Extension { #name : #SpTreeTablePresenter }

{ #category : #'*SystemCommands-RefactoringSupport-Scope' }
SpTreeTablePresenter >> expandSelectedItems [
	self selection selectedPaths do: [ :path | self expandPath: path ]
]
