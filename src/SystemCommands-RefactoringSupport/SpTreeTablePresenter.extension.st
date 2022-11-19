Extension { #name : #SpTreeTablePresenter }

{ #category : #'*SystemCommands-RefactoringSupport-Scope' }
SpTreeTablePresenter >> collapseSelectedItems [

	self selection selectedPaths do: [ :path | self collapsePath: path ]
]

{ #category : #'*SystemCommands-RefactoringSupport-Scope' }
SpTreeTablePresenter >> expandSelectedItems [
	self selection selectedPaths do: [ :path | self expandPath: path ]
]

{ #category : #'*SystemCommands-RefactoringSupport' }
SpTreeTablePresenter >> togglePath: aPath [ 
	self withAdapterPerformOrDefer: [ :anAdapter |
		anAdapter togglePath: aPath ]
]

{ #category : #'*SystemCommands-RefactoringSupport-Scope' }
SpTreeTablePresenter >> toggleSelectedItems [
	self selection selectedPaths do: [ :path | self togglePath: path ]
]
