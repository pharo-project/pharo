Extension { #name : #SpFilteringListPresenter }

{ #category : #'*SystemCommands-RefactoringSupport' }
SpFilteringListPresenter >> unfilteredItems [
	^ unfilteredItems ifNil: [ unfilteredItems := self items ].
]
