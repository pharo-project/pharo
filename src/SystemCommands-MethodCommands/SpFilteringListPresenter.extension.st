Extension { #name : #SpFilteringListPresenter }

{ #category : #'*SystemCommands-MethodCommands' }
SpFilteringListPresenter >> unfilteredItems [

	^ unfilteredItems ifNil: [ unfilteredItems := self items ]
]
