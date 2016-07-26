tests
testAllSelectionActionsWithNoSelection
	| listing |
	listing := GLMPresentation new.
	listing addSelectionAction: GLMAction new.
	listing addSelectionAction: GLMAction new.
	listing
		dynamicActionsOnSelection: [ :p | OrderedCollection with: GLMAction new with: GLMAction new with: GLMAction new ].
	self assert: listing allSelectionActions size equals: 2