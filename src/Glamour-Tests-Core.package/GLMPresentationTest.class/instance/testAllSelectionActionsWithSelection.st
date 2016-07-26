tests
testAllSelectionActionsWithSelection
	| listing |
	listing := GLMPresentation new.
	listing addSelectionAction: GLMAction new.
	listing addSelectionAction: GLMAction new.
	listing selection: Object new.
	listing
		dynamicActionsOnSelection: [ :p | OrderedCollection with: GLMAction new with: GLMAction new with: GLMAction new ].
	self assert: listing allSelectionActions size equals: 5