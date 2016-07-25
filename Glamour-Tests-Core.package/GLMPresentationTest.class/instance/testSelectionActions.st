tests
testSelectionActions
	| listing |
	listing := GLMPresentation new.
	listing selectionActions isEmpty.
	listing addSelectionAction: GLMAction new.
	self assert: listing selectionActions size equals: 1.