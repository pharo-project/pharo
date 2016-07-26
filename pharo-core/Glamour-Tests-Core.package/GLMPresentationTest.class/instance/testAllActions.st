tests
testAllActions
	| listing |
	listing := GLMPresentation new.
	self assert: listing hasActions not.
	listing addAction: GLMAction new.
	listing addAction: GLMAction new.
	listing dynamicActions: [ :p | OrderedCollection with: GLMAction new with: GLMAction new with: GLMAction new ].
	self assert: listing allActions size equals: 5.
	self assert: listing hasActions