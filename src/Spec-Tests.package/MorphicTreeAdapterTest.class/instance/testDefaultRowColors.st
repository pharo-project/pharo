running
testDefaultRowColors
	| defaultColor |
	defaultColor := Smalltalk ui theme backgroundColor.
	treeWithItems buildWithSpec.
	self
		assert: treeWithItems widget widget rowColors
		equals: (Array with: defaultColor with: defaultColor)