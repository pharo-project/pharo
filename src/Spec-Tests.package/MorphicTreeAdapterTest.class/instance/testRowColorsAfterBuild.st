running
testRowColorsAfterBuild
	treeWithItems buildWithSpec.
	treeWithItems oddRowColor: Color red.
	treeWithItems evenRowColor: Color green.
	self
		assert: treeWithItems widget widget rowColors
		equals: (Array with: Color green with: Color red)