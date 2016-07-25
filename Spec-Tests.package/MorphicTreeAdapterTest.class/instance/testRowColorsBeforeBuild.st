running
testRowColorsBeforeBuild
	treeWithItems oddRowColor: Color red.
	treeWithItems evenRowColor: Color green.
	treeWithItems buildWithSpec.
	self
		assert: treeWithItems widget widget rowColors
		equals: (Array with: Color green with: Color red)