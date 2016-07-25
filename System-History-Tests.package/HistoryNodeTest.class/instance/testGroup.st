as yet unclassified
testGroup
	| h i |
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h size = 1.
	self assert: (h at: 1) = i.
	h openGroup.
	self assert: h size = 2.
	self assert: (h at: 1) = i.
	self assert: (h at: 2) isComposite.
