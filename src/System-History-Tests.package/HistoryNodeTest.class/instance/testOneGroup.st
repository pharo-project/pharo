as yet unclassified
testOneGroup
	| h i c i2 i3 i4 | 
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h size = 1.
	self assert: (h at: 1) = i.
	h openGroup.
	self assert: h size = 2.
	self assert: (h at: 1) = i.
	self assert: (c := h at: 2) isComposite.
	self assert: c isEmpty.
	h addItem: (i2 := HistoryLeaf new).
	self assert: h size = 2.
	self assert: c size = 1.
	self assert: c current = i2.
	h closeGroup.
	h addItem: (i3 := HistoryLeaf new).
	self assert: h size = 3.
	self assert: (h at: 3) = i3.
	h closeGroup.
	h addItem: (i4 := HistoryLeaf new).
	self assert: h size = 3.
	

	
	
