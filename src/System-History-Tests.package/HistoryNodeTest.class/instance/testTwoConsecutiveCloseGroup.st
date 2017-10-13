tests
testTwoConsecutiveCloseGroup
	| h i grp1 grp2  i2 i3 i4 | 
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h size = 1.
	self assert: (h at: 1) = i.
	h openGroup.
	self assert: h size = 2.
	grp1 := h at: 2.
	h openGroup.
	self assert: h size = 2.
	self assert: grp1 size = 1.
	grp2 := grp1 at: 1.
	self assert: grp2 size = 0.
	h closeGroup.
	h closeGroup.
	h addItem: (i3 := HistoryLeaf new).
	self assert: h size = 3.
	self assert: h current = i3.
	

	
	
