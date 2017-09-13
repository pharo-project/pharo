tests
testReset
	| h i |
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h current = i.
	self assert: h size = 1.
	h reset.
	self assert: h size = 0.