tests
testClose
	| h i |
	h := HistoryNode new.
	h close.
	h addItem: (i := HistoryLeaf new).
	h open.
 	h addItem: i.
	self assert: h current = i