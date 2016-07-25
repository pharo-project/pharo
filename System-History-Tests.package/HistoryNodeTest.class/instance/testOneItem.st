as yet unclassified
testOneItem
	| h i |
	h := HistoryNode new.
	h addItem: (i := HistoryLeaf new).
	self assert: h size = 1.
	self assert: h current = i