tests
testEmptyHistory
	| h |
	h := HistoryNode new.
	self assert: h size = 0.
	self assert: h current isNil