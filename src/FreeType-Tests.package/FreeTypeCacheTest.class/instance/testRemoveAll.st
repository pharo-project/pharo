tests
testRemoveAll
	| m fifo fontTable |
	m := fullCache instVarNamed: #maximumSize.
	fifo := fullCache instVarNamed: #fifo.
	fontTable := fullCache instVarNamed: #fontTable.
	fullCache removeAll.
	self assert: (fullCache instVarNamed: #fifo) isEmpty.
	self assert: (fullCache instVarNamed: #fontTable) isEmpty.
	self assert: (fullCache instVarNamed: #used) = 0.
	self assert: m = (fullCache instVarNamed: #maximumSize).
	self assert: fifo class = (fullCache instVarNamed: #fifo) class.
	self assert: fontTable class = (fullCache instVarNamed: #fontTable) class.	