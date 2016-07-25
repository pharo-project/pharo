testing
testGrouping
	| aCollection |
	aCollection := OrderedCollection new.

	historyList openGroup.
	self assert: historyList size = 1.
	self assert: (historyList at: 1) isComposite.
	self assert: (historyList at: 1) opened.
	historyList doAndAddRecord: (
		UndoRedoRecord
			do: (MessageSend receiver: aCollection selector: #addLast: argument: 1)
			undo: (MessageSend receiver: aCollection selector: #removeLast)).
	historyList doAndAddRecord: (
		UndoRedoRecord
			do: (MessageSend receiver: aCollection selector: #addLast: argument: 2)
			undo: (MessageSend receiver: aCollection selector: #removeLast)).
	self assert: historyList size = 1.
	self assert: (historyList at: 1) size = 2.
	historyList closeGroup.
	self assert: (historyList at: 1) closed.
	self assert: aCollection size = 2.
	self assert: aCollection first = 1.
	self assert: aCollection last = 2.
	historyList undo.
	self assert: aCollection isEmpty
