testing
testUndo2

	| aCollection |
	aCollection := OrderedCollection newFrom: {1. 2. 3}.

	historyList doAndAddRecord: (
		UndoRedoRecord
			do: (MessageSend receiver: aCollection selector: #removeLast )
			undo: (MessageSend receiver: aCollection selector: #addLast: argument: aCollection last)).

	historyList undo.

	self assert: aCollection size = 3.	
	self assert: aCollection last = 3.