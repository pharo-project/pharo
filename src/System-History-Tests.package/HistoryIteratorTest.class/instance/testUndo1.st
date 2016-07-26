testing
testUndo1

	| aCollection |
	aCollection := OrderedCollection new.

	historyList doAndAddRecord: (
		UndoRedoRecord
			do: (MessageSend receiver: aCollection selector: #addLast: argument: 1)
			undo: (MessageSend receiver: aCollection selector: #removeLast)).

	historyList undo.

	self assert: aCollection size = 0.