testing
testGroupedUndo1
	"test of grouping history entries afterwards by #groupFrom:to:"
	| aCollection |
	aCollection := OrderedCollection newFrom: {1. 2. 3}.

	historyList doAndAddRecord: (
		UndoRedoRecord
			do: (MessageSend receiver: aCollection selector: #removeLast )
			undo: (MessageSend receiver: aCollection selector: #addLast: argument: aCollection last)).

	historyList doAndAddRecord: (
		UndoRedoRecord
			do: (MessageSend receiver: aCollection selector: #removeLast )
			undo: (MessageSend receiver: aCollection selector: #addLast: argument: aCollection last)).

	historyList groupFrom: 1 to: 2.
	historyList undo.

	self assert: ((aCollection last = 3) and: [aCollection second = 2]).