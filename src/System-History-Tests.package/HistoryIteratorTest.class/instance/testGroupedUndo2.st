testing
testGroupedUndo2
	"test of grouping history entries by #openGroup and #closeGroup"
	| aCollection |
	aCollection := OrderedCollection newFrom: {1. 2. 3}.

	historyList openGroup.
	historyList doAndAddRecord: (
		UndoRedoRecord
			do: (MessageSend receiver: aCollection selector: #removeLast )
			undo: (MessageSend receiver: aCollection selector: #addLast: argument: aCollection last)).

	historyList doAndAddRecord: (
		UndoRedoRecord
			do: (MessageSend receiver: aCollection selector: #removeLast )
			undo: (MessageSend receiver: aCollection selector: #addLast: argument: aCollection last)).
	historyList closeGroup.

	"the historyList should have grouped the two commands and placed this group in the undoHistory"
	self assert: ((historyList size = 1) and: [historyList last isKindOf: UndoRedoGroup]).

	historyList undo.
	"both UndoRecords in the group should have been made undone"
	self assert: ((aCollection last = 3) and: [aCollection second = 2]).