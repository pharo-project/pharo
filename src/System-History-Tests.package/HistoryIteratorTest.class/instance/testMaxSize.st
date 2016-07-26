testing
testMaxSize

	| aCollection |
	aCollection := OrderedCollection newFrom: {1. 2. 3}.
	historyList maxSize: 5.

	10 timesRepeat: [
		historyList doAndAddRecord: (
			UndoRedoRecord
				do: (MessageSend receiver: aCollection selector: #size)
				undo: (MessageSend receiver: aCollection selector: #size)).
	].

	self assert: historyList size = 5.