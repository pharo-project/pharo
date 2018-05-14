"
SUnit tests for HistoryIterator
"
Class {
	#name : #HistoryIteratorTest,
	#superclass : #TestCase,
	#instVars : [
		'historyList'
	],
	#category : #'System-History-Tests'
}

{ #category : #running }
HistoryIteratorTest >> setUp [
	super setUp.
	historyList := HistoryIterator new.
]

{ #category : #testing }
HistoryIteratorTest >> testGroupedUndo1 [
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
]

{ #category : #testing }
HistoryIteratorTest >> testGroupedUndo2 [
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
]

{ #category : #testing }
HistoryIteratorTest >> testGrouping [
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

]

{ #category : #testing }
HistoryIteratorTest >> testMaxSize [

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
]

{ #category : #testing }
HistoryIteratorTest >> testUndo1 [

	| aCollection |
	aCollection := OrderedCollection new.

	historyList doAndAddRecord: (
		UndoRedoRecord
			do: (MessageSend receiver: aCollection selector: #addLast: argument: 1)
			undo: (MessageSend receiver: aCollection selector: #removeLast)).

	historyList undo.

	self assert: aCollection size = 0.
]

{ #category : #testing }
HistoryIteratorTest >> testUndo2 [

	| aCollection |
	aCollection := OrderedCollection newFrom: {1. 2. 3}.

	historyList doAndAddRecord: (
		UndoRedoRecord
			do: (MessageSend receiver: aCollection selector: #removeLast )
			undo: (MessageSend receiver: aCollection selector: #addLast: argument: aCollection last)).

	historyList undo.

	self assert: aCollection size = 3.	
	self assert: aCollection last = 3.
]
