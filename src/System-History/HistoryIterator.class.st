"
A HistoryIterator holds an UndoRedoGroup in order to store an history of commands with the possibility of undoing and redoing. The iterator cursor is represented by the index inst var. Index always contains the position of the command that can be currently undone. So, undo decrease index and redo increase index. When a new record is stored, then, index contains the newly added record position.
See HistoryIteratorTest for examples.

Instance Variables
	index:		<Integer>
	maxSize:		<Integer>
	plugged:		<Boolean>
	recorder:		<UndoRedoGroup>

index
	- the iterator cursor

maxSize
	- the maximum number of records that can be added in the root group.

plugged
	- if false, then adding of command is not allowed. Useful to prevent bad history recording recurssions (record while undoing or redoing).

recorder
	- The root of the history tree which records undo/redo commands

"
Class {
	#name : #HistoryIterator,
	#superclass : #Object,
	#instVars : [
		'index',
		'plugged',
		'recorder',
		'maxSize'
	],
	#category : #'System-History-Iterators'
}

{ #category : #adding }
HistoryIterator >> addItem: anHistoryItem [
	| result |
	self isPlugged ifFalse: [^ false].
	self recorder removeLast: (self size - self index).
	result := self recorder addItem: anHistoryItem.
	self updateIndex.
	^ result
]

{ #category : #adding }
HistoryIterator >> addRecord: anItem [
	^ self addItem: anItem

]

{ #category : #accessing }
HistoryIterator >> at: anInteger [
	^ self recorder at: anInteger
]

{ #category : #accessing }
HistoryIterator >> at: anInteger put: anItem [
	self recorder at: anInteger put: anItem
]

{ #category : #grouping }
HistoryIterator >> closeGroup [
	self recorder closeGroup

]

{ #category : #compatibility }
HistoryIterator >> closeRecordGroup [
	^ self closeGroup
]

{ #category : #accessing }
HistoryIterator >> current [
	^ (self index <= self size and: [self index > 0])
		ifTrue: [self recorder at: self index]
]

{ #category : #accessing }
HistoryIterator >> defaultMaximumSize [
	"unlimited by default"
	^ 999999999
]

{ #category : #'undo-undo' }
HistoryIterator >> do [
	^ self redo
]

{ #category : #'undo-undo' }
HistoryIterator >> doAndAddRecord: anUndoRedoRecord [
	| result |
	result := anUndoRedoRecord do.
	self addItem: anUndoRedoRecord.
	^ result
]

{ #category : #accessing }
HistoryIterator >> first [
	^ self recorder first
]

{ #category : #grouping }
HistoryIterator >> groupFrom: firstIdx to: secondIdx [
	| group |
	group := UndoRedoGroup new.
	firstIdx to: (secondIdx min: index) do: [:i | group addItem: (self at:i)].
	group close.
	group 
		isEmpty ifTrue: [^ self].
	firstIdx + 1 to: (secondIdx min: index) do: [:i | self removeAt: i].
	self at: firstIdx put: group.
	index := index - group size + 1.
]

{ #category : #testing }
HistoryIterator >> hasNext [
	"is there an item after current index"
	^ self recorder size - self index > 0
]

{ #category : #testing }
HistoryIterator >> hasPrevious [
	"is there an item before current index"
	^ self index > 0
]

{ #category : #accessing }
HistoryIterator >> index [
	^ index ifNil: [index := self size]
]

{ #category : #testing }
HistoryIterator >> isPlugged [
	^ plugged ifNil: [plugged := true]
]

{ #category : #accessing }
HistoryIterator >> last [
	^ self recorder last
]

{ #category : #accessing }
HistoryIterator >> maxSize [
	^ maxSize ifNil: [maxSize := self defaultMaximumSize]
]

{ #category : #accessing }
HistoryIterator >> maxSize: anInteger [
	maxSize := anInteger
]

{ #category : #accessing }
HistoryIterator >> next [
	"next item in history"
	^ self hasNext
		ifTrue: [ index := self index + 1.
			self current]
]

{ #category : #accessing }
HistoryIterator >> nextList [
	"return 'next' items sublist "
	^ self recorder copyFrom: self index + 1 to: self size
]

{ #category : #grouping }
HistoryIterator >> openGroup [
	self recorder openGroup.
	self updateIndex.

]

{ #category : #compatibility }
HistoryIterator >> openRecordGroup [
	^ self openGroup
]

{ #category : #accessing }
HistoryIterator >> previous [
	"previous item in history"
	^ self hasPrevious
		ifTrue: [index := self index - 1.
			self current]
]

{ #category : #accessing }
HistoryIterator >> previousList [
	"return 'prev' items sublist"
	^ self recorder copyFrom: 1 to: self index - 1
]

{ #category : #accessing }
HistoryIterator >> recorder [
	^ recorder ifNil: [recorder := UndoRedoGroup new]
]

{ #category : #'undo-undo' }
HistoryIterator >> redo [
	self unplugWhile: [
		self hasNext ifFalse: [^false].
		self next redo].
	^ true
]

{ #category : #'undo-undo' }
HistoryIterator >> redo: doMessageSend undo: undoMessageSend [
	self addItem: (UndoRedoRecord redo: doMessageSend undo: undoMessageSend)
]

{ #category : #'undo-undo' }
HistoryIterator >> redoArray: doArray undoArray: undoArray [
	self addItem: (UndoRedoRecord redoArray: doArray undoArray: undoArray)
]

{ #category : #removing }
HistoryIterator >> removeAt: anIndex [
	self recorder removeAt: anIndex
]

{ #category : #removing }
HistoryIterator >> removeFirst [
	self recorder removeFirst
]

{ #category : #initialization }
HistoryIterator >> reset [
	self recorder reset.
	index := nil.
]

{ #category : #accessing }
HistoryIterator >> size [
	^ self recorder size
]

{ #category : #'undo-undo' }
HistoryIterator >> undo [
	self unplugWhile: [
		self current ifNotNil: [:curr | curr undo].
		self previous].
	^ true
]

{ #category : #'undo-undo' }
HistoryIterator >> unplugWhile: aBlock [
	| wasPlugged |
	wasPlugged := self isPlugged.
	plugged := false.
	aBlock ensure: [ plugged := wasPlugged ]
]

{ #category : #adding }
HistoryIterator >> updateIndex [
	self size <= self maxSize
		ifFalse: [self removeFirst].
	index := self size.

]
