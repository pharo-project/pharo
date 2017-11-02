"
I represent datas of the current text area to manage undo operations etc
"
Class {
	#name : #RubEditingState,
	#superclass : #Object,
	#instVars : [
		'hasFocus',
		'undoManager',
		'emphasisHere',
		'pointBlock',
		'markBlock',
		'startOfTyping',
		'previousInterval',
		'scrollPivot',
		'textArea'
	],
	#category : 'Rubric-Editing-Core'
}

{ #category : #undo-redo }
RubEditingState >> clearUndoManager: aKeyboardEvent [
	self undoManager reset.
	^ true
]

{ #category : #accessing }
RubEditingState >> emphasisHere [
	^ emphasisHere
]

{ #category : #accessing }
RubEditingState >> emphasisHere: aListOfTextAttribute [
	emphasisHere := aListOfTextAttribute ifNotNil: [ :l | l reject: [ :a | RubTextSelectionColor = a class ] ]
]

{ #category : #private-debugging }
RubEditingState >> exploreUndoManager: aKeyboardEvent [
	self undoManager inspect.
	^ true
]

{ #category : #accessing }
RubEditingState >> hasFocus [
	^ hasFocus ifNil: [ hasFocus := false ]
]

{ #category : #accessing }
RubEditingState >> hasFocus: aBoolean [
	hasFocus := aBoolean
]

{ #category : #accessing }
RubEditingState >> markBlock [
	^ markBlock
]

{ #category : #accessing }
RubEditingState >> markBlock: aCharacterBlock [
	markBlock := aCharacterBlock
]

{ #category : #accessing }
RubEditingState >> markBlock: markCharacterBlock pointBlock: pointCharacterBlock [
	markBlock := markCharacterBlock.
	pointBlock := pointCharacterBlock
]

{ #category : #accessing }
RubEditingState >> markIndex [
	^ self markBlock ifNotNil: [:m | m stringIndex] ifNil: [ 1 ]

]

{ #category : #accessing }
RubEditingState >> markIndex: markIndex pointIndex: pointIndex [
	markBlock := self paragraph characterBlockForIndex: markIndex.
	pointBlock := self paragraph characterBlockForIndex: pointIndex
]

{ #category : #accessing }
RubEditingState >> paragraph [
	^ textArea paragraph
]

{ #category : #accessing }
RubEditingState >> pointBlock [
	^ pointBlock
]

{ #category : #accessing }
RubEditingState >> pointBlock: aCharacterBlock [
	pointBlock := aCharacterBlock
]

{ #category : #accessing }
RubEditingState >> pointIndex [
	^ self pointBlock ifNotNil: [:p | p stringIndex ] ifNil: [ 0 ]
]

{ #category : #accessing }
RubEditingState >> previousInterval [

	^ previousInterval ifNil: [previousInterval := 1 to: 0]
]

{ #category : #accessing }
RubEditingState >> previousInterval: anInterval [

	previousInterval := anInterval.

]

{ #category : #undo-redo }
RubEditingState >> redo [
	^ self undoManager redo 
]

{ #category : #undo-redo }
RubEditingState >> redoArray: doArray undoArray: undoArray [
	self undoManager isPlugged 
		ifTrue: [self undoManager redoArray: doArray undoArray: undoArray]
]

{ #category : #accessing }
RubEditingState >> scrollPivot [
	^ scrollPivot
]

{ #category : #accessing }
RubEditingState >> scrollPivot: aPoint [
	scrollPivot := aPoint
]

{ #category : #accessing }
RubEditingState >> selectionStart [
	^ self pointBlock ifNotNil: [:pb | pb min: self markBlock]
]

{ #category : #accessing }
RubEditingState >> selectionStop [
	^ self pointBlock ifNotNil: [:pb | pb max: self markBlock]
]

{ #category : #accessing }
RubEditingState >> startOfTyping [
	^ startOfTyping
]

{ #category : #accessing }
RubEditingState >> startOfTyping: anIntegerIndex [
	startOfTyping := anIntegerIndex
]

{ #category : #accessing }
RubEditingState >> textArea: aTextArea [
	textArea := aTextArea.

]

{ #category : #undo-redo }
RubEditingState >> undo [
	^ self undoManager undo 
]

{ #category : #accessing }
RubEditingState >> undoManager [

	^ undoManager ifNil: [ undoManager := HistoryIterator new].

]

{ #category : #undo-redo }
RubEditingState >> undoRedoTransaction: aBlock [
	self undoManager openRecordGroup.
	aBlock ensure: [  self undoManager closeRecordGroup ].

]

{ #category : #private }
RubEditingState >> unplug [
	super unplug.
	undoManager
		ifNotNil: [ 
			undoManager reset.
			undoManager := nil ].
	 pointBlock := markBlock := startOfTyping := previousInterval := nil.
]

{ #category : #accessing }
RubEditingState >> unselect [
	self markBlock: self pointBlock copy
]
