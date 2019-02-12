"
I store the current state of an editing session. An instance of mine is shared by all TextEditor instances that are created during an editing session managed by a TextMorph (see below for more explanations about editing session). The state data are basically made of an undo/redo manager and of all data needed in order to manage text editing undo and redo (mainly all informations for the current and previous selection intervals).
I'm created by a TextEditor at the beginning of an editing session (see TextEditor>>editingStateClass and TextEditor>>editingState). Specializations can be introduced to fit a particular TextEditor subclass need.

Editing session:
An  editing session starts when a TextMorph is created (precisely, when a TextEditor instance is first assigned to a TextMorph editor instance variable). An editing session ends when a TextMorph is deleted. During an editing session, a TextMorph can make use of a lot of TextEditor instances, one at a time. As an example, each time a TextMorph is resized, its editor is released and a new one that fit the TextMorph physical properties  is created. Another example, when a TextMorph loses the keyboard focus, then its editor could be fully released; it is created again when the TextMorph retrieves the focus.
When an editor is created by a TextMorph, the state of the previous TextEditor, stored in its associated EditingState instance, is got and passed to the newly created editor. Thus the editing session remains stable (see TextEditor >> #stateArray and TextMorph >> #installEditorToReplace:). So TextEditor instances are extremely volatile whereas its associated EditingState instance remains during the whole editing session.

Instance Variables:
   emphasisHere <Array of TextAttribute>
   pointBlock <CharacterBlock>
   markBlock <CharacterBlock>
   startOfTyping <Integer>
   previousInterval <Interval>
   previousSelection <Text>
   undoManager <HistoryIterator>
   lastParenLocation <Integer>
   mouseDownInterval <Interval>

- emphasisHere:
   The TextAttributes that are used for the newly entered text

- pointBlock:
   The CharacterBlock where a selection begins (where the mouse has first pointed)

- markBlock:
   The CharacterBlock where a selection ends

- startOfTyping:
   The index of the first character which has been entered during the currently undoable/redoable portion of text (see TextEditor>>#openTypeIn and TextEditor>>#doneTyping)

- previousInterval:
   Previous interval used for undo/redo actions

- previousSelection:
   The previously selected text for undo/redo actions

- undoManager:
   The undo/redo manager

- lastParenLocation:
   Keep the position of the open parenthesis which corresponds to the last entered close parenthesis

- mouseDownInterval:
   The position of the first mouse down in the editor

"
Class {
	#name : #EditingState,
	#superclass : #Object,
	#instVars : [
		'emphasisHere',
		'pointBlock',
		'markBlock',
		'startOfTyping',
		'previousInterval',
		'previousSelection',
		'undoManager',
		'lastParenLocation',
		'mouseDownInterval'
	],
	#category : #'Text-Edition'
}

{ #category : #'undo-redo' }
EditingState >> clearUndoManager: aKeyboardEvent [
	self undoManager reset.
	^ true
]

{ #category : #accessing }
EditingState >> emphasisHere [
	^ emphasisHere
]

{ #category : #accessing }
EditingState >> emphasisHere: aListOfTextAttribute [ 
	emphasisHere := aListOfTextAttribute
				ifNotNil: [:l | l reject: [:a | TextSelectionColor = a class]]
]

{ #category : #'private-debugging' }
EditingState >> exploreUndoManager: aKeyboardEvent [
	self undoManager inspect.
	^ true
]

{ #category : #events }
EditingState >> focused: aBoolean from: aTextMorph [
	(aBoolean and: [aTextMorph sharesFindReplace])
		ifTrue: [[(EditorFindReplaceDialogWindow on: aTextMorph) comeToFront] on: Error do: []].
]

{ #category : #events }
EditingState >> handlesKeyboard: evt from: aTextMorph [
	^ false
]

{ #category : #events }
EditingState >> keystroke: aKeyboardEvent from: aTextMorph [

]

{ #category : #accessing }
EditingState >> lastParenLocation [
	^ lastParenLocation
]

{ #category : #accessing }
EditingState >> lastParenLocation: anIntegerIndex [
	lastParenLocation := anIntegerIndex
]

{ #category : #accessing }
EditingState >> markBlock [
	^ markBlock
]

{ #category : #accessing }
EditingState >> markBlock: aCharacterBlock [
	markBlock := aCharacterBlock
]

{ #category : #events }
EditingState >> mouseDown: anEvent from: aTextMorph [
	self mouseDownInterval: aTextMorph editor selectionInterval.
	self focused: true from: aTextMorph

]

{ #category : #accessing }
EditingState >> mouseDownInterval [
	^ mouseDownInterval
]

{ #category : #accessing }
EditingState >> mouseDownInterval: anInterval [
	mouseDownInterval := anInterval
]

{ #category : #accessing }
EditingState >> pointBlock [
	^ pointBlock
]

{ #category : #accessing }
EditingState >> pointBlock: aCharacterBlock [
	pointBlock := aCharacterBlock.

]

{ #category : #accessing }
EditingState >> previousInterval [

	^ previousInterval ifNil: [previousInterval := 1 to: 0]
]

{ #category : #accessing }
EditingState >> previousInterval: anInterval selection: aSelection [

	previousInterval := anInterval.
	previousSelection := aSelection.

]

{ #category : #accessing }
EditingState >> previousSelection [

	^ previousSelection ifNil: [previousSelection := '' asText]
]

{ #category : #'undo-redo' }
EditingState >> redo [
	^ self undoManager redo 
]

{ #category : #'undo-redo' }
EditingState >> redoArray: doArray undoArray: undoArray [
	self undoManager isPlugged 
		ifTrue: [self undoManager redoArray: doArray undoArray: undoArray]
]

{ #category : #accessing }
EditingState >> startOfTyping [
	^ startOfTyping
]

{ #category : #accessing }
EditingState >> startOfTyping: anIntegerIndex [
	startOfTyping := anIntegerIndex
]

{ #category : #'undo-redo' }
EditingState >> undo [
	^ self undoManager undo 
]

{ #category : #accessing }
EditingState >> undoManager [

	^ undoManager ifNil: [ undoManager := HistoryIterator new].

]

{ #category : #accessing }
EditingState >> unselect [
	self markBlock: self pointBlock copy
]
