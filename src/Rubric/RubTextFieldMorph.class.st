"
I am a morph for a text area which has only one line, like search field
"
Class {
	#name : #RubTextFieldMorph,
	#superclass : #RubScrolledTextMorph,
	#instVars : [
		'hasValidText'
	],
	#category : #'Rubric-Editing-Widgets'
}

{ #category : #drawing }
RubTextFieldMorph >> adornmentColor [
	"color to Indicate edit status for the given morph."

	self hasValidText
		ifFalse: [ ^ Color red ].
	^ super adornmentColor
]

{ #category : #configure }
RubTextFieldMorph >> configureGhostText: aTextArea [
	super configureGhostText: aTextArea.
	self hasKeyboardFocus
		ifTrue: [ 
			aTextArea left: self left]
]

{ #category : #defaults }
RubTextFieldMorph >> defaultGhostTextMorph [
	^ super defaultGhostTextMorph beNotWrapped 
]

{ #category : #geometry }
RubTextFieldMorph >> extent: aPoint [
	super extent: aPoint x @ self textFieldHeight
]

{ #category : #'model protocol' }
RubTextFieldMorph >> hasValidText [
	^ hasValidText ifNil: [ hasValidText := true ] 
]

{ #category : #'model protocol' }
RubTextFieldMorph >> hasValidText: aBoolean [
	hasValidText := aBoolean
]

{ #category : #'initialize-release' }
RubTextFieldMorph >> initialize [
	super initialize.
	self textArea announcer when: RubReturnEntered  send: #whenReturnEntered: to: self.
	self scrollbarsShowNever.
	self extent: self extent
]

{ #category : #private }
RubTextFieldMorph >> resetState [
	hasValidText := nil.
	super resetState
]

{ #category : #'initialize-release' }
RubTextFieldMorph >> textAreaClass [
	^ RubTextFieldArea
]

{ #category : #geometry }
RubTextFieldMorph >> textFieldHeight [
	| vgap |
	vgap := self currentHScrollBarThickness isZero
		ifTrue: [  0 ]
		ifFalse: [ self currentHScrollBarThickness - 1 ].
	^ self textArea height + 1 + (self borderWidth * 2) + vgap
]

{ #category : #'event handling' }
RubTextFieldMorph >> whenReturnEntered: anAnnouncement [
	self changed

]
