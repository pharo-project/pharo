"
I print position of cursor in the text editor.
Look at #printCursorPosition method for details.
  
Internal Representation and Key Implementation Points.

    Instance Variables
	labelMorph:		<StringMorph>
"
Class {
	#name : #ClyCursorPositionLabelMorph,
	#superclass : #ClyTextMorphToolMorph,
	#instVars : [
		'labelMorph'
	],
	#category : #'Calypso-Browser-TextEditors'
}

{ #category : #controlling }
ClyCursorPositionLabelMorph >> attachToTextMorph [
	super attachToTextMorph.
	
	self textArea announcer when: RubSelectionChanged send: #updateContents to: self.
	self updateContents
]

{ #category : #controlling }
ClyCursorPositionLabelMorph >> detachFromTextMorph [
	super detachFromTextMorph.
	
	self textArea announcer unsubscribe: self
]

{ #category : #initialization }
ClyCursorPositionLabelMorph >> initialize [
	super initialize.	
	self setBalloonText: 'Current line number/Total line number [Current character number]'.
	labelMorph := '' asStringMorph.
	self addMorph: labelMorph
]

{ #category : #updating }
ClyCursorPositionLabelMorph >> printCursorPosition [
	| result textArea |
	textArea := self textArea.
	textArea selectionStart ifNil: [ ^ '' ].
	textArea paragraph lines ifNil: [ ^ '' ].
	result := self printTextLinePositionOf: textArea selectionStart.
	textArea selectionStop ~= textArea selectionStart
		ifTrue: [ result := result , ' -- ' , (self printTextLinePositionOf: textArea selectionStop) ].
	^result
]

{ #category : #updating }
ClyCursorPositionLabelMorph >> printTextLinePositionOf: aCharacterBlock [
	| result textLine linePosition |
	textLine := aCharacterBlock textLine.
	linePosition := aCharacterBlock stringIndex - textLine first + 1.
	result := textLine lineNumber printString , '/' , self textArea lines size printString
		, ' [', linePosition printString, ']'.
	^ result
]

{ #category : #updating }
ClyCursorPositionLabelMorph >> updateContents [

	labelMorph contents: self printCursorPosition
]
