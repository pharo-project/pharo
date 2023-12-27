"
Dialog window displaying a message with a single OK button. Escape/return will close. Icon is a themed information icon.
Handles long messages through use of a text editor with potential for scrolling.
"
Class {
	#name : 'LongMessageDialogWindow',
	#superclass : 'MessageDialogWindow',
	#classTraits : '{} + TraitedClass',
	#instVars : [
		'entryText'
	],
	#category : 'Polymorph-Widgets-Windows',
	#package : 'Polymorph-Widgets',
	#tag : 'Windows'
}

{ #category : 'accessing' }
LongMessageDialogWindow >> entryText [
	"Answer the value of entryText"

	^ entryText
]

{ #category : 'accessing' }
LongMessageDialogWindow >> entryText: anObject [
	"Set the value of entryText"

	entryText := anObject.
	self changed: #entryText
]

{ #category : 'testing' }
LongMessageDialogWindow >> isResizeable [
	"Answer whether we are not we can be resized."

	^true
]

{ #category : 'building' }
LongMessageDialogWindow >> newContentMorph [
	"Answer a new content morph."

	self iconMorph: self newIconMorph.
	self textMorph: self newTextMorph.
	^self newGroupboxFor: (
		(self newRow: {self iconMorph. self textMorph})
			cellPositioning: #top;
			vResizing: #spaceFill)
]

{ #category : 'building' }
LongMessageDialogWindow >> newTextMorph [
	"Answer a new text editor morph."

	|tm|
	tm := (self
		newTextEditorFor: self
		getText: #entryText
		setText: #entryText:
		getEnabled: nil)
			beForPlainText;
			minWidth: self display width // 4;
			minHeight: self display height // 4;
			wrapped: true;
			beReadOnly.
	^tm
]

{ #category : 'accessing' }
LongMessageDialogWindow >> text: aStringOrText [
	"Set the text."

	|t|
	t := aStringOrText isString
		ifTrue: [aStringOrText asText addAttribute: (TextFontReference toFont: self textFont); yourself]
		ifFalse: [aStringOrText].
	self entryText: t
]
