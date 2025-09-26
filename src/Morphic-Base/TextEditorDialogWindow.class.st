"
Multi-line text entry dialog.
"
Class {
	#name : 'TextEditorDialogWindow',
	#superclass : 'MessageDialogWindow',
	#instVars : [
		'entryText',
		'textEditorMorph'
	],
	#category : 'Morphic-Base-Windows',
	#package : 'Morphic-Base',
	#tag : 'Windows'
}

{ #category : 'icons' }
TextEditorDialogWindow class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#smallQuestion
]

{ #category : 'accessing' }
TextEditorDialogWindow >> cancel [
	"Cancel and close."

	self entryText: nil.
	^super cancel
]

{ #category : 'focus handling' }
TextEditorDialogWindow >> defaultFocusMorph [
	"Answer the morph that should have the keyboard
	focus by default when the dialog is opened."

	^self textEditorMorph textMorph
]

{ #category : 'private' }
TextEditorDialogWindow >> entryHeight: aNumber [
	"Set the height of the text editor morph.
	Set the width to be 2 times this also."

	self textEditorMorph
		minHeight: aNumber;
		minWidth: 500
]

{ #category : 'accessing' }
TextEditorDialogWindow >> entryText [
	"Answer the value of entryText"

	^ entryText
]

{ #category : 'accessing' }
TextEditorDialogWindow >> entryText: anObject [
	"Set the value of entryText"

	entryText := anObject.
	self changed: #entryText.
	self textEditorMorph selectAll.
	self ok.
	self textEditorMorph hasUnacceptedEdits: true
]

{ #category : 'visual properties' }
TextEditorDialogWindow >> icon [
	"Answer an icon for the receiver."

	^ self iconNamed: #question
]

{ #category : 'initialization' }
TextEditorDialogWindow >> initialize [
	"Initialize the receiver."

	super initialize.
	self beResizeable.
	entryText := ''.
	self changed: #entryText.
	self textEditorMorph selectAll
]

{ #category : 'actions' }
TextEditorDialogWindow >> newButtons [
	"Answer new buttons as appropriate."

	^{
		self newCancelButton.
		self newOKButton isDefault: true
	}
]

{ #category : 'actions' }
TextEditorDialogWindow >> newContentMorph [
	"Answer a new content morph."

	self iconMorph: self newIconMorph.
	self textMorph: self newTextMorph.
	self textMorph wrapFlag: false.
	self textEditorMorph: self newTextEditorMorph.
	^self newGroupboxForAll: {
		self newRow: {self iconMorph. self textMorph}.
		self textEditorMorph}
]

{ #category : 'creation' }
TextEditorDialogWindow >> newTextEditorMorph [
	"Answer a new text entry morph."
	^(self
		newTextEditorFor: self
		getText: #entryText
		setText: #entryText:
		getEnabled: nil)
		selectAll
]

{ #category : 'accessing' }
TextEditorDialogWindow >> textEditorMorph [
	"Answer the value of textEditorMorph"

	^ textEditorMorph
]

{ #category : 'accessing' }
TextEditorDialogWindow >> textEditorMorph: anObject [
	"Set the value of textEditorMorph"

	textEditorMorph := anObject
]

{ #category : 'private' }
TextEditorDialogWindow >> textLines [

	^ super textLines, entryText asString lines
]
