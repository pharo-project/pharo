"
Single-line text entry dialog.
"
Class {
	#name : #TextEntryDialogWindow,
	#superclass : #TextEditorDialogWindow,
	#classVars : [
		'MinimumWidth'
	],
	#category : #'Polymorph-Widgets-Windows'
}

{ #category : #settings }
TextEntryDialogWindow class >> minimumWidth [
	^ MinimumWidth ifNil: [MinimumWidth := 450]
]

{ #category : #settings }
TextEntryDialogWindow class >> minimumWidth: anInteger [
	MinimumWidth := anInteger
]

{ #category : #accessing }
TextEntryDialogWindow >> entryCompletion: anEntryCompletion [
	anEntryCompletion chooseBlock: [:t | self ok].
	self textEditorMorph entryCompletion: anEntryCompletion


]

{ #category : #api }
TextEntryDialogWindow >> initialExtent [
	^ self minimumWidth @ (super initialExtent y)

]

{ #category : #accessing }
TextEntryDialogWindow >> minimumWidth [
	^ (super minimumWidth max: self class minimumWidth) min: (Display width)
]

{ #category : #accessing }
TextEntryDialogWindow >> newButtons [
	"Answer new buttons as appropriate."

	^{self newOKButton isDefault: true. self newCancelButton}
]

{ #category : #creation }
TextEntryDialogWindow >> newTextEditorMorph [
	"Answer a new text entry morph."

	^(self
		newTextEntryFor: self
		getText: #entryText
		setText: #entryText:
		getEnabled: nil
		help: nil)
		acceptOnCR: false;
		selectAll
]

{ #category : #accessing }
TextEntryDialogWindow >> withDropListButton [
	self textEditorMorph withDropListButton
]
