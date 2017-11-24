"
I'm a Dialog whose TextEditors characters are replaced by *. 

Example:
(UITheme builder openModal: 
        (PasswordDialogWindow new
                title: 'Authentification';
                textFont: StandardFonts defaultFont;
                text: 'Enter your password')) entryText explore
"
Class {
	#name : #PasswordDialogWindow,
	#superclass : #TextEntryDialogWindow,
	#instVars : [
		'textEditor',
		'showPassword'
	],
	#category : #'Polymorph-Widgets-Windows'
}

{ #category : #protocol }
PasswordDialogWindow >> beDecrypted [

	textEditor font: self theme textFont
]

{ #category : #protocol }
PasswordDialogWindow >> beEncrypted [

	textEditor font: (StrikeFont passwordFontSize: self theme textFont pointSize)
]

{ #category : #actions }
PasswordDialogWindow >> newContentMorph [
	"Answer a new content morph."

	self iconMorph: self newIconMorph.
	self textMorph: self newTextMorph.
	self textMorph wrapFlag: false.
	self textEditorMorph: self newTextEditorMorph.
	^self newGroupboxForAll: {
		self newRow: {self iconMorph. self textMorph}.
		self textEditorMorph.
		self showPasswordButton}
]

{ #category : #creation }
PasswordDialogWindow >> newTextEditorMorph [
	"Answer a new morph for the text entry using a password font."

	textEditor := super newTextEditorMorph.
	self beEncrypted.
	^ textEditor
]

{ #category : #private }
PasswordDialogWindow >> showPassword [
	
	^ showPassword ifNil: [ showPassword := false ]
]

{ #category : #private }
PasswordDialogWindow >> showPassword: aBoolean [

	(showPassword := aBoolean)
		ifTrue: [ self beDecrypted ]
		ifFalse: [ self beEncrypted ].
	self changed: #showPassword
]

{ #category : #private }
PasswordDialogWindow >> showPasswordButton [

	^ (CheckboxMorph 
			on: self 
			selected: #showPassword
			changeSelected: #showPassword:)
			label: 'Show password';
			labelClickable: true;
			height: 25.
]
