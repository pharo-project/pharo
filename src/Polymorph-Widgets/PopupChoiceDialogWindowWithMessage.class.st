"
I'm a dialog which ask the user to select a element in a list. See me in action:

UIManager default inform:
        (UIManager default 
                chooseFrom: #(one two three) 
                values: #('1' '2' '3')  
                message: 'Select one' 
                title: 'PopupChoiceDialogWindowWithMessage example').
"
Class {
	#name : #PopupChoiceDialogWindowWithMessage,
	#superclass : #PopupChoiceDialogWindow,
	#instVars : [
		'textMorph',
		'iconMorph',
		'textFont',
		'message'
	],
	#category : #Polymorph-Widgets-Windows
}

{ #category : #accessing }
PopupChoiceDialogWindowWithMessage >> icon [
	"Answer an icon for the receiver."

	^ self iconNamed: #questionIcon
]

{ #category : #accessing }
PopupChoiceDialogWindowWithMessage >> iconMorph [
	"Answer the value of iconMorph"

	^ iconMorph
]

{ #category : #accessing }
PopupChoiceDialogWindowWithMessage >> iconMorph: anObject [
	"Set the value of iconMorph"

	iconMorph := anObject
]

{ #category : #api }
PopupChoiceDialogWindowWithMessage >> initialExtent [
	"Answer the initial extent for the receiver.
	Adjust the text if the text	would be wider than 1/2 the display width."
	
	|ext|
	ext := super initialExtent.
	self textMorph width > (Display width // 2) ifTrue: [
		self textMorph
			wrapFlag: true;
			hResizing: #rigid;
			extent: Display width // 2 @ 0.
		ext := super initialExtent].
	^ext
]

{ #category : #accessing }
PopupChoiceDialogWindowWithMessage >> message [
	
	^ message
]

{ #category : #accessing }
PopupChoiceDialogWindowWithMessage >> message: aStringOrText [
	
	message := aStringOrText
]

{ #category : #actions }
PopupChoiceDialogWindowWithMessage >> newContentMorph [
	| top bottom |
	self textMorph: self newTextMorph.
	self text: self message.
	self iconMorph: self newIconMorph.
	top := self newRow: {self iconMorph. self textMorph}.
	bottom := super newContentMorph.
	^ self newGroupboxFor: (self newColumn: {top. bottom}).
]

{ #category : #actions }
PopupChoiceDialogWindowWithMessage >> newIconMorph [
	"Answer an icon for the receiver."
	
	^ImageMorph new form: self icon
]

{ #category : #actions }
PopupChoiceDialogWindowWithMessage >> newTextMorph [
	"Answer a text morph."
	
	^self newText: ''
]

{ #category : #accessing }
PopupChoiceDialogWindowWithMessage >> text: aStringOrText [
	"Set the text."
	
	|t|
	t := aStringOrText isString
		ifTrue: [aStringOrText asText addAttribute: (TextFontReference toFont: self textFont); yourself]
		ifFalse: [aStringOrText].
	t addAttribute: TextAlignment centered.
	self textMorph newContents: t
]

{ #category : #accessing }
PopupChoiceDialogWindowWithMessage >> textFont [
	"Answer the text font."
	
	^textFont
]

{ #category : #accessing }
PopupChoiceDialogWindowWithMessage >> textFont: aFont [
	"Set the text font."
	
	textFont :=  aFont
]

{ #category : #accessing }
PopupChoiceDialogWindowWithMessage >> textMorph [
	"Answer the value of textMorph"

	^ textMorph
]

{ #category : #accessing }
PopupChoiceDialogWindowWithMessage >> textMorph: anObject [
	"Set the value of textMorph"

	textMorph := anObject
]
