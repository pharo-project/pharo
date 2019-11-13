"
I attach a bloc of code to a portion of text. The bloc is executed when the user clic on this portion of text.

---- exemple 1 -----
| stream |
stream := TextStream on: (Text new: 100).
stream 
	nextPutAll:  'Start a ';
	withAttributes: {TextEmphasis underlined. TextAction new actOnClickBlock: [Smalltalk tools browser open ]}
	do: [ stream nextPutAll: 'browser' ].
TextMorph new 
	newContents: stream contents;
	openInWindow .
---- exemple 2 -----
| stream |
stream := TextStream on: (Text new: 100).
stream 
	withAttributes: {TextAction new actOnClickBlock: [:attr :event :textMorph :parag :editor | 		
		textMorph owner color: Color white.
		parag rightFlush.
		editor flash]}
	do: [ stream nextPutAll: 'Change me' ].
TextMorph new 
	newContents: stream contents;
	openInWindow .
"
Class {
	#name : #TextAction,
	#superclass : #TextAttribute,
	#instVars : [
		'actOnClickBlock'
	],
	#classVars : [
		'ActOnClickBlock'
	],
	#category : #'Text-Core-Attributes'
}

{ #category : #accessing }
TextAction class >> actOnClickBlock [
	^ ActOnClickBlock
]

{ #category : #accessing }
TextAction class >> actOnClickBlock: aBlock [
	ActOnClickBlock := aBlock
]

{ #category : #'class initialization' }
TextAction class >> initialize [
	ActOnClickBlock := [ ]
]

{ #category : #evaluating }
TextAction >> actOnClick: anEvent for: anObject in: paragraph editor: editor [
	"sent when a user clicks on a piece of text to which I am applied in an editor"
	"may be self is included in the event or an Object. "
	^ actOnClickBlock valueWithEnoughArguments: (Array with: self with: anEvent with: anObject with: paragraph with: editor)
]

{ #category : #accessing }
TextAction >> actOnClickBlock [
	^ actOnClickBlock
]

{ #category : #accessing }
TextAction >> actOnClickBlock: aBlock [
	actOnClickBlock := aBlock
]

{ #category : #'event handling' }
TextAction >> actOnMove: anEvent for: target in: aParagraph editor: anEditor [
	"override in subclasses"
	^ false
]

{ #category : #testing }
TextAction >> analyze: aString [
	"Analyze the selected text to find both the parameter to store and the text to emphesize (may be different from original selection).  Does not return self!.  May be of the form:
3+4
<3+4>
Click Here<3+4>
<3+4>Click Here
"
	"Obtain the showing text and the instructions"
	| b1 b2 trim param show |
	b1 := aString indexOf: $<.
	b2 := aString indexOf: $>.
	(b1 < b2) & (b1 > 0) ifFalse: ["only one part"
		param := self validate: aString.
		param ifNil: [ ^{ nil. nil } ].
		^ Array with: param with: (param ifEmpty: [nil] ifNotEmpty: [param])].
	"Two parts"
	trim := aString trimBoth.
	(trim at: 1) == $< 
		ifTrue: [(trim last) == $>
			ifTrue: ["only instructions" 
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param ifEmpty: [nil] ifNotEmpty: [param]]
			ifFalse: ["at the front"
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param ifEmpty: [nil] ifNotEmpty: [aString copyFrom: b2+1 to: aString size]]]
		ifFalse: [(trim last) == $>
			ifTrue: ["at the end"
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param ifEmpty: [nil] ifNotEmpty: [aString copyFrom: 1 to: b1-1]]
			ifFalse: ["Illegal -- <> has text on both sides"
				show := nil]].
	^ Array with: param with: show
]

{ #category : #testing }
TextAction >> dominatedByCmd0 [
	"Cmd-0 should turn off active text"
	^ true
]

{ #category : #accessing }
TextAction >> info [
	^ 'no hidden info'
]

{ #category : #initialization }
TextAction >> initialize [
	super initialize.
	actOnClickBlock := ActOnClickBlock.
]

{ #category : #testing }
TextAction >> mayActOnEvent: anEvent [

	^ true
]

{ #category : #testing }
TextAction >> validate: aString [
	"any format is OK with me"
	^ aString
]
