"
I am used to render font with cairo ""toy"" text api if no freetype
font is used.


AthensCairoSurfaceExamples exampleDrawText
"
Class {
	#name : #AthensStrikeFontRenderer,
	#superclass : #Object,
	#instVars : [
		'font',
		'canvas'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #'instance creation' }
AthensStrikeFontRenderer class >> on: aCanvas forFont: aFont [
	^ self new canvas: aCanvas; font: aFont; yourself
]

{ #category : #accessing }
AthensStrikeFontRenderer >> canvas: aCanvas [
	canvas := aCanvas
]

{ #category : #private }
AthensStrikeFontRenderer >> createCairoText: aString [
	| fdesc at |
	at := AthensCairoText new.
	at text: aString.
	fdesc := font asAthensFontDescription.
	at readFrom: fdesc.
	^ at
]

{ #category : #accessing }
AthensStrikeFontRenderer >> font [

	^ font
]

{ #category : #accessing }
AthensStrikeFontRenderer >> font: anObject [

	font := anObject
]

{ #category : #rendering }
AthensStrikeFontRenderer >> renderCharacters: aString from: start to: end [
	| cairoText |
	aString isEmpty
		ifTrue: [ ^ self ].
	cairoText := self createCairoText: (aString copyFrom: start to: end).
	canvas paint loadOnCairoCanvas: canvas.
	canvas newPath.
	cairoText drawOn: canvas
]
