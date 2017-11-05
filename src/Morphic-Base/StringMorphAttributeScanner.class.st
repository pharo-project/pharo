"
A StringMorphAttributeScanner provides the interface of a CharacterScanner so that text attributes may be collected from a Text and used elsewhere, like in setting the attributes of a StringMorph.

"
Class {
	#name : #StringMorphAttributeScanner,
	#superclass : #Object,
	#instVars : [
		'fontNumber',
		'textColor',
		'emphasis',
		'alignment',
		'actualFont',
		'indent',
		'kern'
	],
	#category : #'Morphic-Base-Text Support'
}

{ #category : #accessing }
StringMorphAttributeScanner >> actualFont [
	"Answer the value of actualFont"

	^ actualFont ifNil: [ TextStyle defaultFont ]
]

{ #category : #scanning }
StringMorphAttributeScanner >> addEmphasis: anInteger [
	"Set the value of emphasis"

	emphasis := emphasis bitOr: anInteger
]

{ #category : #scanning }
StringMorphAttributeScanner >> addKern: kernDelta [
	"Set the current kern amount."
	kern := kern + kernDelta
]

{ #category : #accessing }
StringMorphAttributeScanner >> alignment [
	"Answer the value of alignment"

	^ alignment
]

{ #category : #accessing }
StringMorphAttributeScanner >> emphasis [
	"Answer the value of emphasis"

	^ emphasis
]

{ #category : #accessing }
StringMorphAttributeScanner >> font [
	"Answer the value of font"

	^(self textStyle fontAt: self fontNumber) copy
]

{ #category : #accessing }
StringMorphAttributeScanner >> fontNumber [
	"Answer the value of font"

	^ fontNumber
]

{ #category : #accessing }
StringMorphAttributeScanner >> indent [
	"Answer the value of indent"

	^ indent
]

{ #category : #scanning }
StringMorphAttributeScanner >> indentationLevel: anInteger [
	"Set the value of indent"

	indent := anInteger
]

{ #category : #initialization }
StringMorphAttributeScanner >> initialize [
	super initialize.
	emphasis := 0.
	indent := 0.
	kern := 0.
	fontNumber := 1.
	actualFont := TextStyle defaultFont
]

{ #category : #'string morph' }
StringMorphAttributeScanner >> initializeFromStringMorph: aStringMorph [

	fontNumber := 1.
	self setActualFont: (aStringMorph font ifNil: [ TextStyle defaultFont ]).
	
	emphasis := actualFont emphasis.
	textColor := aStringMorph color.

]

{ #category : #accessing }
StringMorphAttributeScanner >> kern [
	"Answer the value of kern"

	^ kern
]

{ #category : #scanning }
StringMorphAttributeScanner >> setActualFont: aFont [
	"Set the value of actualFont, from a TextFontReference"

	actualFont := aFont.
	aFont textStyle ifNotNil: [ :ts | fontNumber := ts fontIndexOf: aFont ]
]

{ #category : #scanning }
StringMorphAttributeScanner >> setAlignment: aSymbol [
	"Set the value of alignment"

	alignment := aSymbol
]

{ #category : #scanning }
StringMorphAttributeScanner >> setFont: fontNum [
	"Set the value of font"

	fontNumber := fontNum
]

{ #category : #accessing }
StringMorphAttributeScanner >> textColor [
	"Answer the value of textColor"

	^ textColor
]

{ #category : #scanning }
StringMorphAttributeScanner >> textColor: anObject [
	"Set the value of textColor"

	textColor := anObject
]

{ #category : #accessing }
StringMorphAttributeScanner >> textStyle [
	^self actualFont textStyle ifNil: [ TextStyle default ]
]
