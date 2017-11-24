"
I responsible for scanning the Text and transforming it into a list of AthensTextLine, where each line is a list of AthensTextDisplayCommand (sub)instances
"
Class {
	#name : #AthensTextScanner,
	#superclass : #Object,
	#instVars : [
		'paragraph',
		'text',
		'currentLine',
		'lines',
		'currentAttributes',
		'currentFont',
		'currentColor',
		'textStyle',
		'currentStrip',
		'currentBackground'
	],
	#category : #'Athens-Text'
}

{ #category : #'as yet unclassified' }
AthensTextScanner >> addCharacter: position [
	| charWidth fnt |

	currentStrip ifNil: [

		self applyAttributes.
		
		fnt := currentFont font.
		currentLine maxAscent: fnt getPreciseAscent.
		currentLine maxDescent: fnt getPreciseDescent.
		currentLine maxHeight: fnt getPreciseHeight.
		
		currentStrip := AthensCharacterSpan new.
		currentLine addCommand: currentStrip.
		currentStrip start: position.
	].
	
	charWidth := currentFont font getGlyphWidth: (text at: position).
	currentLine addWidth: charWidth.
	currentBackground ifNotNil: [
		currentBackground addWidth: charWidth ].
	
	currentStrip stop: position.

]

{ #category : #'as yet unclassified' }
AthensTextScanner >> addCommand: aCommand [
	currentLine addCommand: aCommand
]

{ #category : #'text attributes' }
AthensTextScanner >> addEmphasis: emphasis [
	| fnt |
	
	fnt := currentFont ifNil: [ self defaultFont ] ifNotNil: [ currentFont font ].
	
	self setActualFont: (fnt emphasized: emphasis)
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> applyAttributes [
	currentAttributes do: [:attr |
		attr emphasizeScanner: self ]. 
	
	currentFont ifNil: [ self setActualFont: self defaultFont ].
	currentColor ifNil: [ self textColor: self defaultColor ].
	
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> defaultColor [
	^ Color black
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> defaultFont [

	^ self textStyle defaultFont
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> defaultTextStyle [

	^ TextStyle default
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> endLine: index [

	currentLine  endIndex: index
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> initialize [
	lines := OrderedCollection new.
	currentAttributes := #().
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> lines [
	^ lines
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> newLine: startingIndex [ 

	currentLine := lines add: (AthensTextLine new text: text;
		startIndex: startingIndex; yourself).

"	currentStrip ifNotNil: [
		
		currentLine addCommand: currentFont copy.
		currentLine addCommand: currentColor copy.

		currentBackground ifNotNil: [
			currentBackground := currentBackground copy start: 0; width: 0. 
			currentLine addCommand: currentBackground ].
	].
"
"reset everything, to make sure all attributes will be reapplied for new line"
	
	currentColor := currentBackground := currentFont := currentStrip := nil.
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> resetBackground [
	self textBackground: Color transparent.
]

{ #category : #'text attributes' }
AthensTextScanner >> setActualFont: aFont [

	currentFont ifNil: [
		
		currentFont := AthensFontChange new font: aFont.
		self addCommand: currentFont.
		^ self 
	].

	aFont = currentFont font ifFalse: [
		currentFont := AthensFontChange new font: aFont.
		self addCommand: currentFont.
	].

]

{ #category : #'as yet unclassified' }
AthensTextScanner >> setAttributes: anArray [ 
	currentBackground := nil.
	currentAttributes := anArray.
	
	"we should reset character string here"
	currentStrip := nil.

]

{ #category : #'text attributes' }
AthensTextScanner >> setFont: fontNumber [

	self setActualFont: (self textStyle fontAt: fontNumber).
	
	
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> text: aText [ 
	text := aText
]

{ #category : #'text attributes' }
AthensTextScanner >> textBackground: aColor [

	"background is transparent by default "
	
	(currentBackground notNil and: [ currentBackground color = aColor ])
		ifTrue: [ ^ self].
		
	currentBackground ifNil: [ aColor isTransparent ifTrue: [ ^ self ] ].
	
	currentBackground := AthensBackgroundChange new color: aColor.
	currentBackground start: currentLine width.
	currentLine addCommand: currentBackground.
]

{ #category : #'text attributes' }
AthensTextScanner >> textColor: aColor [

	currentColor ifNil: [
		currentColor := AthensColorChange new color: aColor.
		self addCommand: currentColor.
		^ self 
	].

	currentColor color = aColor ifFalse: [
		currentColor := AthensColorChange new color: aColor.
		self addCommand: currentColor.	
	].
	
]

{ #category : #'as yet unclassified' }
AthensTextScanner >> textStyle [
	^ textStyle ifNil: [ textStyle := self defaultTextStyle ]
]
