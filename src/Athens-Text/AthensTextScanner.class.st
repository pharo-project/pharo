"
I responsible for scanning the Text and transforming it into a list of AthensTextLine, where each line is a list of AthensTextDisplayCommand (sub)instances
"
Class {
	#name : #AthensTextScanner,
	#superclass : #Object,
	#instVars : [
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

{ #category : #accessing }
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
	
	currentStrip stop: position

]

{ #category : #accessing }
AthensTextScanner >> addCommand: aCommand [
	currentLine addCommand: aCommand
]

{ #category : #'text attributes' }
AthensTextScanner >> addEmphasis: emphasis [
	| fnt |
	
	fnt := currentFont ifNil: [ self defaultFont ] ifNotNil: [ currentFont font ].
	
	self setActualFont: (fnt emphasized: emphasis)
]

{ #category : #accessing }
AthensTextScanner >> applyAttributes [
	currentAttributes do: [:attr |
		attr emphasizeScanner: self ]. 
	
	currentFont ifNil: [ self setActualFont: self defaultFont ].
	currentColor ifNil: [ self textColor: self defaultColor ]
	
]

{ #category : #defaults }
AthensTextScanner >> defaultColor [
	^ Color black
]

{ #category : #defaults }
AthensTextScanner >> defaultFont [

	^ self textStyle defaultFont
]

{ #category : #defaults }
AthensTextScanner >> defaultTextStyle [

	^ TextStyle default
]

{ #category : #accessing }
AthensTextScanner >> endLine: index [

	currentLine endIndex: index
]

{ #category : #initialization }
AthensTextScanner >> initialize [
	lines := OrderedCollection new.
	currentAttributes := #().
]

{ #category : #accessing }
AthensTextScanner >> lines [
	^ lines
]

{ #category : #accessing }
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

{ #category : #accessing }
AthensTextScanner >> resetBackground [
	self textBackground: Color transparent
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

{ #category : #accessing }
AthensTextScanner >> setAttributes: anArray [ 
	currentBackground := nil.
	currentAttributes := anArray.
	
	"we should reset character string here"
	currentStrip := nil

]

{ #category : #'text attributes' }
AthensTextScanner >> setFont: fontNumber [

	self setActualFont: (self textStyle fontAt: fontNumber).
	
	
]

{ #category : #accessing }
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

{ #category : #accessing }
AthensTextScanner >> textStyle [
	^ textStyle ifNil: [ textStyle := self defaultTextStyle ]
]
