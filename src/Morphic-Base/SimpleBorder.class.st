"
see BorderedMorph
"
Class {
	#name : #SimpleBorder,
	#superclass : #BorderStyle,
	#instVars : [
		'baseColor',
		'color',
		'width'
	],
	#category : #Morphic-Base-Borders
}

{ #category : #accessing }
SimpleBorder >> baseColor [
	^baseColor ifNil:[Color transparent]
]

{ #category : #accessing }
SimpleBorder >> baseColor: aColor [
	| cc |
	cc := aColor isTransparent ifTrue:[nil] ifFalse:[aColor].
	baseColor = cc ifTrue:[^self].
	baseColor := cc.
	self releaseCachedState.
	self color: cc.

]

{ #category : #accessing }
SimpleBorder >> bottomRightColor [
	"Changed from direct access to color since, if nil,
	self color is transparent."

	^self color
]

{ #category : #accessing }
SimpleBorder >> color [
	^color ifNil:[Color transparent]
]

{ #category : #accessing }
SimpleBorder >> color: aColor [
	color = aColor ifTrue:[^self].
	color := aColor.
	self releaseCachedState.
]

{ #category : #drawing }
SimpleBorder >> drawLineFrom: startPoint to: stopPoint on: aCanvas [ 
	| lineColor |
	lineColor := (stopPoint truncated quadrantOf: startPoint truncated) > 2 
				ifTrue: [self topLeftColor]
				ifFalse: [self bottomRightColor].
	aCanvas 
		line: startPoint
		to: stopPoint 
		width: self width
		color: lineColor
]

{ #category : #drawing }
SimpleBorder >> frameRectangle: aRectangle on: aCanvas [
	aCanvas frameAndFillRectangle: aRectangle
		fillColor: Color transparent
		borderWidth: self width
		topLeftColor: self topLeftColor
		bottomRightColor: self bottomRightColor.
]

{ #category : #initialization }
SimpleBorder >> initialize [

	super initialize.
	width := 1.
	color := Color transparent
	
]

{ #category : #accessing }
SimpleBorder >> style [
	^#simple
]

{ #category : #accessing }
SimpleBorder >> topLeftColor [
	"Changed from direct access to color since, if nil,
	self color is transparent."

	^self color
]

{ #category : #accessing }
SimpleBorder >> width [
	^width
]

{ #category : #accessing }
SimpleBorder >> width: aNumber [
	width = aNumber ifTrue:[^self].
	width := aNumber truncated max: (width isPoint ifTrue:[0@0] ifFalse:[0]).
	self releaseCachedState.
]
