"
Border with customisable inner margin.
"
Class {
	#name : #MarginBorder,
	#superclass : #SimpleBorder,
	#instVars : [
		'margin'
	],
	#category : #'Morphic-Base-Borders'
}

{ #category : #accessing }
MarginBorder >> drawLineFrom: startPoint to: stopPoint on: aCanvas [
	"Reduce the width by the margin."
	
	| lineColor |
	lineColor := (stopPoint truncated quadrantOf: startPoint truncated) > 2 
				ifTrue: [self topLeftColor]
				ifFalse: [self bottomRightColor].
	aCanvas 
		line: startPoint
		to: stopPoint 
		width: (self width - self margin max: 0)
		color: lineColor
]

{ #category : #accessing }
MarginBorder >> frameRectangle: aRectangle on: aCanvas [
	"Reduce width by the margin."
	
	aCanvas frameAndFillRectangle: aRectangle
		fillColor: Color transparent
		borderWidth: (self width - self margin max: 0)
		topLeftColor: self topLeftColor
		bottomRightColor: self bottomRightColor.
]

{ #category : #initialization }
MarginBorder >> initialize [
	"Initialize the receiver."

	super initialize.
	self
		margin: 0
]

{ #category : #accessing }
MarginBorder >> margin [
	"Answer the value of margin"

	^ margin
]

{ #category : #accessing }
MarginBorder >> margin: anObject [
	"Set the value of margin"

	margin := anObject
]
