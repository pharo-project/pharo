Class {
	#name : #RubCurrentLineBarDecorator,
	#superclass : #RubParagraphDecorator,
	#category : #'Rubric-Editing-Core'
}

{ #category : #querying }
RubCurrentLineBarDecorator class >> key [
	^ #selectionBar
]

{ #category : #drawing }
RubCurrentLineBarDecorator >> drawOn: aCanvas [
 
	| line bnd |
	
	aCanvas isShadowDrawing ifTrue: [ ^self ].
	
	self selectionStart = self selectionStop ifFalse: [^self].
	
	line := self selectionStart textLine.
	bnd := (line topLeft - (self margins left @ 0)) corner: (line bottomRight + (self margins right @ 0)).
	aCanvas fillRectangle: bnd color: self selectionBarColor
]

{ #category : #drawing }
RubCurrentLineBarDecorator >> drawOnAthensCanvas: anAthensCanvas [
	| line bnd |
	super drawOnAthensCanvas: anAthensCanvas.
	self selectionStart = self selectionStop ifFalse: [^self].
	line := self selectionStart textLine.
	bnd := (line topLeft - (self margins left @ 0)) corner: ((line bottomRight + (self margins right @ 0)) min: anAthensCanvas clipRect bottomRight).
	anAthensCanvas setPaint: self selectionBarColor.
	anAthensCanvas setShape: bnd.
	anAthensCanvas draw 

]

{ #category : #accessing }
RubCurrentLineBarDecorator >> selectionBarColor [
	^ self theme selectionBarColor
]
