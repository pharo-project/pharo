"
A label that underdraws to the top-left and/or bottom-right with a lighter and/or darker colour to the receiver.
"
Class {
	#name : #EmbossedStringMorph,
	#superclass : #StringMorph,
	#instVars : [
		'style',
		'trackPaneColor'
	],
	#category : #'Morphic-Base-Basic'
}

{ #category : #accessing }
EmbossedStringMorph >> adoptPaneColor: paneColor [
	"Set the color."
	
	(paneColor notNil and: [self trackPaneColor])
		ifTrue: [self color: paneColor].
	super adoptPaneColor: paneColor
]

{ #category : #drawing }
EmbossedStringMorph >> drawOn: aCanvas [
	"Draw the hi/lowlights too."

	|box|
	self style == #plain
		ifTrue: [^super drawOn: aCanvas].
	box := self bounds.
	(self style == #inset or: [self style == #insetNoHighlight])
		ifTrue: [self style == #insetNoHighlight
					ifFalse: [aCanvas drawString: self contents in: (box translateBy: 1)
								font: self fontToUse color: self color veryMuchLighter].
				aCanvas
					drawString: self contents in: (box translateBy: -1)
					font: self fontToUse color: self color muchDarker;
					drawString: self contents in: box
					font: self fontToUse color: self color]
		ifFalse: [self style == #raisedNoHighlight
					ifFalse: [aCanvas drawString: self contents in: (box translateBy: -1)
								font: self fontToUse color: self color veryMuchLighter].
				aCanvas
					drawString: self contents in: (box translateBy: 1)
					font: self fontToUse color: self color muchDarker;
					drawString: self contents in: box
					font: self fontToUse color: self color]
]

{ #category : #initialization }
EmbossedStringMorph >> initWithContents: aString font: aFont emphasis: emphasisCode [
	"Grrr, why do they do basicNew?"
	
	super initWithContents: aString font: aFont emphasis: emphasisCode.
	self
		style: #inset;
		trackPaneColor: true.
]

{ #category : #initialization }
EmbossedStringMorph >> initialize [
	"Initialize the receiver."
	
	super initialize.
	self
		style: #inset;
		trackPaneColor: true
]

{ #category : #accessing }
EmbossedStringMorph >> measureContents [
	"Measure the contents for fitting. Add 2@2 for hi/lowlights."

	^super measureContents + 2
]

{ #category : #accessing }
EmbossedStringMorph >> style [
	"Answer the value of style"

	^ style
]

{ #category : #accessing }
EmbossedStringMorph >> style: anObject [
	"Set the value of style"

	style := anObject.
	self changed
]

{ #category : #accessing }
EmbossedStringMorph >> styleSymbols [
	"Answer the valid styles."

	^#(plain inset insetNoHighlight raised raisedNoHighlight)
]

{ #category : #accessing }
EmbossedStringMorph >> trackPaneColor [
	"Answer the value of trackPaneColor"

	^ trackPaneColor
]

{ #category : #accessing }
EmbossedStringMorph >> trackPaneColor: anObject [
	"Set the value of trackPaneColor"

	trackPaneColor := anObject
]
