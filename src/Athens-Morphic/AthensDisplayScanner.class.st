"
An AthensDisplayScanner displays the paragraphs text on an AthensCanvas.
"
Class {
	#name : #AthensDisplayScanner,
	#superclass : #DisplayScanner,
	#instVars : [
		'canvas'
	],
	#category : #'Athens-Morphic'
}

{ #category : #'instance creation' }
AthensDisplayScanner class >> for: aParagraph on: anAthensCanvas [
	^ self new initWithParagraph: aParagraph andCanvas: anAthensCanvas; yourself
]

{ #category : #'instance creation' }
AthensDisplayScanner class >> on: anAthensCanvas [
	^ self new initWith: anAthensCanvas; yourself
]

{ #category : #displaying }
AthensDisplayScanner >> displayString: string from: startIndex to: stopIndex at: aPoint [
	canvas pathTransform
		restoreAfter: [
			canvas pathTransform translateBy: aPoint + (0@font ascent).
			canvas drawString: string from: startIndex to: stopIndex ]
]

{ #category : #displaying }
AthensDisplayScanner >> fillTextBackground [
	backgroundColor ifNotNil: [ canvas drawShape: (line left @ lineY extent: line width @ line lineHeight) ]
]

{ #category : #initialization }
AthensDisplayScanner >> initWithParagraph: aParagraph andCanvas: anAthensCanvas [
	self text: aParagraph text textStyle: aParagraph textStyle.
	canvas := anAthensCanvas
]

{ #category : #initialization }
AthensDisplayScanner >> initialize [
	super initialize
]

{ #category : #displaying }
AthensDisplayScanner >> setFont [
	super setFont.
	canvas setFont:font.
	canvas setPaint: foregroundColor
]
