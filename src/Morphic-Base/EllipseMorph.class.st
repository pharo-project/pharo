"
A round BorderedMorph. Supports borderWidth and borderColor. 
Only simple borderStyle is implemented.

EllipseMorph new borderWidth:10; borderColor: Color green; openInWorld.
EllipseMorph new borderStyle:(SimpleBorder width: 5 color: Color blue); openInWorld.
"
Class {
	#name : #EllipseMorph,
	#superclass : #BorderedMorph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#category : #'Morphic-Base-Basic'
}

{ #category : #drawing }
EllipseMorph >> areasRemainingToFill: aRectangle [
	"Could be improved by quick check of inner rectangle"

	^ Array with: aRectangle
]

{ #category : #geometry }
EllipseMorph >> bottomLeftCorner [
	^self intersectionWithLineSegmentFromCenterTo: bounds bottomLeft 

]

{ #category : #geometry }
EllipseMorph >> bottomRightCorner [
	^self intersectionWithLineSegmentFromCenterTo: bounds bottomRight 

]

{ #category : #testing }
EllipseMorph >> canDrawBorder: aBorderStyle [
	^aBorderStyle style == #simple
]

{ #category : #geometry }
EllipseMorph >> closestPointTo: aPoint [
	^self intersectionWithLineSegmentFromCenterTo: aPoint
]

{ #category : #'geometry testing' }
EllipseMorph >> containsPoint: aPoint [

	| radius other delta xOverY |
	(bounds containsPoint: aPoint) ifFalse: [^ false].  "quick elimination"
	(bounds width = 1 or: [bounds height = 1])
		ifTrue: [^ true].  "Degenerate case -- code below fails by a bit"

	radius := bounds height asFloat / 2.
	other := bounds width asFloat / 2.
	delta := aPoint - bounds topLeft - (other@radius).
	xOverY := bounds width asFloat / bounds height asFloat.
	^ (delta x asFloat / xOverY) squared + delta y squared <= radius squared
]

{ #category : #rounding }
EllipseMorph >> cornerStyle: aSymbol [
	"Set the receiver's corner style.  But, in this case, do *not*"

	(extension isNil or: [self cornerStyle == aSymbol]) ifTrue: [^self].
	extension cornerStyle: nil.
	self changed
]

{ #category : #accessing }
EllipseMorph >> couldHaveRoundedCorners [
	^ false
]

{ #category : #initialization }
EllipseMorph >> defaultBorderWidth [
	"answer the default border width for the receiver"
	^ 1
]

{ #category : #initialization }
EllipseMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Color yellow
]

{ #category : #accessing }
EllipseMorph >> doesBevels [
	^ false
]

{ #category : #drawing }
EllipseMorph >> drawDropShadowOn: aCanvas [ 

	aCanvas fillOval: bounds fillStyle: self shadowColor borderWidth: 0 borderColor: nil
]

{ #category : #drawing }
EllipseMorph >> drawOn: aCanvas [ 
	
	aCanvas 
		fillOval: bounds 
		fillStyle: self fillStyle 
		borderWidth: borderWidth  
		borderColor: borderColor.

]

{ #category : #geometry }
EllipseMorph >> intersectionWithLineSegmentFromCenterTo: aPoint [ 
	| dx aSquared bSquared m mSquared xSquared x y dy |
	(self containsPoint: aPoint)
		ifTrue: [ ^aPoint ].
	dx := aPoint x - self center x.
	dy := aPoint y - self center y.
	dx = 0
		ifTrue: [ ^self bounds pointNearestTo: aPoint ].
	m := dy / dx.
	mSquared := m squared.
	aSquared := (self bounds width / 2) squared.
	bSquared := (self bounds height / 2) squared.
	xSquared := 1 / ((1 / aSquared) + (mSquared / bSquared)).
	x := xSquared sqrt.
	dx < 0 ifTrue: [ x := x negated ].
	y := m * x.
	^ self center + (x @ y) asIntegerPoint.

]

{ #category : #geometry }
EllipseMorph >> topLeftCorner [
	^self intersectionWithLineSegmentFromCenterTo: bounds topLeft 

]

{ #category : #geometry }
EllipseMorph >> topRightCorner [
	^self intersectionWithLineSegmentFromCenterTo: bounds topRight

]
