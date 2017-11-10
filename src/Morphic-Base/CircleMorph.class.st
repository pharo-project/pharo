"
I am a specialization of EllipseMorph that knows enough to remain circular.

"
Class {
	#name : #CircleMorph,
	#superclass : #EllipseMorph,
	#instVars : [
		'referencePosition',
		'originalCenter',
		'originalAngle'
	],
	#category : #Morphic-Base-Basic
}

{ #category : #example }
CircleMorph class >> exampleCircleMorph [
	(CircleMorph new)
	 extent: 50 @ 50;color: Color blue;
	 openInWorld.
]

{ #category : #geometry }
CircleMorph >> bounds: aRectangle [
	| size |
	size := aRectangle width min: aRectangle height.
	super bounds: (Rectangle origin: aRectangle origin extent: size @ size).
]

{ #category : #initialization }
CircleMorph >> defaultBounds [

	^0@0 extent: 40@40
]

{ #category : #initialization }
CircleMorph >> defaultColor [

	^Color green lighter
]

{ #category : #geometry }
CircleMorph >> extent: aPoint [
	| size oldRotationCenter |
	oldRotationCenter := self rotationCenter.
	size := aPoint x min: aPoint y.
	super extent: size @ size.
	self rotationCenter: oldRotationCenter.
]

{ #category : #initialization }
CircleMorph >> initialize [
	super initialize. 
	referencePosition := self center.

]

{ #category : #accessing }
CircleMorph >> originalCenter [
	^ originalCenter
]

{ #category : #accessing }
CircleMorph >> originalCenter: anObject [
	originalCenter := anObject
]

{ #category : #'rotate scale and flex' }
CircleMorph >> prepareForRotating [
	"When rotating from a halo, I can do this without a flex shell"

	^ self

]

{ #category : #'rotate scale and flex' }
CircleMorph >> prepareForScaling [
	"When scaling from a halo, I can do this without a flex shell"

	^ self

]

{ #category : #'rotate scale and flex' }
CircleMorph >> privateMoveBy: delta [
	referencePosition := referencePosition + delta.
	originalCenter := (originalCenter ifNil: [ self center ]) + delta.
	super privateMoveBy: delta
]

{ #category : #'geometry etoy' }
CircleMorph >> referencePosition [
	"Return the current reference position of the receiver"

	^ referencePosition
]

{ #category : #'geometry etoy' }
CircleMorph >> rotationCenter [
	"Return the rotation center of the receiver. The rotation center defines the relative offset inside the receiver's bounds for locating the reference position."
	| refPos |
	refPos := self referencePosition.
	^ (refPos - self bounds origin) / self bounds extent asFloatPoint
]

{ #category : #'geometry etoy' }
CircleMorph >> rotationCenter: aPointOrNil [
	"Set the new rotation center of the receiver. The rotation center defines the relative offset inside the receiver's bounds for locating the reference position."

	| newRef box |
	aPointOrNil
		ifNotNil: [ box := self bounds.
			newRef := box origin + (aPointOrNil * box extent).
			self setRotationCenterFrom: newRef ]
]

{ #category : #'rotate scale and flex' }
CircleMorph >> rotationDegrees [

	^ self forwardDirection
]

{ #category : #'rotate scale and flex' }
CircleMorph >> rotationDegrees: degrees [
	| ref newPos flex origAngle origCenter |
	ref := self referencePosition.
	origAngle := originalAngle ifNil: [ self heading ].
	origCenter := originalCenter ifNil: [ self center ].
	flex := (MorphicTransform offset: ref negated)
		withAngle: (degrees - origAngle) degreesToRadians.
	newPos := (flex transform: origCenter) - flex offset.
	self position: (self position + newPos - self center) asIntegerPoint.
	referencePosition := ref.
	originalAngle := origAngle.
	originalCenter := origCenter.
	self forwardDirection: degrees.
	self changed
]

{ #category : #menus }
CircleMorph >> setRotationCenterFrom: aPoint [
	"Called by halo rotation code.
	Circles store their referencePosition."

	referencePosition := aPoint.
	originalCenter := self center.
	originalAngle := self heading
]

{ #category : #geometry }
CircleMorph >> transformedBy: aTransform [
	aTransform isIdentity ifTrue:[^self].
	^self center: (aTransform localPointToGlobal: self center).

]
