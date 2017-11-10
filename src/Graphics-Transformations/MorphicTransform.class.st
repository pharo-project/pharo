"
This class implements simple translation, scaling and rotation for points, as well as inverse transformations.  These transformations are used in TransformMorphs (clipping scrollers) and TransformationMorphs (general flex-morph wrappers) to map, eg, global mouse coords into local coords, and to invert, eg, local damage rectangles into global damage rectangles.
"
Class {
	#name : #MorphicTransform,
	#superclass : #DisplayTransform,
	#instVars : [
		'offset',
		'angle',
		'scale'
	],
	#category : #Graphics-Transformations
}

{ #category : #'instance creation' }
MorphicTransform class >> identity [

	^ self offset: 0@0 angle: 0.0 scale: 1.0
]

{ #category : #'instance creation' }
MorphicTransform class >> new [

	^ self offset: 0@0

]

{ #category : #'instance creation' }
MorphicTransform class >> offset: aPoint [

	^ self offset: aPoint angle: 0.0 scale: 1.0
]

{ #category : #'instance creation' }
MorphicTransform class >> offset: aPoint angle: radians scale: aNumberOrPoint [

	^ self basicNew setOffset: aPoint angle: radians scale: aNumberOrPoint
]

{ #category : #accessing }
MorphicTransform >> angle [
	^ angle
]

{ #category : #converting }
MorphicTransform >> asMatrixTransform2x3 [
	^((MatrixTransform2x3 withRotation: angle radiansToDegrees negated) composedWithLocal:
		(MatrixTransform2x3 withScale: scale))
			offset: offset negated
]

{ #category : #converting }
MorphicTransform >> asMorphicTransform [

	^ self
]

{ #category : #transformations }
MorphicTransform >> composedWith: aTransform [
	"Return a new transform that has the effect of transforming points first by the receiver and then by the argument."

	self isIdentity ifTrue: [^ aTransform].
	aTransform isIdentity ifTrue: [^ self].
	^ CompositeTransform new globalTransform: self
							localTransform: aTransform
]

{ #category : #composing }
MorphicTransform >> composedWithLocal: aTransform [
	aTransform isIdentity ifTrue:[^self].
	self isIdentity ifTrue:[^aTransform].
	aTransform isMorphicTransform ifFalse:[^super composedWithLocal: aTransform].
	self isPureTranslation ifTrue:[
		^aTransform withOffset: aTransform offset + self offset].
	aTransform isPureTranslation ifTrue:[
		^self withOffset: (self localPointToGlobal: aTransform offset negated) negated].
	^super composedWithLocal: aTransform.
]

{ #category : #'transforming points' }
MorphicTransform >> globalPointToLocal: aPoint [
	"Transform aPoint from global coordinates into local coordinates"
	^self transform: aPoint
]

{ #category : #accessing }
MorphicTransform >> inverseTransformation [
	"Return the inverse transformation of the receiver"
	^MorphicTransform
		offset: (self transform: 0@0) - (self transform: offset)
		angle: angle negated
		scale: scale reciprocal
]

{ #category : #transformations }
MorphicTransform >> invert: aPoint [ 
	"Transform the given point from local to global coordinates."
	| p3 p2 |
	self isPureTranslation ifTrue: [ ^ aPoint - offset ].
	p3 := aPoint * scale.
	p2 := (p3 x * angle cos + (p3 y * angle sin)) @ (p3 y * angle cos - (p3 x * angle sin)).
	^ p2 - offset
]

{ #category : #transformations }
MorphicTransform >> invertBoundsRect: aRectangle [ 
	"Return a rectangle whose coordinates have been transformed
	from local back to global coordinates.  NOTE: if the transformation
	is not just a translation, then it will compute the bounding box
	in global coordinates."
	| outerRect |
	self isPureTranslation 
		ifTrue: 
			[ ^ (self invert: aRectangle topLeft) corner: (self invert: aRectangle bottomRight) ]
		ifFalse: 
			[ outerRect := Rectangle encompassing: (aRectangle innerCorners collect: [ :p | self invert: p ]).
			"Following asymmetry due to likely subsequent truncation"
			^ outerRect topLeft - (1 @ 1) corner: outerRect bottomRight + (2 @ 2) ]
]

{ #category : #transformations }
MorphicTransform >> invertRect: aRectangle [

	self error: 'method name changed to emphasize enclosing bounds'.
	^ self invertBoundsRect: aRectangle
]

{ #category : #testing }
MorphicTransform >> isIdentity [
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."

	^ self isPureTranslation and: [offset = (0@0)]

]

{ #category : #testing }
MorphicTransform >> isMorphicTransform [
	^true
]

{ #category : #testing }
MorphicTransform >> isPureTranslation [
	"Return true if the receiver specifies no rotation or scaling."

	^ angle = 0.0 and: [scale = 1.0]

]

{ #category : #'transforming points' }
MorphicTransform >> localPointToGlobal: aPoint [
	"Transform aPoint from global coordinates into local coordinates"
	^self invert: aPoint
]

{ #category : #accessing }
MorphicTransform >> offset [
	^ offset

]

{ #category : #printing }
MorphicTransform >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut:$(;
		nextPutAll:'angle = '; print: angle;
		nextPutAll:'; scale = '; print: scale;
		nextPutAll:'; offset = '; print: offset;
		nextPut:$).
]

{ #category : #accessing }
MorphicTransform >> scale [
	^ scale
]

{ #category : #private }
MorphicTransform >> setAngle: radians [
	angle := radians
]

{ #category : #initialize }
MorphicTransform >> setIdentiy [
	scale := 1.0.
	offset := 0 @ 0.
	angle := 0.0
]

{ #category : #private }
MorphicTransform >> setOffset: aPoint [ 
	offset := aPoint
]

{ #category : #private }
MorphicTransform >> setOffset: aPoint angle: radians scale: aNumberOrPoint [
	offset := aPoint.
	angle := radians.
	scale := aNumberOrPoint
]

{ #category : #private }
MorphicTransform >> setScale: aNumberOrPoint [ 
	scale := aNumberOrPoint
]

{ #category : #transformations }
MorphicTransform >> transform: aPoint [ 
	"Transform the given point from global to local coordinates."
	| p2 p3 |
	self isPureTranslation ifTrue: [ ^ aPoint + offset ].
	p2 := aPoint + offset.
	p3 := (p2 x * angle cos - (p2 y * angle sin)) @ (p2 y * angle cos + (p2 x * angle sin)) / scale.
	^ p3
]

{ #category : #transformations }
MorphicTransform >> transformBoundsRect: aRectangle [ 
	"Return a rectangle whose coordinates have been transformed
	from global to local coordinates.  NOTE: if the transformation
	is not just a translation, then it will compute the bounding box
	in global coordinates."
	| outerRect |
	self isPureTranslation 
		ifTrue: 
			[ ^ (self transform: aRectangle topLeft) corner: (self transform: aRectangle bottomRight) ]
		ifFalse: 
			[ outerRect := Rectangle encompassing: (aRectangle innerCorners collect: [ :p | self transform: p ]).
			"Following asymmetry due to likely subsequent truncation"
			^ outerRect topLeft - (1 @ 1) corner: outerRect bottomRight + (2 @ 2) ]
]

{ #category : #accessing }
MorphicTransform >> withAngle: radians [
	^ self copy setAngle: radians
]

{ #category : #accessing }
MorphicTransform >> withOffset: aPoint [
	^ self copy setOffset: aPoint
]

{ #category : #accessing }
MorphicTransform >> withScale: aNumberOrPoint [
	^ self copy setScale: aNumberOrPoint
]
