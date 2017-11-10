"
Identity transformation
"
Class {
	#name : #IdentityTransform,
	#superclass : #DisplayTransform,
	#classVars : [
		'Default'
	],
	#category : #Graphics-Transformations
}

{ #category : #initialization }
IdentityTransform class >> initialize [
	"IdentityTransform initialize"
	Default := self basicNew
]

{ #category : #'instance creation' }
IdentityTransform class >> new [
	"There can be only one"
	^Default
]

{ #category : #accessing }
IdentityTransform >> angle [
	^ 0.0
]

{ #category : #converting }
IdentityTransform >> asMatrixTransform2x3 [
	"Represent the receiver as a 2x3 matrix transformation"
	^MatrixTransform2x3 identity
]

{ #category : #composing }
IdentityTransform >> composedWith: aTransform [
	^aTransform
]

{ #category : #composing }
IdentityTransform >> composedWithGlobal: aTransformation [
	^aTransformation
]

{ #category : #composing }
IdentityTransform >> composedWithLocal: aTransformation [
	^aTransformation
]

{ #category : #'transforming rects' }
IdentityTransform >> globalBoundsToLocal: aRectangle [
	"Transform aRectangle from global coordinates into local coordinates"
	^aRectangle
]

{ #category : #'transforming points' }
IdentityTransform >> globalPointToLocal: aPoint [
	"Transform aPoint from global coordinates into local coordinates"
	^aPoint
]

{ #category : #'transforming points' }
IdentityTransform >> globalPointsToLocal: inArray [
	"Transform all the points of inArray from global into local coordinates"
	^inArray
]

{ #category : #accessing }
IdentityTransform >> inverseTransformation [
	"Return the inverse transformation of the receiver"
	^self
]

{ #category : #'transforming points' }
IdentityTransform >> invertBoundsRect: aRectangle [
	"Return a rectangle whose coordinates have been transformed
	from local back to global coordinates. Since I am the identity matrix
	no transformation is made."

	^aRectangle

]

{ #category : #testing }
IdentityTransform >> isIdentity [
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."
	^true
]

{ #category : #testing }
IdentityTransform >> isPureTranslation [
	"Return true if the receiver specifies no rotation or scaling."
	^true
]

{ #category : #'transforming rects' }
IdentityTransform >> localBoundsToGlobal: aRectangle [
	"Transform aRectangle from local coordinates into global coordinates"
	^aRectangle
]

{ #category : #'transforming points' }
IdentityTransform >> localPointToGlobal: aPoint [
	"Transform aPoint from local coordinates into global coordinates"
	^aPoint
]

{ #category : #'transforming points' }
IdentityTransform >> localPointsToGlobal: inArray [
	"Transform all the points of inArray from local into global coordinates"
	^inArray
]

{ #category : #accessing }
IdentityTransform >> offset [
	^0@0
]

{ #category : #initialize }
IdentityTransform >> setIdentity [
	"I *am* the identity transform"
	^self
]

{ #category : #'transforming rects' }
IdentityTransform >> sourceQuadFor: aRectangle [
	^ aRectangle innerCorners
]
