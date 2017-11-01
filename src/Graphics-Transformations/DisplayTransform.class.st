"
This class represents a base for generic transformations of 2D points between different coordinate systems (including scaling and rotation). The transformations map objects between one coordinate system and another where it is assumed that a nested hierarchy of transformations can be defined.

It is assumed that transformations deal with Integer points. All transformations should return Integer coordinates (even though float points may be passed in as argument).

Compositions of transformations MUST work in the following order. A 'global' transformation (the argument in #composedWithGlobal:) is defined as a transformation that takes place between the receiver (the 'local') transformation and any 'global' point computations, whereas a 'local' transformation (e.g., the argument in #composedWithLocal:) takes place between the receiver ('global') and any 'local' points. For the transformation methods this means that combining a global and a local transformation will result in the following order:

		globalPointToLocal: globalPoint
			""globalPoint -> globalTransform -> localTransform -> locaPoint""
			^localTransform globalPointToLocal:
				(globalTransform globalPointToLocal: globalPoint)

		localPointToGlobal: localPoint
			""localPoint -> localTransform -> globalTransform -> globalPoint""
			^globalTransform localPointToGlobal:
				(localTransform localPointToGlobal: localPoint)


"
Class {
	#name : #DisplayTransform,
	#superclass : #Object,
	#category : 'Graphics-Transformations'
}

{ #category : #'instance creation' }
DisplayTransform class >> identity [
	^self new setIdentity
]

{ #category : #converting }
DisplayTransform >> asCompositeTransform [
	"Represent the receiver as a composite transformation"
	^CompositeTransform new
		globalTransform: self
		localTransform: self species identity
]

{ #category : #converting }
DisplayTransform >> asMatrixTransform2x3 [
	"Represent the receiver as a 2x3 matrix transformation"
	^self subclassResponsibility
]

{ #category : #composing }
DisplayTransform >> composedWithGlobal: aTransformation [
	"Return the composition of the receiver and the global transformation passed in.
	A 'global' transformation is defined as a transformation that takes place
	between the receiver (the 'local') transformation and any 'global' point
	computations, e.g., for the methods

		globalPointToLocal: globalPoint
			globalPoint -> globalTransform -> localTransform -> locaPoint

		localPointToGlobal: localPoint
			localPoint -> localTransform -> globalTransform -> globalPoint

		"
	^aTransformation composedWithLocal: self
]

{ #category : #composing }
DisplayTransform >> composedWithLocal: aTransformation [
	"Return the composition of the receiver and the local transformation passed in.
	A 'local' transformation is defined as a transformation that takes place
	between the receiver (the 'global') transformation and any 'local' point
	computations, e.g., for the methods

		globalPointToLocal: globalPoint
			globalPoint -> globalTransform -> localTransform -> locaPoint

		localPointToGlobal: localPoint
			localPoint -> localTransform -> globalTransform -> globalPoint

		"
	self isIdentity ifTrue:[^ aTransformation].
	aTransformation isIdentity ifTrue:[^ self].
	^ CompositeTransform new globalTransform: self
							localTransform: aTransformation
]

{ #category : #'transforming rects' }
DisplayTransform >> globalBoundsToLocal: aRectangle [
	"Transform aRectangle from global coordinates into local coordinates"
	^Rectangle encompassing: (self globalPointsToLocal: aRectangle corners)
]

{ #category : #'transforming points' }
DisplayTransform >> globalPointToLocal: aPoint [
	"Transform aPoint from global coordinates into local coordinates"
	^self subclassResponsibility
]

{ #category : #'transforming points' }
DisplayTransform >> globalPointsToLocal: inArray [
	"Transform all the points of inArray from global into local coordinates"
	^inArray collect:[:pt| self globalPointToLocal: pt]
]

{ #category : #accessing }
DisplayTransform >> inverseTransformation [
	"Return the inverse transformation of the receiver"
	^self subclassResponsibility
]

{ #category : #'transforming points' }
DisplayTransform >> invertBoundsRect: aRectangle [
	"Return a rectangle whose coordinates have been transformed
	from local back to global coordinates."

	^self subclassResponsibility
]

{ #category : #testing }
DisplayTransform >> isCompositeTransform [
	"Return true if the receiver is a composite transformation.
	Composite transformations may have impact on the accuracy."
	^false
]

{ #category : #testing }
DisplayTransform >> isIdentity [
	"Return true if the receiver is the identity transform; that is, if applying to a point returns the point itself."
	^self subclassResponsibility
]

{ #category : #testing }
DisplayTransform >> isMatrixTransform2x3 [
	"Return true if the receiver is 2x3 matrix transformation"
	^false
]

{ #category : #testing }
DisplayTransform >> isMorphicTransform [
	"Return true if the receiver is a MorphicTransform, that is specifies the transformation values explicitly."
	^false
]

{ #category : #testing }
DisplayTransform >> isPureTranslation [
	"Return true if the receiver specifies no rotation or scaling."
	^self subclassResponsibility
]

{ #category : #'transforming rects' }
DisplayTransform >> localBoundsToGlobal: aRectangle [
	"Transform aRectangle from local coordinates into global coordinates"
	^Rectangle encompassing: (self localPointsToGlobal: aRectangle corners)
]

{ #category : #'transforming points' }
DisplayTransform >> localPointToGlobal: aPoint [
	"Transform aPoint from local coordinates into global coordinates"
	^self subclassResponsibility
]

{ #category : #'transforming points' }
DisplayTransform >> localPointsToGlobal: inArray [
	"Transform all the points of inArray from local into global coordinates"
	^inArray collect:[:pt| self localPointToGlobal: pt]
]

{ #category : #initialize }
DisplayTransform >> setIdentity [
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	^self subclassResponsibility
]

{ #category : #'transforming rects' }
DisplayTransform >> sourceQuadFor: aRectangle [
	^ aRectangle innerCorners collect: 
		[:p | self globalPointToLocal: p]
]
