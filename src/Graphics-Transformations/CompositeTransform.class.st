"
A composite transform provides the effect of several levels of coordinate transformations.
"
Class {
	#name : #CompositeTransform,
	#superclass : #DisplayTransform,
	#instVars : [
		'globalTransform',
		'localTransform'
	],
	#category : #'Graphics-Transformations'
}

{ #category : #'instance creation' }
CompositeTransform class >> globalTransform: gt localTransform: lt [
	^self new globalTransform: gt localTransform: lt
]

{ #category : #accessing }
CompositeTransform >> angle [
	^ localTransform angle + globalTransform angle
]

{ #category : #converting }
CompositeTransform >> asCompositeTransform [
	^self
]

{ #category : #converting }
CompositeTransform >> asMatrixTransform2x3 [
	^globalTransform asMatrixTransform2x3
		composedWithLocal: localTransform asMatrixTransform2x3
]

{ #category : #converting }
CompositeTransform >> asMorphicTransform [
	"Squash a composite transform down to a simple one"
	^ MorphicTransform offset: self offset angle: self angle scale: self scale
]

{ #category : #initialization }
CompositeTransform >> composedWith: aTransform [
	"Return a new transform that has the effect of transforming points first by the receiver and then by the argument."

	self isIdentity ifTrue: [^ aTransform].
	aTransform isIdentity ifTrue: [^ self].
	^ CompositeTransform new globalTransform: self
							localTransform: aTransform
]

{ #category : #'transforming points' }
CompositeTransform >> globalPointToLocal: aPoint [
	"Transform aPoint from global coordinates into local coordinates"
	^localTransform globalPointToLocal:
		(globalTransform globalPointToLocal: aPoint)
]

{ #category : #initialization }
CompositeTransform >> globalTransform: gt localTransform: lt [
	globalTransform := gt.
	localTransform := lt
]

{ #category : #accessing }
CompositeTransform >> inverseTransformation [
	"Return the inverse transformation of the receiver"
	^self species new
		globalTransform: localTransform inverseTransformation
		localTransform: globalTransform inverseTransformation
]

{ #category : #transformations }
CompositeTransform >> invert: aPoint [
	^ globalTransform invert: (localTransform invert: aPoint)
]

{ #category : #testing }
CompositeTransform >> isCompositeTransform [
	^true
]

{ #category : #testing }
CompositeTransform >> isIdentity [
	^ globalTransform isIdentity and: [localTransform isIdentity]
]

{ #category : #testing }
CompositeTransform >> isPureTranslation [
	^ globalTransform isPureTranslation and: [localTransform isPureTranslation]
]

{ #category : #'transforming points' }
CompositeTransform >> localPointToGlobal: aPoint [
	"Transform aPoint from global coordinates into local coordinates"
	^globalTransform localPointToGlobal:
		(localTransform localPointToGlobal: aPoint)
]

{ #category : #accessing }
CompositeTransform >> offset [
	^ (self localPointToGlobal: 0@0) negated
]

{ #category : #accessing }
CompositeTransform >> scale [
	^ localTransform scale * globalTransform scale
]

{ #category : #transformations }
CompositeTransform >> transform: aPoint [
	^ localTransform transform: (globalTransform transform: aPoint)
]
