"
OrientedFill is an abstract superclass for fills which can be aligned appropriately.

Instance variables:
	origin	<Point>	The point at which to align the fill.
	direction <Point>	The direction in which the fill is defined
	normal	<Point>	Typically, just the direction rotated by 90 degrees.
"
Class {
	#name : #OrientedFillStyle,
	#superclass : #FillStyle,
	#instVars : [
		'origin',
		'direction',
		'normal'
	],
	#category : #'Balloon-Fills'
}

{ #category : #comparing }
OrientedFillStyle >> = anOrientedFillStyle [
	"Answer whether equal."

	^self species = anOrientedFillStyle species
		and: [self origin = anOrientedFillStyle origin
		and: [self direction = anOrientedFillStyle direction
		and: [self normal = anOrientedFillStyle normal]]]
]

{ #category : #accessing }
OrientedFillStyle >> direction [
	^direction ifNil:[direction := normal y @ normal x negated]
]

{ #category : #accessing }
OrientedFillStyle >> direction: aPoint [
	direction := aPoint
]

{ #category : #comparing }
OrientedFillStyle >> hash [
	"Hash is implemented because #= is implemented."
	
	^self species hash
		bitXor: (self origin hash
		bitXor: (self direction hash
		bitXor: (self normal hash)))
]

{ #category : #testing }
OrientedFillStyle >> isOrientedFill [
	"Return true if the receiver keeps an orientation (e.g., origin, direction, and normal)"
	^true
]

{ #category : #accessing }
OrientedFillStyle >> normal [
	^normal ifNil:[normal := direction y negated @ direction x]
]

{ #category : #accessing }
OrientedFillStyle >> normal: aPoint [
	normal := aPoint
]

{ #category : #accessing }
OrientedFillStyle >> origin [
	^origin
]

{ #category : #accessing }
OrientedFillStyle >> origin: aPoint [
	origin := aPoint.
]
