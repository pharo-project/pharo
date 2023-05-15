"
i am abstract path builder, to specify an interface protocol which is available for building paths,
by using  canvas or surface #createPath: method.
"
Class {
	#name : #AthensPathBuilder,
	#superclass : #Object,
	#classVars : [
		'ZeroPoint'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #'instance creation' }
AthensPathBuilder class >> createPath: aBlock [

	^ self new createPath: aBlock
]

{ #category : #'class initialization' }
AthensPathBuilder class >> initialize [
	ZeroPoint := 0@0
]

{ #category : #testing }
AthensPathBuilder class >> isAbstract [

	^ self == AthensPathBuilder
]

{ #category : #'path commands' }
AthensPathBuilder >> absolute [
	"Next commands will use absolute coordinates for all segment points"

	self subclassResponsibility
]

{ #category : #'path commands' }
AthensPathBuilder >> ccwArcTo: endPt angle: rot [
	"Add a counter-clockwise arc segment, starting from current path endpoint and
	ending at andPt. Angle should be specified in radians "

	self subclassResponsibility
]

{ #category : #'path commands' }
AthensPathBuilder >> close [
	"Close the current path controur"

	self subclassResponsibility
]

{ #category : #'creating path' }
AthensPathBuilder >> createPath:	aBlock [
	"aBlock value: self ...."

	self subclassResponsibility
]

{ #category : #'path commands' }
AthensPathBuilder >> curveVia: cp1 and: cp2 to: aPoint [
	"Add a cubic bezier curve,
	starting from current path endpoint, using control points cp1, cp2
	and ending at aPoint "

	self subclassResponsibility
]

{ #category : #'path commands' }
AthensPathBuilder >> curveVia: cp1 to: aPoint [
	"Add a quadric bezier curve,
	starting from current path endpoint, using control point cp1,
	and ending at aPoint "

	self subclassResponsibility
]

{ #category : #'path commands' }
AthensPathBuilder >> cwArcTo: endPt angle: rot [
	"Add a clockwise arc segment, starting from current path endpoint and
	ending at andPt. Angle should be specified in radians "


	self subclassResponsibility
]

{ #category : #'path commands' }
AthensPathBuilder >> lineTo: aPoint [
	"Add a line segment, starting from current path endpoint to aPoint"

	self subclassResponsibility
]

{ #category : #'path commands' }
AthensPathBuilder >> moveTo: aPoint [
	"Move command always starts a new contour "
	self subclassResponsibility
]

{ #category : #'path commands' }
AthensPathBuilder >> reflectedCurveVia: cp2 to: aPoint [
	"Add a reflected cubic bezier curve, starting from current path endpoint and ending at aPoint.
	The first control point is calculated as a reflection from the current point, if the last command was also a cubic bezier curve.
	Otherwise, the first control point is the current point. The second control point is cp2."

	self subclassResponsibility
]

{ #category : #'path commands' }
AthensPathBuilder >> relative [
	"Next commands will use relative coordinates for all segment points"

	self subclassResponsibility
]

{ #category : #'path commands' }
AthensPathBuilder >> string: aString font: aFont [
	"Adds closed paths for the string to the current path"

	self subclassResponsibility
]
