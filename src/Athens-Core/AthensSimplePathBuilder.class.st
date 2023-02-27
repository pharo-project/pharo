"
i building path containing from AthensPathSegment. 
the resulting path is backend neutral (while different backend may implement own builder which is more efficient)
"
Class {
	#name : #AthensSimplePathBuilder,
	#superclass : #AthensPathBuilder,
	#instVars : [
		'absolute',
		'open',
		'contourStartPt',
		'xMin',
		'yMin',
		'xMax',
		'yMax',
		'pathStart',
		'lastSegment'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #'path commands' }
AthensSimplePathBuilder >> absolute [
	absolute := true
]

{ #category : #private }
AthensSimplePathBuilder >> addSegment: aSegment [
	lastSegment next: aSegment.
	lastSegment := aSegment
]

{ #category : #'path commands' }
AthensSimplePathBuilder >> ccwArcTo: endPt angle: rot [
	" angle should be specified in radians "
	^ self addSegment: (AthensCCWArcSegment new endPoint: (self toAbsolute: endPt) angle: rot)
]

{ #category : #'path commands' }
AthensSimplePathBuilder >> close [
	"Close the current contour"
	self addSegment: (AthensCloseSegment new point: contourStartPt).

	contourStartPt := nil.
	open := false
]

{ #category : #'creating path' }
AthensSimplePathBuilder >> createPath:	aBlock [
	"aBlock value: self ...."
	aBlock value: self.

	^ pathStart
]

{ #category : #'path commands' }
AthensSimplePathBuilder >> curveVia: cp1 and: cp2 to: aPoint [
	"Cubic bezier curve"

	| pt1 pt2 pt3 |
	"Quad bezier curve"

	pt1 := self toAbsolute: cp1.
	pt2 := self toAbsolute: cp2.
	pt3 := self toAbsolute: aPoint.
	^ self addSegment: (
		AthensCubicSegment new
			from: lastSegment endPoint
			via: pt1
			and: pt2
			to: pt3)
]

{ #category : #'path commands' }
AthensSimplePathBuilder >> curveVia: cp1 to: aPoint [
	| pt1 pt2 |
	"Quad bezier curve"

	pt1 := self toAbsolute: cp1.
	pt2 := self toAbsolute: aPoint.
	^ self addSegment: (
		AthensQuadSegment new
			from: lastSegment endPoint
			via: pt1
			to: pt2)
]

{ #category : #'path commands' }
AthensSimplePathBuilder >> cwArcTo: endPt angle: rot [
	"Angle should be specified in radians"
	^ self addSegment: (AthensCWArcSegment new endPoint: (self toAbsolute: endPt) angle: rot)
]

{ #category : #initialization }
AthensSimplePathBuilder >> initialize [
	"A new path always starts from implicit (moveTo:0@0) segment.
	If next segment is moveTo: , the point of already existing move segment will be changed,
	avoiding creating extra move segments.

	"
	super initialize.
	absolute := false.
	contourStartPt := ZeroPoint.
	pathStart := lastSegment := (AthensMoveSegment new  point: ZeroPoint)
]

{ #category : #'path commands' }
AthensSimplePathBuilder >> lineTo: aPoint [

	^ self addSegment: (AthensLineSegment new point: (self toAbsolute: aPoint))
]

{ #category : #'path commands' }
AthensSimplePathBuilder >> moveTo: aPoint [
	"Move command always starts a new contour "
	contourStartPt := self toAbsolute: aPoint.

	"Collapse multiple moves to a single one"
	lastSegment isMove ifTrue: [
		lastSegment point: contourStartPt.
		^ self
	].

	self addSegment: (AthensMoveSegment new point: contourStartPt; reopen: open).
	open := true
]

{ #category : #accessing }
AthensSimplePathBuilder >> pathBounds [
	^ xMin@yMin corner: xMax@yMax
]

{ #category : #accessing }
AthensSimplePathBuilder >> pathStart [
	^ pathStart
]

{ #category : #'path commands' }
AthensSimplePathBuilder >> reflectedCurveVia: cp2 to: aPoint [
	"Reflected cubic bezier curve"

	| pt1 pt2 pt3 |

	pt2 := self toAbsolute: cp2.
	pt1 := lastSegment isCubic
		ifTrue: [ lastSegment via2reflected ]
		ifFalse: [ lastSegment endPoint ].
	pt3 := self toAbsolute: aPoint.
	^ self addSegment: (
		AthensCubicSegment new
			from: lastSegment endPoint
			via: pt1
			and: pt2
			to: pt3)
]

{ #category : #'path commands' }
AthensSimplePathBuilder >> relative [
	absolute := false
]

{ #category : #private }
AthensSimplePathBuilder >> toAbsolute: aPoint [
	| pt |
	pt := absolute
		ifTrue: [ aPoint ]
		ifFalse: [ lastSegment endPoint + aPoint ].

	"note the coordinate to calculate the path's bounding box"
	xMin ifNil: [
		xMin := xMax := pt x.
		yMin := yMax := pt y.
		]
	ifNotNil: [
		xMin := pt x min: xMin.
		yMin := pt y min: yMin.
		xMax := pt x max: xMax.
		yMax := pt y max: yMax.
	].
	^ pt
]
