Extension { #name : #PolygonMorph }

{ #category : #'*Athens-Morphic' }
PolygonMorph >> adjustSegmentPointsForArrows [
	" In the list of vertices, adjust first and last entry if this polygon uses arrows."

	| verts |
	(self isClosed or: [ self hasArrows not ])
		ifTrue: [ ^ self vertices ].
	verts := self vertices copy.
	(arrows == #back or: [ arrows == #both ])
		ifTrue: [
			| arrow |
			arrow := self arrowBoundsAt: verts first from: verts second.
			arrow size = 4
				ifTrue: [ verts at: 1 put: arrow third ]
				ifFalse: [ verts at: 1 put: (arrow copyFrom: 2 to: 3) average ] ].
	(arrows == #forward or: [ arrows == #both ])
		ifTrue: [
			| arrow |
			arrow := self arrowBoundsAt: verts last from: verts nextToLast.
			arrow size = 4
				ifTrue: [ verts at: verts size put: arrow third ]
				ifFalse: [ verts at: verts size put: (arrow copyFrom: 2 to: 3) average ] ].
	^ verts
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> asAthensCurvedOpenPathOn: anAthensCanvas [
"like asAthensCurvedPathOn: but use quadratic curve for start and end points"
	| points |
	points := self computeCurvedSegmentCtrlPoints.
	^ anAthensCanvas
		createPath: [ :builder |
			self vertices notEmpty
				ifTrue: [
					| prior |
					"points contains {ctrl1 vertice ctrl2}"
					prior := points first.
					builder absolute.
					builder moveTo: prior second.
					builder curveVia: (points at: 2) first to: (points at: 2) second.
					prior := points at: 2.
					3 to: points size - 1 do: [ :index |
							| p |
							p := points at: index.
							builder curveVia: prior third and: p first to: p second.
							prior := p ].
					builder curveVia: prior third to: points last second].
			builder ]
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> asAthensCurvedPathOn: anAthensCanvas [
	| points |
	points := self computeCurvedSegmentCtrlPoints.
	^ anAthensCanvas
		createPath: [ :builder |
			self vertices notEmpty
				ifTrue: [
					| prior | "points contains {ctrl1 vertice ctrl2}"
					prior := points first.
					builder absolute.
					builder moveTo: prior second.
					points allButFirst
						do: [ :p |
							builder curveVia: prior third and: p first to: p second.
							prior := p ].
					self isClosed
						ifTrue: [
							builder curveVia: prior third and: points first first to: points first second.
							builder close ] ].
			builder ]
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> asAthensLinePathOn: anAthensCanvas [
	| verts |
	verts := self adjustSegmentPointsForArrows.
	^ anAthensCanvas
		createPath: [ :builder |
			verts notEmpty
				ifTrue: [
					builder absolute.
					builder moveTo: verts first.
					verts allButFirstDo: [ :p | builder lineTo: p ] ].
			self isClosed
				ifTrue: [ builder close ].
			builder ]
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> asAthensPathOn: anAthensCanvas [
	^ self isCurvy
		ifTrue: [
			self isOpen
				ifTrue: [ self asAthensCurvedOpenPathOn: anAthensCanvas ]
				ifFalse: [ self asAthensCurvedPathOn: anAthensCanvas ] ]
		ifFalse: [ self asAthensLinePathOn: anAthensCanvas ]
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> computeCurvedSegmentCtrlPoints [
	"compute control points for the curved polygon.
For every vertice C take the two edges L and R. Compute the line T between the midpoints of L and R.
Use T is the tangent on the C. T is divided by the C with the same ration as L/R.
The start and end point of T defines the controlpoint c1 and c2
for the current vertice C.
"

	| ctrls vert |
	vert := self adjustSegmentPointsForArrows.
	ctrls := (1 to: vert size)
		collect: [ :i |
			| prior current next lenpc lencn ctrl1 ctrl2 tangent |
			prior := vert atWrap: i - 1.
			current := vert atWrap: i.
			next := vert atWrap: i + 1.
			tangent := (next - prior) / 2.
			lenpc := current distanceTo: prior.
			lencn := next distanceTo: current.
			lenpc = 0 ifTrue:[
				ctrl1 := prior]
			ifFalse:[	ctrl1 := current - (tangent / (1 + (lencn / lenpc)))].
				lencn = 0 ifTrue:[
					ctrl2 := next]
				ifFalse:[
			ctrl2 := current + (tangent / (1 + (lenpc / lencn)))].
		"collect ctrl1 current and ctrl2"
			{(ctrl1 asFloatPoint).
			current.
			(ctrl2 asFloatPoint)} ].
	^ ctrls
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> drawArrowOnAthensCanvas: anAthensCanvas at: endPoint from: priorPoint [
	"Draw a triangle oriented along the line from priorPoint to
	endPoint. Answer the wingBase."

	| pts spec wingBase path |
	pts := self arrowBoundsAt: endPoint from: priorPoint.
	wingBase := pts size = 4
		ifTrue: [ pts third ]
		ifFalse: [ (pts copyFrom: 2 to: 3) average ].
	spec := self valueOfProperty: #arrowSpec ifAbsent: [ PolygonMorph defaultArrowSpec ].
	path := anAthensCanvas
		createPath: [ :builder |
			builder absolute.
			builder moveTo: pts first.
			pts allButFirst do: [ :p | builder lineTo: p ]. builder close. ].
	spec x sign = spec y sign
		ifTrue: [
			anAthensCanvas setPaint: self borderColor]
		ifFalse: [
			(anAthensCanvas
				setStrokePaint: self borderColor)
				width: (borderWidth + 1) // 2].
	anAthensCanvas drawShape: path.
	^ wingBase
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> drawArrowsOnAthensCanvas: aAthensCanvas [
	"Answer (possibly modified) endpoints for border drawing"
	"ArrowForms are computed only upon demand"
	| array |

	self hasArrows
		ifFalse: [^ #() ].
	"Nothing to do"

	array := Array with: vertices first with: vertices last.

	"Prevent crashes for #raised or #inset borders"
	borderColor isColor
		ifFalse: [ ^array ].

	(arrows == #forward or: [arrows == #both])
		ifTrue: [ array at: 2 put: (self
				drawArrowOnAthensCanvas: aAthensCanvas
				at: vertices last
				from: self nextToLastPoint) ].

	(arrows == #back or: [arrows == #both])
		ifTrue: [ array at: 1 put: (self
				drawArrowOnAthensCanvas: aAthensCanvas
				at: vertices first
				from: self nextToFirstPoint) ].

	^array
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> drawOnAthensCanvas: anAthensCanvas [
	| border shape stroke |
	border := self borderStyle.
	shape := self asAthensPathOn: anAthensCanvas.
	self isClosed
		ifTrue: [
			anAthensCanvas setPaint: self fillStyle.
			anAthensCanvas drawShape: shape ].
	stroke := anAthensCanvas setStrokePaint: border color.
	stroke width: border width.
	self dashedBorder notNil
		ifTrue: [ self setDashOnStroke: stroke ].
	anAthensCanvas drawShape: shape.
	self hasTwoColorDash
		ifTrue: [
			self setSecondColorDashOnStroke: stroke.
			anAthensCanvas drawShape: shape ].
	self drawArrowsOnAthensCanvas: anAthensCanvas
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> hasTwoColorDash [
	" return true if the dash spec has another color"

	^ self dashedBorder notNil and: [ self dashedBorder size > 2 ]
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> setDashOnStroke: anAthensStrokePaint [
	| d commonOffset |
	d := self dashedBorder.
	commonOffset := d size > 3
		ifTrue: [ d fourth ]
		ifFalse: [ 0 ].
	anAthensStrokePaint
		dashes:
			{(d first).
			(d second)}
		offset: commonOffset
]

{ #category : #'*Athens-Morphic' }
PolygonMorph >> setSecondColorDashOnStroke: anAthensStrokePaint [
	| d c commonOffset |
	d := self dashedBorder.
	c := d third.
	commonOffset := d size > 3
		ifTrue: [ d fourth ]
		ifFalse: [ 0 ].
	anAthensStrokePaint fillPaint: c.
	anAthensStrokePaint
		dashes:
			{(d second).
			(d first)}
		offset: commonOffset + d second
]
