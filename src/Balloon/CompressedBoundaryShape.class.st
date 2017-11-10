"
This class represents a very compact representation of a boundary shape. It consists of a number of compressed arrays that can be handled by the balloon engine directly. Due to this, there are certain restrictions (see below). Boundaries are always represented by three subsequent points that define a quadratic bezier segment. It is recommended that for straight line segments the control point is set either to the previous or the next point.

Instance variables:
	points		<PointArray | ShortPointArray>	Point storage area
	leftFills		<ShortRunArray>	Containing the ""left"" fill index of each segment
	rightFills	<ShortRunArray>	Containing the ""right"" fill index of each segment
	lineWidths	<ShortRunArray>	Containing the line width of each segment
	lineFills		<ShortRunArray>	Containing the line fill (e.g., line color) of each segment
	fillStyles	<Collections>			Contains the actual fill styles referenced by the indexes

RESTRICTIONS:
None of the ShortRunArrays may contain a run of length Zero.
Also, due to the use of ShortRunArrays 
	a) you cannot have more than 32768 different fill styles
	b) you cannot have a line width that exceeds 32768
In case you have trouble with a), try to merge some of the fills into one. You might do so by converting colors to 32bit pixel values. In case you have trouble with b) you might change the general resolution of the compressed shape to have less accuracy.

"
Class {
	#name : #CompressedBoundaryShape,
	#superclass : #Object,
	#instVars : [
		'points',
		'leftFills',
		'rightFills',
		'lineWidths',
		'lineFills',
		'fillStyles'
	],
	#category : #Balloon-Geometry
}

{ #category : #'instance creation' }
CompressedBoundaryShape class >> points: pointList leftFills: leftFillList rightFills: rightFillList fillStyles: fillStyleList lineWidths: lineWidthList lineFills: lineFillList [
	^self new setPoints: pointList leftFills: leftFillList rightFills: rightFillList fillStyles: fillStyleList lineWidths: lineWidthList lineFills: lineFillList
]

{ #category : #accessing }
CompressedBoundaryShape >> bounds [
	| min max width |
	points isEmpty ifTrue:[^0@0 corner: 1@1].
	min := max := points first.
	points do:[:pt|
		min := min min: pt.
		max := max max: pt
	].
	width := 0.
	lineWidths valuesDo:[:w| width := width max: w].
	^(min corner: max) insetBy: (width negated asPoint)
]

{ #category : #editing }
CompressedBoundaryShape >> collectFills: aBlock [
	fillStyles := fillStyles collect: aBlock.
]

{ #category : #editing }
CompressedBoundaryShape >> copyAndCollectFills: aBlock [
	^self copy collectFills: aBlock
]

{ #category : #accessing }
CompressedBoundaryShape >> fillStyles [
	^fillStyles
]

{ #category : #accessing }
CompressedBoundaryShape >> leftFills [
	^leftFills
]

{ #category : #accessing }
CompressedBoundaryShape >> lineFills [
	^lineFills
]

{ #category : #accessing }
CompressedBoundaryShape >> lineWidths [
	^lineWidths
]

{ #category : #morphing }
CompressedBoundaryShape >> morphFrom: srcShape to: dstShape at: ratio [
	| scale unscale srcPoints dstPoints pt1 pt2 x y |
	scale := (ratio * 1024) asInteger.
	scale < 0 ifTrue:[scale := 0].
	scale > 1024 ifTrue:[scale := 1024].
	unscale := 1024 - scale.
	srcPoints := srcShape points.
	dstPoints := dstShape points.
	1 to: points size do:[:i|
		pt1 := srcPoints at: i.
		pt2 := dstPoints at: i.
		x := ((pt1 x * unscale) + (pt2 x * scale)) bitShift: -10.
		y := ((pt1 y * unscale) + (pt2 y * scale)) bitShift: -10.
		points at: i put: x@y].
]

{ #category : #accessing }
CompressedBoundaryShape >> numSegments [
	^points size // 3
]

{ #category : #accessing }
CompressedBoundaryShape >> points [
	^points
]

{ #category : #accessing }
CompressedBoundaryShape >> rightFills [
	^rightFills
]

{ #category : #accessing }
CompressedBoundaryShape >> segments [
	"Return all the segments in the receiver"
	| out |
	out := Array new writeStream.
	self segmentsDo:[:seg| out nextPut: seg].
	^out contents
]

{ #category : #enumerating }
CompressedBoundaryShape >> segmentsDo: aBlock [
	"Enumerate all segments in the receiver and execute aBlock"
	| p1 p2 p3 |
	1 to: points size by: 3 do:[:i|
		p1 := points at: i.
		p2 := points at: i+1.
		p3 := points at: i+2.
		(p1 = p2 or:[p2 = p3]) ifTrue:[
			aBlock value: (LineSegment from: p1 to: p3).
		] ifFalse:[
			aBlock value: (Bezier2Segment from: p1 via: p2 to: p3).
		].
	].
]

{ #category : #private }
CompressedBoundaryShape >> setPoints: pointList leftFills: leftFillList rightFills: rightFillList fillStyles: fillStyleList lineWidths: lineWidthList lineFills: lineFillList [
	points := pointList.
	leftFills := leftFillList.
	rightFills := rightFillList.
	lineWidths := lineWidthList.
	lineFills := lineFillList.
	fillStyles := fillStyleList.
]
