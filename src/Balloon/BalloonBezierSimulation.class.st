"
This class is a simulation of the code that's run by the Balloon engine. For debugging purposes only.
"
Class {
	#name : #BalloonBezierSimulation,
	#superclass : #Object,
	#instVars : [
		'start',
		'end',
		'via',
		'lastX',
		'lastY',
		'fwDx',
		'fwDy',
		'fwDDx',
		'fwDDy',
		'maxSteps'
	],
	#classVars : [
		'HeightSubdivisions',
		'LineConversions',
		'MonotonSubdivisions',
		'OverflowSubdivisions'
	],
	#category : #'Balloon-Simulation'
}

{ #category : #initialization }
BalloonBezierSimulation class >> initialize [
	HeightSubdivisions := 0.
	LineConversions := 0.
	MonotonSubdivisions := 0.
	OverflowSubdivisions := 0.
]

{ #category : #private }
BalloonBezierSimulation >> absoluteSquared8Dot24: value [
	"Compute the squared value of a 8.24 number with 0.0 <= value < 1.0,
	e.g., compute (value * value) bitShift: -24"
	| halfWord1 halfWord2 result |
	(value >= 0 and:[value < 16r1000000]) ifFalse:[^self error:'Value out of range'].
	halfWord1 := value bitAnd: 16rFFFF.
	halfWord2 := (value bitShift: -16) bitAnd: 255.

	result := (halfWord1 * halfWord1) bitShift: -16. "We don't need the lower 16bits at all"
	result := result + ((halfWord1 * halfWord2) * 2).
	result := result + ((halfWord2 * halfWord2) bitShift: 16).
	"word1 := halfWord1 * halfWord1.
	word2 := (halfWord2 * halfWord1) + (word1 bitShift: -16).
	word1 := word1 bitAnd: 16rFFFF.
	word2 := word2 + (halfWord1 * halfWord2).
	word2 := word2 + ((halfWord2 * halfWord2) bitShift: 16)."

	^result bitShift: -8
]

{ #category : #computing }
BalloonBezierSimulation >> computeInitialStateFrom: source with: transformation [
	"Compute the initial state in the receiver."
	start := (transformation localPointToGlobal: source start) asIntegerPoint.
	end := (transformation localPointToGlobal: source end) asIntegerPoint.
	via := (transformation localPointToGlobal: source via) asIntegerPoint.
]

{ #category : #computing }
BalloonBezierSimulation >> computeSplitAt: t [
	"Split the receiver at the parametric value t"
	| left right newVia1 newVia2 newPoint |
	left := self shallowCopy.
	right := self shallowCopy.
	"Compute new intermediate points"
	newVia1 := (via - start) * t + start.
	newVia2 := (end - via) * t + via.
	"Compute new point on curve"
	newPoint := ((newVia1 - newVia2) * t + newVia2) asIntegerPoint.
	left via: newVia1 asIntegerPoint.
	left end: newPoint.
	right start: newPoint.
	right via: newVia2 asIntegerPoint.
	^Array with: left with: right
]

{ #category : #private }
BalloonBezierSimulation >> debugDraw [
	| entry minY maxY lX lY canvas |
	entry := BalloonEdgeData new.
	canvas := Display getCanvas.
	minY := (start y min: end y) min: via y.
	maxY := (start y max: end y) max: via y.
	entry yValue: minY.
	self stepToFirstScanLineAt: minY in: entry.
	lX := entry xValue.
	lY := entry yValue.
	minY+1 to: maxY do:[:y|
		self stepToNextScanLineAt: y in: entry.
		canvas line: lX@lY to: entry xValue @ y width: 2 color: Color black.
		lX := entry xValue.
		lY := y.
	].

]

{ #category : #private }
BalloonBezierSimulation >> debugDraw2 [
	| canvas last max t next |
	canvas := Display getCanvas.
	max := 100.
	last := nil.
	0 to: max do:[:i|
		t := i asFloat / max asFloat.
		next := self valueAt: t.
		last ifNotNil:[
			canvas line: last to: next rounded width: 2 color: Color blue.
		].
		last := next rounded.
	].
]

{ #category : #private }
BalloonBezierSimulation >> debugDrawWide: n [
	| entry minY maxY canvas curve p1 p2 entry2 y |
	curve := self class new.
	curve start: start + (0@n).
	curve via: via + (0@n).
	curve end: end + (0@n).
	entry := BalloonEdgeData new.
	entry2 := BalloonEdgeData new.
	canvas := Display getCanvas.
	minY := (start y min: end y) min: via y.
	maxY := (start y max: end y) max: via y.
	entry yValue: minY.
	entry2 yValue: minY + n.
	self stepToFirstScanLineAt: minY in: entry.
	curve stepToFirstScanLineAt: minY+n in: entry2.
	y := minY.
	1 to: n do:[:i|
		y := y + 1.
		self stepToNextScanLineAt: y in: entry.
		p1 := entry xValue @ y.
		canvas line: p1 to: p1 + (n@0) width: 1 color: Color black.
	].
	[y < maxY] whileTrue:[
		y := y + 1.
		self stepToNextScanLineAt: y in: entry.
		p2 := (entry xValue + n) @ y.
		curve stepToNextScanLineAt: y in: entry2.
		p1 := entry2 xValue @ y.
		canvas line: p1 to: p2 width: 1 color: Color black.
	].

]

{ #category : #accessing }
BalloonBezierSimulation >> end [
	^end
]

{ #category : #accessing }
BalloonBezierSimulation >> end: aPoint [
	end := aPoint
]

{ #category : #computing }
BalloonBezierSimulation >> floatStepToFirstScanLineAt: yValue in: edgeTableEntry [
	"Float version of forward differencing"
	|  startX endX startY endY deltaY fwX1 fwX2 fwY1 fwY2 
	steps scaledStepSize squaredStepSize |
	(end y) >= (start y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
	].

	deltaY := endY - startY.

	"Quickly check if the line is visible at all"
	(yValue >= endY or:[deltaY = 0]) ifTrue:[
		^edgeTableEntry lines: 0].

	fwX1 := (startX + endX - (2 * via x)) asFloat.
	fwX2 := (via x - startX * 2) asFloat.
	fwY1 := (startY + endY - (2 * via y)) asFloat.
	fwY2 := ((via y - startY) * 2) asFloat.
	steps := deltaY asInteger * 2.
	scaledStepSize := 1.0 / steps asFloat.
	squaredStepSize := scaledStepSize * scaledStepSize.
	fwDx := fwX2 * scaledStepSize.
	fwDDx := 2.0 * fwX1 * squaredStepSize.
	fwDy := fwY2 * scaledStepSize.
	fwDDy := 2.0 * fwY1 * squaredStepSize.
	fwDx := fwDx + (fwDDx * 0.5).
	fwDy := fwDy + (fwDDy * 0.5).

	lastX := startX asFloat.
	lastY := startY asFloat.

	"self xDirection: xDir.
	self yDirection: yDir."
	edgeTableEntry xValue: startX.
	edgeTableEntry yValue: startY.
	edgeTableEntry zValue: 0.
	edgeTableEntry lines: deltaY.

	"If not at first scan line then step down to yValue"
	yValue = startY ifFalse:[
		self stepToNextScanLineAt: yValue in: edgeTableEntry.
		"And adjust remainingLines"
		edgeTableEntry lines: deltaY - (yValue - startY).
	].
]

{ #category : #computing }
BalloonBezierSimulation >> floatStepToNextScanLineAt: yValue in: edgeTableEntry [
	"Float version of forward differencing"
	[yValue asFloat > lastY] whileTrue:[
		self assert: [fwDx < -50.0 or:[fwDx > 50.0]].
		self assert: [fwDy < -50.0 or:[fwDy > 50.0]].
		self assert: [fwDDx < -50.0 or:[fwDDx > 50.0]].
		self assert: [fwDDy < -50.0 or:[fwDDy > 50.0]].
		lastY := lastY + fwDy.
		fwDx := fwDx + fwDDx.
		fwDy := fwDy + fwDDy.
	].
	edgeTableEntry xValue: lastX asInteger.
	edgeTableEntry zValue: 0.
]

{ #category : #accessing }
BalloonBezierSimulation >> inTangent [
	"Return the tangent at the start point"
	^via - start
]

{ #category : #accessing }
BalloonBezierSimulation >> initialX [
	^start y <= end y
		ifTrue:[start x]
		ifFalse:[end x]
]

{ #category : #accessing }
BalloonBezierSimulation >> initialY [
	^start y <= end y
		ifTrue:[start y]
		ifFalse:[end y]
]

{ #category : #accessing }
BalloonBezierSimulation >> initialZ [
	^0 "Assume no depth given"
]

{ #category : #computing }
BalloonBezierSimulation >> intStepToFirstScanLineAt: yValue in: edgeTableEntry [
	"Scaled integer version of forward differencing"
	|  startX endX startY endY deltaY fwX1 fwX2 fwY1 fwY2 
	 scaledStepSize squaredStepSize |
	(end y) >= (start y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
	].

	deltaY := endY - startY.

	"Quickly check if the line is visible at all"
	(yValue >= endY or:[deltaY = 0]) ifTrue:[
		^edgeTableEntry lines: 0].

	fwX1 := (startX + endX - (2 * via x)).
	fwX2 := (via x - startX * 2).
	fwY1 := (startY + endY - (2 * via y)).
	fwY2 := ((via y - startY) * 2).
	maxSteps := deltaY asInteger * 2.
	scaledStepSize := 16r1000000 // maxSteps.
	"@@: Okay, we need some fancy 64bit multiplication here"
	squaredStepSize := self absoluteSquared8Dot24: scaledStepSize.
	squaredStepSize = ((scaledStepSize * scaledStepSize) bitShift: -24)
		ifFalse:[self error:'Bad computation'].
	fwDx := fwX2 * scaledStepSize.
	fwDDx := 2 * fwX1 * squaredStepSize.
	fwDy := fwY2 * scaledStepSize.
	fwDDy := 2 * fwY1 * squaredStepSize.
	fwDx := fwDx + (fwDDx // 2).
	fwDy := fwDy + (fwDDy // 2).

	self validateIntegerRange.

	lastX := startX * 256.
	lastY := startY * 256.

	edgeTableEntry xValue: startX.
	edgeTableEntry yValue: startY.
	edgeTableEntry zValue: 0.
	edgeTableEntry lines: deltaY.

	"If not at first scan line then step down to yValue"
	yValue = startY ifFalse:[
		self stepToNextScanLineAt: yValue in: edgeTableEntry.
		"And adjust remainingLines"
		edgeTableEntry lines: deltaY - (yValue - startY).
	].
]

{ #category : #computing }
BalloonBezierSimulation >> intStepToNextScanLineAt: yValue in: edgeTableEntry [
	"Scaled integer version of forward differencing"
	[maxSteps >= 0 and:[yValue * 256 > lastY]] whileTrue:[
		self validateIntegerRange.
		lastX := lastX + ((fwDx + 16r8000) // 16r10000).
		lastY := lastY + ((fwDy + 16r8000) // 16r10000).
		fwDx := fwDx + fwDDx.
		fwDy := fwDy + fwDDy.
		maxSteps := maxSteps - 1.
	].
	edgeTableEntry xValue: lastX // 256.
	edgeTableEntry zValue: 0.
]

{ #category : #computing }
BalloonBezierSimulation >> isMonoton [
	"Return true if the receiver is monoton along the y-axis,
	e.g., check if the tangents have the same sign"
	^(via y - start y) * (end y - via y) >= 0
]

{ #category : #accessing }
BalloonBezierSimulation >> outTangent [
	"Return the tangent at the end point"
	^end - via
]

{ #category : #printing }
BalloonBezierSimulation >> printOn: aStream [
	aStream 
		nextPutAll: self class name;
		nextPut:$(;
		print: start;
		nextPutAll:' - ';
		print: via;
		nextPutAll:' - ';
		print: end;
		nextPut:$)
]

{ #category : #printing }
BalloonBezierSimulation >> quickPrint: curve [
	self quickPrint: curve first: false
]

{ #category : #printing }
BalloonBezierSimulation >> quickPrint: curve first: aBool [
	aBool
		ifTrue: [ self trace: String cr ].
	self
		trace: (String streamContents: [ :e | self quickPrint: curve on: e ])
]

{ #category : #printing }
BalloonBezierSimulation >> quickPrint: curve on: aStream [
	aStream
		nextPut: $(;
		nextPutAll: curve start asString;
		space;
		nextPutAll: curve via asString;
		space;
		nextPutAll: curve end asString;
		nextPut: $)
]

{ #category : #accessing }
BalloonBezierSimulation >> start [
	^start
]

{ #category : #accessing }
BalloonBezierSimulation >> start: aPoint [
	start := aPoint
]

{ #category : #private }
BalloonBezierSimulation >> stepToFirst [
	|  startX endX startY endY deltaY fwX1 fwX2 fwY1 fwY2 
	steps scaledStepSize squaredStepSize |
	(end y) >= (start y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
	].

	deltaY := endY - startY.

	"Quickly check if the line is visible at all"
	(deltaY = 0) ifTrue:[^self].

	fwX1 := (startX + endX - (2 * via x)) asFloat.
	fwX2 := (via x - startX * 2) asFloat.
	fwY1 := (startY + endY - (2 * via y)) asFloat.
	fwY2 := ((via y - startY) * 2) asFloat.
	steps := deltaY asInteger * 2.
	scaledStepSize := 1.0 / steps asFloat.
	squaredStepSize := scaledStepSize * scaledStepSize.
	fwDx := fwX2 * scaledStepSize.
	fwDDx := 2.0 * fwX1 * squaredStepSize.
	fwDy := fwY2 * scaledStepSize.
	fwDDy := 2.0 * fwY1 * squaredStepSize.
	fwDx := fwDx + (fwDDx * 0.5).
	fwDy := fwDy + (fwDDy * 0.5).

	lastX := startX asFloat.
	lastY := startY asFloat.

]

{ #category : #private }
BalloonBezierSimulation >> stepToFirstInt [
	"Scaled integer version of forward differencing"
	|  startX endX startY endY deltaY fwX1 fwX2 fwY1 fwY2 
	 scaledStepSize squaredStepSize |
	(end y) >= (start y) ifTrue:[
		startX := start x.	endX := end x.
		startY := start y.	endY := end y.
	] ifFalse:[
		startX := end x.	endX := start x.
		startY := end y.	endY := start y.
	].

	deltaY := endY - startY.

	"Quickly check if the line is visible at all"
	(deltaY = 0) ifTrue:[^nil].

	fwX1 := (startX + endX - (2 * via x)).
	fwX2 := (via x - startX * 2).
	fwY1 := (startY + endY - (2 * via y)).
	fwY2 := ((via y - startY) * 2).
	maxSteps := deltaY asInteger * 2.
	scaledStepSize := 16r1000000 // maxSteps.
	"@@: Okay, we need some fancy 64bit multiplication here"
	squaredStepSize := (scaledStepSize * scaledStepSize) bitShift: -24.
	fwDx := fwX2 * scaledStepSize.
	fwDDx := 2 * fwX1 * squaredStepSize.
	fwDy := fwY2 * scaledStepSize.
	fwDDy := 2 * fwY1 * squaredStepSize.
	fwDx := fwDx + (fwDDx // 2).
	fwDy := fwDy + (fwDDy // 2).

	self validateIntegerRange.

	lastX := startX * 256.
	lastY := startY * 256.

]

{ #category : #computing }
BalloonBezierSimulation >> stepToFirstScanLineAt: yValue in: edgeTableEntry [
	"Compute the initial x value for the scan line at yValue"
	^self intStepToFirstScanLineAt: yValue in: edgeTableEntry
]

{ #category : #private }
BalloonBezierSimulation >> stepToNext [
		lastX := lastX + fwDx.
		lastY := lastY + fwDy.
		fwDx := fwDx + fwDDx.
		fwDy := fwDy + fwDDy.
]

{ #category : #computing }
BalloonBezierSimulation >> stepToNextScanLineAt: yValue in: edgeTableEntry [
	"Compute the next x value for the scan line at yValue.
	This message is sent during incremental updates. 
	The yValue parameter is passed in here for edges
	that have more complicated computations,"
	^self intStepToNextScanLineAt: yValue in: edgeTableEntry
]

{ #category : #computing }
BalloonBezierSimulation >> subdivide [
	"Subdivide the receiver"
	| dy dx |
	"Test 1: If the bezier curve is not monoton in Y, we need a subdivision"
	self isMonoton ifFalse:[
		MonotonSubdivisions := MonotonSubdivisions + 1.
		^self subdivideToBeMonoton].

	"Test 2: If the receiver is horizontal, don't do anything"
	(end y = start y) ifTrue:[^nil].

	"Test 3: If the receiver can be represented as a straight line,
			make a line from the receiver and declare it invalid"
	((end - start) crossProduct: (via - start)) = 0 ifTrue:[
		LineConversions := LineConversions + 1.
		^self subdivideToBeLine].

	"Test 4: If the height of the curve exceeds 256 pixels, subdivide 
			(forward differencing is numerically not very stable)"
	dy := end y - start y.
	dy < 0 ifTrue:[dy := dy negated].
	(dy > 255) ifTrue:[
		HeightSubdivisions := HeightSubdivisions + 1.
		^self subdivideAt: 0.5].

	"Test 5: Check if the incremental values could possibly overflow the scaled integer range"
	dx := end x - start x.
	dx < 0 ifTrue:[dx := dx negated].
	dy * 32 < dx ifTrue:[
		OverflowSubdivisions := OverflowSubdivisions + 1.
		^self subdivideAt: 0.5].

	^nil
]

{ #category : #computing }
BalloonBezierSimulation >> subdivideAt: parameter [
	"Subdivide the receiver at the given parameter"
	| both |
	self assert: [parameter <= 0.0 or:[parameter >= 1.0]].
	both := self computeSplitAt: parameter.
	"Transcript cr.
	self quickPrint: self.
	Transcript space.
	self quickPrint: both first.
	Transcript space.
	self quickPrint: both last.
	Transcript endEntry."
	self via: both first via.
	self end: both first end.
	^both last
]

{ #category : #computing }
BalloonBezierSimulation >> subdivideToBeLine [
	"Not a true subdivision.
	Just return a line representing the receiver and fake me to be of zero height"
	| line |
	line := BalloonLineSimulation new.
	line start: start.
	line end: end.
	"Make me invalid"
	end := start.
	via := start.
	 ^line
]

{ #category : #computing }
BalloonBezierSimulation >> subdivideToBeMonoton [
	"Subdivide the receiver at it's extreme point"
	| v1 v2 t other |
	v1 := (via - start).
	v2 := (end - via).
	t := (v1 y / (v2 y - v1 y)) negated asFloat.
	other := self subdivideAt: t.
	self assert: [self isMonoton].
	self assert: [other isMonoton].
	^other
]

{ #category : #private }
BalloonBezierSimulation >> validateIntegerRange [
	self assert: [fwDx class == SmallInteger].
	self assert: [fwDy class == SmallInteger].
	self assert: [fwDDx class == SmallInteger].
	self assert: [fwDDy class == SmallInteger].

]

{ #category : #private }
BalloonBezierSimulation >> valueAt: parameter [
	"Return the point at the value parameter:
		p(t) =	(1-t)^2 * p1 + 
				2*t*(1-t) * p2 + 
				t^2 * p3.
	"
	| t1 t2 t3 |
	t1 := (1.0 - parameter) squared.
	t2 := 2 * parameter * (1.0 - parameter).
	t3 := parameter squared.
	^(start * t1) + (via * t2) + (end * t3)
]

{ #category : #accessing }
BalloonBezierSimulation >> via [
	^via
]

{ #category : #accessing }
BalloonBezierSimulation >> via: aPoint [
	via := aPoint
]
