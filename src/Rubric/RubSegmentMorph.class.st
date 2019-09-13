"
3. The CLOSED and SEGMENTED variant looks like polygon. This is actually what you get when you do
	PolygonMorph new openInWorld
You get a triangle. See below how to manipulate these objects...

4. The CLOSED and SMOOTHED variant looks like blob (???)


"
Class {
	#name : #RubSegmentMorph,
	#superclass : #Morph,
	#instVars : [
		'vertices',
		'filledForm',
		'smoothCurve',
		'curveState',
		'borderForm',
		'borderColor',
		'borderWidth'
	],
	#category : #'Rubric-Editing-Core'
}

{ #category : #'instance creation' }
RubSegmentMorph class >> vertices: verts color: c borderWidth: bw borderColor: bc [
	^ self basicNew beStraightSegments vertices: verts color: c borderWidth: bw borderColor: bc
]

{ #category : #drawing }
RubSegmentMorph >> areasRemainingToFill: aRectangle [
	"Could be improved by quick check of inner rectangle"

	^ Array with: aRectangle
]

{ #category : #initialization }
RubSegmentMorph >> beSmoothCurve [
	smoothCurve == true
		ifFalse: [ 
			smoothCurve := true.
			self computeBounds ]
]

{ #category : #initialization }
RubSegmentMorph >> beStraightSegments [

	smoothCurve == false ifFalse:
		[smoothCurve := false.
		self computeBounds]
]

{ #category : #accessing }
RubSegmentMorph >> borderColor: aColor [ 

	super borderColor: aColor.
	self releaseCachedState
]

{ #category : #private }
RubSegmentMorph >> borderForm [
	"A form must be created for drawing the border whenever the borderColor is translucent."

	| borderCanvas |
	borderForm ifNotNil: [^ borderForm].
	borderCanvas := (Display defaultCanvasClass extent: bounds extent depth: 1)
		asShadowDrawingCanvas: Color black.
	borderCanvas translateBy: bounds topLeft negated
		during:[:tempCanvas| self drawBorderOn: tempCanvas].
	borderForm := borderCanvas form.
	^ borderForm
]

{ #category : #accessing }
RubSegmentMorph >> borderWidth: anInteger [

	super borderWidth: anInteger.
	self computeBounds
]

{ #category : #geometry }
RubSegmentMorph >> bounds: newBounds [
	"This method has to be reimplemented since self extent: will also change self bounds origin,
	super bounds would leave me in wrong position when container is growing.
	Always change extent first then position"
	
	self extent: newBounds extent; position: newBounds topLeft

]

{ #category : #'cubic support' }
RubSegmentMorph >> changeInSlopes: slopes of: verts [ 
	"A message to knots of a spline. Returns an array with the 3rd cubic coeff."
	"The last nth item is correct iff this is a closed cubic.
	Presumably that is the only time we care.
	We always return the same sized array as self."
	| n slopeChanges |
	n := verts size.
	n = slopes size ifFalse: [^ self error: 'vertices and slopes differ in number'].
	slopeChanges := Array new: n.
	1 to: n do: [:i | slopeChanges at: i put: (verts atWrap: i + 1)
					- (verts at: i) * 3 - ((slopes at: i) * 2)
					- (slopes atWrap: i + 1)].
	^ slopeChanges
]

{ #category : #'cubic support' }
RubSegmentMorph >> changeOfChangesInSlopes: slopes of: verts [
	"A message to knots of a spline. Returns an array with the 4rd 
	cubic coeff."
	"The last nth item is correct iff this is a closed cubic. 
	Presumably that is the only time we care. 
	We always return the same sized array as self."
	| n changes |
	n := verts size.
	n = slopes size ifFalse: [^ self error: 'vertices and slopes differ in number'].
	changes := Array new: n.
	1 to: n do: [:i | changes at: i put: (verts at: i)
					- (verts atWrap: i + 1) * 2
					+ (slopes at: i)
					+ (slopes atWrap: i + 1)].
	^ changes
]

{ #category : #'cubic support' }
RubSegmentMorph >> closedCubicSlopesOf: knots [ 
	"Sent to knots returns the slopes of a closed cubic spline.
	From the same set of java sources as naturalCubic. This is a smalltalk  
	transliteration of the java code."
	"from java code NatCubicClosed extends NatCubic  
	solves for the set of equations for all knots: 
	b1+4*b2+b3=3*(a3-a1)
	where a1 is (knots atWrap: index + 1) etc.
	and the b's are the slopes .
	by decomposing the matrix into upper triangular and lower matrices  
	and then back sustitution. See Spath 'Spline Algorithms for Curves  
	and Surfaces' pp 19--21. The D[i] are the derivatives at the knots.  
	"
	
	| v w x y z n1  D F G H |
	n1 := knots size.
	n1 < 3
		ifTrue: [self error: 'Less than 3 points makes a poor curve'].
	v := Array new: n1.
	w := Array new: n1.
	y := Array new: n1.
	
	D := Array new: n1.
	x := knots.
	z := 1.0 / 4.0.
	v at: 2 put: z.
	w at: 2 put: z.
	y at: 1 put: z * 3.0 * ((x at: 2) - (x at: n1)).
	H := 4.0.
	F := 3 * ((x at: 1) - (x at: n1 - 1)).
	G := 1.
	2 to: n1 - 1 do: [:k | 
			z := 1.0 / (4.0 - (v at: k)).
			v at: k + 1 put: z.
			w at: k + 1 put: z negated
					* (w at: k).
			y at: k put: z * (3.0 * ((x at: k + 1) - (x at: k - 1)) - (y at: k - 1)).
			H := H - (G * (w at: k)).
			F := F - (G * (y at: k - 1)).
			G := (v at: k) negated * G].
	H := H - (G + 1 * ((v at: n1) + (w at: n1))).
	y at: n1 put: F - (G + 1 * (y at: n1 - 1)).
	D at: n1 put: (y at: n1) / H.
	D at: n1 - 1 put: (y at: n1 - 1) - ((v at: n1) + (w at: n1) * (D at: n1)).
	(1 to: n1 - 2)
		reverseDo: [:k | D at: k put: (y at: k)
					- ((v at: k + 1) * (D at: k + 1)) - ((w at: k + 1) * (D at: n1))].
	^ D 


]

{ #category : #smoothing }
RubSegmentMorph >> coefficients [
	curveState ifNotNil: [^ curveState at: 1].
	^ self vertices size < 1 
		ifTrue: [ self  ]
		ifFalse: [ self coefficientsForMoreThanThreePoints ].  
]

{ #category : #smoothing }
RubSegmentMorph >> coefficientsForMoreThanThreePoints [ 
	"Less than three points handled as segments by our lineSegmentsDo:"
	| verts coefficients vertXs slopeXs vertYs slopeYs bestSegments |
	verts := self vertices. 
	coefficients := {
		vertXs := verts collect: [:p | p x asFloat].
		slopeXs := self slopes: vertXs. 
		self changeInSlopes: slopeXs of: vertXs .
		self changeOfChangesInSlopes: slopeXs of: vertXs. 
		vertYs := verts collect: [:p | p y asFloat].
		slopeYs := self slopes: vertYs.
		self changeInSlopes: slopeYs of: vertYs.
		self changeOfChangesInSlopes: slopeYs of: vertYs. 
		Array new: verts size withAll: 12}.
		
		
	bestSegments := (1 to: verts size) collect: [:i | (self transform: coefficients toCubicPointPolynomialAt: i) bestSegments].
	coefficients at: 9 put:bestSegments.

	curveState := {coefficients. nil. nil}.
	self computeNextToEndPoints.
	^ coefficients
]

{ #category : #private }
RubSegmentMorph >> computeBounds [
	vertices ifNil: [^ self].
	self changed.
	self releaseCachedState.
	bounds := self curveBounds expanded.
	self layoutChanged.
	self changed.

]

{ #category : #smoothing }
RubSegmentMorph >> computeNextToEndPoints [
	| pointAfterFirst pointBeforeLast |
	pointAfterFirst := nil.
	self lineSegmentsDo: 
			[:p1 :p2 | 
			pointAfterFirst ifNil: [pointAfterFirst := p2 asIntegerPoint].
			pointBeforeLast := p1 asIntegerPoint].
	curveState at: 2 put: pointAfterFirst.
	curveState at: 3 put: pointBeforeLast
]

{ #category : #testing }
RubSegmentMorph >> containsPoint: aPoint [
	(super containsPoint: aPoint)
		ifFalse: [ ^ false ].
	color isTransparent
		ifFalse: [ ^ (self filledForm pixelValueAt: aPoint - bounds topLeft + 1) > 0 ].
	self
		lineSegmentsDo: [ :p1 :p2 | 
			(aPoint onLineFrom: p1 to: p2 within: (3 max: (borderWidth + 1) // 2) asFloat)
				ifTrue: [ ^ true ] ].
	^ false 
]

{ #category : #accessing }
RubSegmentMorph >> cornerStyle: aSymbol [
	"Set the receiver's corner style.  But, in this case, do *not*"

	(extension isNil or: [self cornerStyle == aSymbol]) ifTrue: [^self].
	extension cornerStyle: nil.
	self changed
]

{ #category : #accessing }
RubSegmentMorph >> couldHaveRoundedCorners [
	^ false
]

{ #category : #private }
RubSegmentMorph >> curveBounds [

	"Compute the bounds from actual curve traversal, with 
	leeway for borderWidth. 
	Also note the next-to-first and next-to-last points for arrow 
	directions."

	"wiz - to avoid roundoff errors we return unrounded curvebounds."

	"we expect our receiver to take responsibility for approriate rounding adjustment."

	"hint: this is most likely 'self curveBounds expanded' "

	| pointAfterFirst pointBeforeLast oX oY cX cY |

	self isCurvy
		ifFalse: [ ^ ( Rectangle encompassing: vertices ) expandBy: self borderWidth * 0.5 ].
	curveState := nil.	"Force recomputation"	"curveBounds := vertices first corner: vertices last."
	pointAfterFirst := nil.
	self
		lineSegmentsDo: [ :p1 :p2 | 
			pointAfterFirst
				ifNil: [ pointAfterFirst := p2 floor.
					oX := cX := p1 x.
					oY := cY := p1 y
					].	"curveBounds := curveBounds encompass: p2 ."
			oX := oX min: p2 x.
			cX := cX max: p2 x.
			oY := oY min: p2 y.
			cY := cY max: p2 y.
			pointBeforeLast := p1 floor
			].
	curveState at: 2 put: pointAfterFirst.
	curveState at: 3 put: pointBeforeLast.
	^ ( oX @ oY corner: cX @ cY ) expandBy: self borderWidth * 0.5
]

{ #category : #initialization }
RubSegmentMorph >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ Color
		r: 0.0
		g: 0.419
		b: 0.935
]

{ #category : #initialization }
RubSegmentMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Color orange
]

{ #category : #shaping }
RubSegmentMorph >> diamondOval [
	"Set my vertices to an array of edge midpoint vertices. Order of vertices is in the tradion of warpblt quads."
	
	| b |
	b := self bounds.
	self setVertices: { b leftCenter. b bottomCenter. b rightCenter. b topCenter }
]

{ #category : #drawing }
RubSegmentMorph >> drawBorderOn: aCanvas [ 
	self
		drawClippedBorderOn: aCanvas
		usingEnds: (Array with: vertices first with: vertices last)
]

{ #category : #drawing }
RubSegmentMorph >> drawBorderOn: aCanvas usingEnds: anArray [
	"Display my border on the canvas."

	| bigClipRect style |
	style := self borderStyle.
	bigClipRect := aCanvas clipRect expandBy: (self borderWidth + 1) // 2.
	self
		lineSegmentsDo: [ :p1 :p2 | 
			| p1i p2i |
			p1i := p1 asIntegerPoint.
			p2i := p2 asIntegerPoint.
			style drawLineFrom: p1i to: p2i on: aCanvas ]
]

{ #category : #drawing }
RubSegmentMorph >> drawBorderOnAthensCanvas: aCanvas [ 
	self
		drawClippedBorderOnAthensCanvas: aCanvas
		usingEnds: (Array with: vertices first with: vertices last)
]

{ #category : #drawing }
RubSegmentMorph >> drawBorderOnAthensCanvas: aCanvas usingEnds: anArray [
	"Display my border on the canvas."

	| bigClipRect style |
	style := self borderStyle.
	bigClipRect := aCanvas clipRect expandBy: (self borderWidth + 1) // 2.
	self
		lineSegmentsDo: [ :p1 :p2 | 
			| p1i p2i path |
			p1i := p1 asIntegerPoint.
			p2i := p2 asIntegerPoint.
			path := aCanvas
				createPath: [ :builder | 
					builder absolute.
					builder moveTo: p1i.
					builder lineTo: p2i ].
			(aCanvas setStrokePaint: style color) width: style width.
			aCanvas drawShape: path ]
]

{ #category : #drawing }
RubSegmentMorph >> drawClippedBorderOn: aCanvas usingEnds: anArray [ 
	aCanvas clipBy: self bounds during:[:cc| self drawBorderOn: cc usingEnds: anArray].
]

{ #category : #drawing }
RubSegmentMorph >> drawClippedBorderOnAthensCanvas: aCanvas usingEnds: anArray [
	aCanvas clipBy: self bounds during: [ self drawBorderOnAthensCanvas: aCanvas usingEnds: anArray ]
]

{ #category : #drawing }
RubSegmentMorph >> drawOn: aCanvas [ 
	"Display the receiver, a spline curve, approximated by straight 
	line segments."
	|  |
	aCanvas drawPolygon: self getVertices fillStyle: self fillStyle.
	self drawBorderOn: aCanvas

]

{ #category : #drawing }
RubSegmentMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Display the receiver, a spline curve, approximated by straight 
	line segments."
	| verts polygonPath |
	verts := self getVertices.
	polygonPath := anAthensCanvas
		createPath: [ :builder | 
			builder
				absolute;
				moveTo: verts first.
			verts allButFirstDo: [ :pt | builder lineTo: pt ].
			"builder close" ].
	anAthensCanvas setPaint: self fillStyle.
	anAthensCanvas setShape: polygonPath.
	anAthensCanvas draw.
	self drawBorderOnAthensCanvas: anAthensCanvas

]

{ #category : #geometry }
RubSegmentMorph >> extent: newExtent [
	"Not really advisable, but we can preserve most of the geometry if we don't
	shrink things too small."

	| safeExtent center |
	center := self referencePosition.
	safeExtent := newExtent max: 20 @ 20.
	self
		setVertices:
			(vertices collect: [ :p | (p - center) * (safeExtent asFloatPoint / (bounds extent max: 1 @ 1)) + center ])
]

{ #category : #private }
RubSegmentMorph >> filledForm [
	"Note: The filled form is actually 2 pixels bigger than bounds, and the point corresponding to this morphs' position is at 1@1 in the form.  This is due to the details of the fillig routines, at least one of which requires an extra 1-pixel margin around the outside.  Computation of the filled form is done only on demand."
	| bb origin |
	filledForm ifNotNil: [^ filledForm].
	filledForm := Form extent: bounds extent+2.

	"Draw the border..."
	bb := (BitBlt toForm: filledForm) sourceForm: nil; fillColor: Color black;
			combinationRule: Form over; width: 1; height: 1.
	origin := bounds topLeft asIntegerPoint-1.
	self lineSegmentsDo: [:p1 :p2 | bb drawFrom: p1 asIntegerPoint-origin
										to: p2 asIntegerPoint-origin].

	"Fill it in..."
	filledForm convexShapeFill: Color black.

	(borderColor isColor and: [borderColor isTranslucentButNotTransparent]) ifTrue: 
		["If border is stored as a form, then erase any overlap now."
		filledForm copy: self borderForm boundingBox from: self borderForm
			to: 1@1 rule: Form erase].

	^ filledForm
]

{ #category : #private }
RubSegmentMorph >> getVertices [

	smoothCurve ifFalse: [^ vertices].

	"For curves, enumerate the full set of interpolated points"
	^ Array streamContents:
		[:s | self lineSegmentsDo: [:pt1 :pt2 | s nextPut: pt1]]
]

{ #category : #initialization }
RubSegmentMorph >> initialize [
	super initialize.
	vertices := Array with: 5 @ 0 with: 20 @ 10 with: 0 @ 20.
	smoothCurve := false.
	self computeBounds.

]

{ #category : #geometry }
RubSegmentMorph >> intersects: aRectangle [ 
	"Answer whether any of my segments intersects aRectangle, which is in World coordinates."
	| rect |
	(super intersects: aRectangle) ifFalse: [ ^false ].
	rect := self bounds: aRectangle in: self world.
	self
		lineSegmentsDo: [:p1 :p2 | (rect intersectsLineFrom: p1 to: p2)
				ifTrue: [^ true]].
	^ false
]

{ #category : #testing }
RubSegmentMorph >> isCurve [
	^ smoothCurve
]

{ #category : #testing }
RubSegmentMorph >> isCurvy [
	"Test for significant curves.  
	Small smoothcurves in practice are straight."
	^ smoothCurve and: [vertices size > 2]
]

{ #category : #smoothing }
RubSegmentMorph >> lineSegmentsDo: endPointsBlock [ 
	"Emit a sequence of segment endpoints into endPointsBlock."
	"Unlike the method this one replaces we expect the curve 
	coefficents not the dirivatives"
	"Also unlike the replaced method the smooth closed curve
	does 
	not need an extra vertex. 
	We take care of the extra endpoint here. Just like for 
	segmented curves."
	| cs x y beginPoint |
	vertices size < 1
		ifTrue: [^ self].
	"test too few vertices first"
	self isCurvy
		ifFalse: [beginPoint := nil.
			"smoothCurve 
			ifTrue: [cs := self coefficients]."
			"some things still depend on smoothCurves having 
			curveState"
			vertices
				do: [:vert | 
					beginPoint
						ifNotNil: [endPointsBlock value: beginPoint value: vert].
					beginPoint := vert].
			endPointsBlock value: beginPoint value: vertices first.
			^ self].
	"For curves we include all the interpolated sub segments."
	"self assert: [(vertices size > 2 )].	"
	cs := self coefficients.
	beginPoint := (x := cs first first) @ (y := cs fifth first).
	1 to: cs first size
		do: [:i | | t n x3 y3 x1 endPoint x2 y1 y2 | 
			"taylor series coefficients"
			x1 := cs second at: i.
			y1 := cs sixth at: i.
			x2 := cs third at: i.
			y2 := cs seventh at: i.
			x3 := cs fourth at: i.
			y3 := cs eighth at: i.
			n := cs ninth at: i.
			"guess n 
			n := 5 max: (x2 abs + y2 abs * 2.0 + (cs third atWrap:
			i 
			+ 1) abs + (cs seventh atWrap: i + 1) abs / 100.0) 
			rounded."
			1
				to: n - 1
				do: [:j | 
					t := j asFloat / n asFloat.
					endPoint := x3 * t + x2 * t + x1 * t + x @ (y3 * t + y2 * t + y1 * t + y).
					endPointsBlock value: beginPoint value: endPoint.
					beginPoint := endPoint].
			endPoint := (x := cs first atWrap: i + 1) @ (y := cs fifth atWrap: i + 1).
			endPointsBlock value: beginPoint value: endPoint.
			beginPoint := endPoint]
]

{ #category : #geometry }
RubSegmentMorph >> merge: aPolygon [ 
	"Expand myself to enclose the other polygon.  (Later merge overlapping or disjoint in a smart way.)  For now, the two polygons must share at least two vertices.  Shared vertices must come one after the other in each polygon.  Polygons must not overlap."

	| shared mv vv hv xx |
	shared := vertices select: [:mine | aPolygon vertices includes: mine].
	shared size < 2 ifTrue: [^nil].	"not sharing a segment"
	mv := vertices asOrderedCollection.
	[shared includes: mv first] whileFalse: 
			["rotate them"

			vv := mv removeFirst.
			mv addLast: vv].
	hv := aPolygon vertices asOrderedCollection.
	[mv first = hv first] whileFalse: 
			["rotate him until same shared vertex is first"

			vv := hv removeFirst.
			hv addLast: vv].
	[shared size > 2] whileTrue: 
			[shared := shared asOrderedCollection.
			(self 
				mergeDropThird: mv
				in: hv
				from: shared) ifNil: [^nil]].
	"works by side effect on the lists"
	(mv second) = hv last 
		ifTrue: 
			[mv
				removeFirst;
				removeFirst.
			^self setVertices: (hv , mv) asArray].
	(hv second) = mv last 
		ifTrue: 
			[hv
				removeFirst;
				removeFirst.
			^self setVertices: (mv , hv) asArray].
	(mv second) = (hv second) 
		ifTrue: 
			[hv removeFirst.
			mv remove: (mv second).
			xx := mv removeFirst.
			^self setVertices: (hv , (Array with: xx) , mv reversed) asArray].
	mv last = hv last 
		ifTrue: 
			[mv removeLast.
			hv removeFirst.
			^self setVertices: (mv , hv reversed) asArray].
	^nil
]

{ #category : #geometry }
RubSegmentMorph >> mergeDropThird: mv in: hv from: shared [ 
	"We are merging two polygons.  In this case, they have at least three identical shared vertices.  Make sure they are sequential in each, and drop the middle one from vertex lists mv, hv, and shared.  First vertices on lists are identical already."

	"know (mv first = hv first)"

	| mdrop vv |
	(shared includes: (mv at: mv size - 2)) 
		ifTrue: [(shared includes: mv last) ifTrue: [mdrop := mv last]]
		ifFalse: 
			[(shared includes: mv last) 
				ifTrue: [(shared includes: mv second) ifTrue: [mdrop := mv first]]].
	(shared includes: (mv third)) 
		ifTrue: [(shared includes: mv second) ifTrue: [mdrop := mv second]].
	mdrop ifNil: [^nil].
	mv remove: mdrop.
	hv remove: mdrop.
	shared remove: mdrop.
	[shared includes: mv first] whileFalse: 
			["rotate them"

			vv := mv removeFirst.
			mv addLast: vv].
	[mv first = hv first] whileFalse: 
			["rotate him until same shared vertex is first"

			vv := hv removeFirst.
			hv addLast: vv]
]

{ #category : #accessing }
RubSegmentMorph >> midVertices [
	"Return and array of midpoints for this line or closed curve"
	| midPts nextVertIx tweens |
	vertices size < 2
		ifTrue: [^ vertices].
	midPts := OrderedCollection new.
	nextVertIx := 2.
	tweens := OrderedCollection new.
	tweens add: vertices first asIntegerPoint.
	"guarantee at least two points."
	self
		lineSegmentsDo: [:p1 :p2 | 
			tweens addLast: p2 asIntegerPoint.
			p2 = (vertices atWrap: nextVertIx)
				ifTrue: ["Found endPoint."
					midPts addLast: (tweens atWrap: tweens size + 1 // 2)
							+ (tweens at: tweens size // 2 + 1) // 2.
					tweens := OrderedCollection new.
					tweens add: p2 asIntegerPoint.
					"guarantee at least two points."
					nextVertIx := nextVertIx + 1]].
	^ midPts asArray
]

{ #category : #'cubic support' }
RubSegmentMorph >> naturalCubicSlopesOf: knots [ 
	"Sent to knots returns the slopes of a natural cubic curve fit."
	"We solve the equation for knots with end conditions:  
	2*b1+b2 = 3(a1 - a0) 
	bN1+2*bN = 3*(aN-aN1)
	and inbetween:
	b2+4*b3+b4=3*(a4-a2)
	where a2 is (knots atWrap: index + 1) etc.
	and the b's are the slopes .
	N is the last index (knots size)
	N1 is N-1.
	 
	by using row operations to convert the matrix to upper  
	triangular and then back sustitution. The D[i] are the derivatives at the  
	knots."
	
	| x gamma delta D n1 |
	n1 := knots size.
	n1 < 3
		ifTrue: [self error: 'Less than 3 points makes a poor curve'].
	x := knots.
	gamma := Array new: n1.
	delta := Array new: n1.
	
	D := Array new: n1.
	gamma at: 1 put: 1.0 / 2.0.
	2 to: n1 - 1 do: [:i | gamma at: i put: 1.0 / (4.0
						- (gamma at: i - 1))].
	gamma at: n1 put: 1.0 / (2.0
				- (gamma at: n1 - 1)).
	delta at: 1 put: 3.0 * ((x at: 2)
				- (x at: 1))
			* (gamma at: 1).
	2 to: n1 - 1 do: [:i | delta at: i put: 3.0 * ((x at: i + 1)
						- (x at: i - 1))
					- (delta at: i - 1)
					* (gamma at: i)].
	delta at: n1 put: 3.0 * ((x at: n1)
				- (x at: n1 - 1))
			- (delta at: n1 - 1)
			* (gamma at: n1).
	D
		at: n1
		put: (delta at: n1).
	(1 to: n1 - 1)
		reverseDo: [:i | D at: i put: (delta at: i)
					- ((gamma at: i)
							* (D at: i + 1))].
	^ D
]

{ #category : #private }
RubSegmentMorph >> privateMoveBy: delta [
	super privateMoveBy: delta.
	vertices := vertices collect: [:p | p + delta].
	curveState := nil.  "Force recomputation"
	(self valueOfProperty: #referencePosition) ifNotNil:
		[:oldPos | self setProperty: #referencePosition toValue: oldPos + delta]
]

{ #category : #geometry }
RubSegmentMorph >> referencePosition [ 
	"Return the current reference position of the receiver"
	^ self valueOfProperty: #referencePosition ifAbsent: [super referencePosition]

]

{ #category : #caching }
RubSegmentMorph >> releaseCachedState [

	super releaseCachedState.
	filledForm := nil.
	borderForm := nil.
	curveState := nil.
	(self hasProperty: #flex) ifTrue:
		[self removeProperty: #flex].

]

{ #category : #'cubic support' }
RubSegmentMorph >> segmentedSlopesOf: knots [ 
	"For a collection of floats. Returns the slopes for straight 
	segments between vertices."
	"last slope closes the polygon. Always return same size as 
	self. "
	^ knots collectWithIndex: [:x :i | (knots atWrap: i + 1) - x]
]

{ #category : #private }
RubSegmentMorph >> setVertices: newVertices [
	vertices := newVertices.
	self computeBounds
]

{ #category : #smoothing }
RubSegmentMorph >> slopes: knots [ 
	"Choose slopes according to state of polygon"
	self isCurvy ifFalse: [^ self segmentedSlopesOf: knots ].
	^ self closedCubicSlopesOf: knots
]

{ #category : #'cubic support' }
RubSegmentMorph >> transform: coefficients toCubicPointPolynomialAt: vIndex [ 
	"From curve information assemble a 4-array of points representing the coefficents for curve segment between to points. Beginning point is first point in array endpoint is the pointSum of the array. Meant to be sent to newcurves idea of curve coefficents." 
	| transformed |
	transformed := (1 to: 4) collect: [:i | 
			((coefficients at: i) at: vIndex) @ ((coefficients at: 4 + i) at: vIndex)].
	^ Cubic withAll: transformed
]

{ #category : #geometry }
RubSegmentMorph >> transformedBy: aTransform [
	self setVertices: (self vertices collect:[:v| aTransform localPointToGlobal: v])
]

{ #category : #accessing }
RubSegmentMorph >> vertices [
	^ vertices
]

{ #category : #initialization }
RubSegmentMorph >> vertices: verts color: aColor borderWidth: borderWidthInteger borderColor: anotherColor [ 
	super initialize.
	vertices := verts.
	color := aColor.
	borderWidth := borderWidthInteger. 
	borderColor := anotherColor.
	self computeBounds
]
