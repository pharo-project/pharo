"
This class implements a morph which can behave as four different objects depending on the the following two facts:
- is it OPEN or CLOSED?
- is it SEGMENTED or SMOOTHED.

1. The OPEN and SEGMENTED variant looks like polyline.

2. The OPEN and SMOOTHED variant looks like spline (kind of curve)

3. The CLOSED and SEGMENTED variant looks like polygon. This is actually what you get when you do
	PolygonMorph new openInWorld
You get a triangle. See below how to manipulate these objects...

4. The CLOSED and SMOOTHED variant looks like blob (???)

Prototypes of this morph can also be found in ""Object Catalog"". Several (different variants) of this object are among ""Basic"" morphs.

Explore the assiciated morph-menu. It enables you
- to toggle showing of ""handles"". They make it possible to
	- reposition already existing vertices (by moving yellow handles)
	- create new vertices (by moving green handles)
	- delete already existing vertices (by dragging and dropping one yellow handle closely
	  nearby the adjacent yellow handle
  Handles can be made visible/hidden by shift+leftclicking the morph. This way it is possible
  to quickly show handles, adjust vertices and then again hide handles.
- making closed polygon open, i.e. converting it to a curve (and vice versa)
- toggle smoothed/segmented line/outline
- set up custom dashing (for line, curves or borders of closed polygons
- set up custom arrow-heads (for lines resp. curves)

------------------------------------------------------------------------------------------
Implementation notes:

This class combines the old Polygon and Curve classes.

The 1-bit fillForm to make display and containment tests reasonably fast.  However, this functionality is in the process of being supplanted by balloon capabilities, which should eventually provide anti-aliasing as well.

wiz 7/18/2004 21:26
s have made some changes to this class to

1) correct some bugs associated with one vertex polygons.

2) prepare for some enhancements with new curves.

3) add shaping items to menu.
"
Class {
	#name : #PolygonMorph,
	#superclass : #BorderedMorph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#instVars : [
		'vertices',
		'closed',
		'filledForm',
		'arrows',
		'arrowForms',
		'smoothCurve',
		'curveState',
		'borderDashSpec',
		'handles',
		'borderForm'
	],
	#category : #'Morphic-Base-Basic'
}

{ #category : #'instance creation' }
PolygonMorph class >> arrowPrototype [
	"Answer an instance of the receiver that will serve as a prototypical arrow"
	"PolygonMorph arrowPrototype openInWorld"
	
	| aa |
	aa := self new. 
	aa vertices: (Array with: 0@0 with: 40@40) 
		color: Color black 
		borderWidth: 2 
		borderColor: Color black.
	aa makeForwardArrow.	
	aa computeBounds.
	^ aa
]

{ #category : #settings }
PolygonMorph class >> defaultArrowSpec [
	^ 5@4
]

{ #category : #examples }
PolygonMorph class >> example1 [
	<sampleInstance>
	"self example1"
	^ (PolygonMorph
		vertices: {261@400. 388@519. 302@595. 	222@500.	141@583. 34@444}
		color: Color blue
		borderWidth: 3
		borderColor: Color black) openInWorld
]

{ #category : #examples }
PolygonMorph class >> example2 [
	"self example2"
	| poly | 
	poly := PolygonMorph
		vertices: {261@400. 388@519. 302@595. 	222@500.	141@583. 34@444}
		color: Color blue
		borderWidth: 3
		borderColor: Color black.
	poly beSmoothCurve.
	poly openInWorld
]

{ #category : #examples }
PolygonMorph class >> example3 [
	"self example3"
	| poly | 
	poly := PolygonMorph
				vertices: {261@400. 388@519. 302@595. 	222@500.	141@583. 34@444}
				color: Color blue
				borderWidth: 3
				borderColor: Color black.
	poly makeOpen.
	poly openInWorld
]

{ #category : #examples }
PolygonMorph class >> example4 [
	"self example4"
	| poly | 
	poly :=  (PolygonMorph
		vertices: {261@400. 388@519. 302@595. 	222@500.	141@583. 34@444}
		color: Color blue
		borderWidth: 3
		borderColor: Color black).
	poly dashedBorder: { 5 .
		5.
		Color red.
		50 .
		0 }.
	poly openInWorld
]

{ #category : #'instance creation' }
PolygonMorph class >> fromHand: hand [
	"Let the user draw a polygon, clicking at each vertex, and ending by clicking within 5 of the first point..."
	"self fromHand: ActiveHand"
	
	| p1 poly oldVerts pN opposite |
	"wait till guy will press the mouse button"
	hand captureEventsUntil: [:evt |
		evt isMouse ifTrue: [ p1 := evt cursorPoint].
		(evt isMouse and: [ evt anyButtonPressed ]) not ].
	
	opposite := (Display colorAt: p1) negated.
	opposite = Color transparent ifTrue: [opposite := Color red].
	(poly := LineMorph from: p1 to: p1 color: opposite width: 2) openInWorld.
	oldVerts := {p1}.
	
	[true] whileTrue: [
		"wait till button release"
		hand captureEventsWhile: [:evt |
			evt isMouse 
				ifTrue: [ 
					pN := evt cursorPoint.
					poly setVertices: (oldVerts copyWith: pN).
					evt anyButtonPressed ] 
				ifFalse: [ true ] ].

		(oldVerts size > 1 and: [(pN distanceTo: p1) < 5]) 
			ifFalse: [
				oldVerts := poly vertices.
				"loop till button will be pressed"
				hand captureEventsUntil: [:evt | 		
					evt isMouse 
						ifTrue: [ 
							pN := evt cursorPoint.
							poly setVertices: (oldVerts copyWith: pN).
							evt anyButtonPressed ] 
					ifFalse: [ false ]]]
			ifTrue: [ ^ (poly setVertices: (poly vertices copyWith: p1)) delete].
	]
]

{ #category : #'instance creation' }
PolygonMorph class >> fromHandFreehand: hand [
	"Let the user draw a polygon, holding the mouse down, and ending by clicking within 5 of the first point..."
	"self fromHandFreehand: ActiveHand"
	
	| p1 poly pN opposite |
	"wait till guy will press the mouse button"
	hand captureEventsUntil: [:evt |
		evt isMouse ifTrue: [ p1 := evt cursorPoint].
		(evt isMouse and: [ evt anyButtonPressed ])  ].
	
	opposite := (Display colorAt: p1) negated.
	opposite = Color transparent ifTrue: [opposite := Color red].
	(poly := LineMorph from: p1 to: p1 color: opposite width: 2) openInWorld.
	
	hand captureEventsWhile: [:evt |
		
		evt isMouse ifTrue:[ 
			pN := evt cursorPoint.
			(pN distanceTo: poly vertices last) > 3 ifTrue:
				[poly setVertices: (poly vertices copyWith: pN) ].
			
			evt anyButtonPressed 
			]
		ifFalse: [ true ]
	].
	^ (poly setVertices: (poly vertices copyWith: p1)) delete

]

{ #category : #'instance creation' }
PolygonMorph class >> vertices: verts color: c borderWidth: bw borderColor: bc [
	"(PolygonMorph
		vertices: {261@400. 388@519. 302@595. 	222@500.	141@583. 34@444}
		color: Color blue
		borderWidth: 3
		borderColor: Color black) openInWorld"
	^ self basicNew beStraightSegments vertices: verts color: c borderWidth: bw borderColor: bc
]

{ #category : #menu }
PolygonMorph >> addCustomMenuItems: aMenu hand: aHandMorph [ 
	| |
	super addCustomMenuItems: aMenu hand: aHandMorph.
	aMenu
		addUpdating: #handlesShowingPhrase
		target: self
		selector: #showOrHideHandles.
	vertices size > 2
		ifTrue: [ self addPolyLIneCurveMenuItems: aMenu hand: aHandMorph ].
	aMenu add: 'specify dashed line' selector: #specifyDashedLine.
	self isOpen
		ifTrue: [self addPolyArrowMenuItems: aMenu hand: aHandMorph]
			ifFalse: [self addPolyShapingMenuItems: aMenu hand: aHandMorph]
]

{ #category : #editing }
PolygonMorph >> addHandles [
	"Put moving handles at the vertices. Put adding handles at
	edge midpoints.
	Moving over adjacent vertex and dropping will delete a
	vertex. "
	| tri |
	self removeHandles.
	handles := OrderedCollection new.
	tri := Array
				with: 0 @ -4
				with: 4 @ 3
				with: -3 @ 3.
	vertices
		withIndexDo: [:vertPt :vertIndex | | handle newVert | 
			handle := EllipseMorph
						newBounds: (Rectangle center: vertPt extent: 8 @ 8)
						color: (self handleColorAt: vertIndex) .
			handle
				on: #mouseMove
				send: #dragVertex:event:fromHandle:
				to: self
				withValue: vertIndex.
			handle
				on: #mouseUp
				send: #dropVertex:event:fromHandle:
				to: self
				withValue: vertIndex.
				handle
				on: #click
				send: #clickVertex:event:fromHandle:
				to: self
				withValue: vertIndex.
			self addMorph: handle.
			handles addLast: handle.
			(closed
					or: [1 = vertices size
						"Give a small polygon a chance to grow. 
						-wiz"
					or: [vertIndex < vertices size]])
				ifTrue: [newVert := PolygonMorph
								vertices: (tri
										collect: [:p | p + (vertPt
													+ (vertices atWrap: vertIndex + 1) // 2)])
								color: Color green
								borderWidth: 1
								borderColor: Color black.
					newVert
						on: #mouseDown
						send: #newVertex:event:fromHandle:
						to: self
						withValue: vertIndex.
					self addMorph: newVert.
					handles addLast: newVert]].
	self isCurvy
		ifTrue: [self updateHandles; layoutChanged].
	self changed
]

{ #category : #menu }
PolygonMorph >> addPolyArrowMenuItems: aMenu hand: aHandMorph [
	aMenu addLine.
	aMenu addWithLabel: '---' enablement: [ self isOpen and: [ arrows ~~ #none ] ] action: #makeNoArrows.
	aMenu addWithLabel: '-->' enablement: [ self isOpen and: [ arrows ~~ #forward ] ] action: #makeForwardArrow.
	aMenu addWithLabel: '<--' enablement: [ self isOpen and: [ arrows ~~ #back ] ] action: #makeBackArrow.
	aMenu addWithLabel: '<->' enablement: [ self isOpen and: [ arrows ~~ #both ] ] action: #makeBothArrows.
	aMenu add: 'customize arrows' selector: #customizeArrows:.
	(self hasProperty: #arrowSpec)
		ifTrue: [ aMenu add: 'standard arrows' selector: #standardArrows ]
]

{ #category : #menu }
PolygonMorph >> addPolyLIneCurveMenuItems: aMenu hand: aHandMorph [ 

	aMenu addLine;
				addUpdating: #openOrClosePhrase
				target: self
				selector: #toggleOpenOrClosed.
			
			aMenu
				addUpdating: #smoothOrSegmentedPhrase
				target: self
				selector: #toggleSmoothing.
]

{ #category : #menu }
PolygonMorph >> addPolyShapingMenuItems: aMenu hand: aHandMorph [
	aMenu addLine.
	aMenu addWithLabel: 'make inscribed diamondOval' enablement: [ self isClosed ] action: #diamondOval.
	aMenu addWithLabel: 'make enclosing rectangleOval' enablement: [ self isClosed ] action: #rectangleOval
]

{ #category : #drawing }
PolygonMorph >> areasRemainingToFill: aRectangle [
	"Could be improved by quick check of inner rectangle"

	^ Array with: aRectangle
]

{ #category : #private }
PolygonMorph >> arrowBoundsAt: endPoint from: priorPoint [ 
	"Answer a triangle oriented along the line from priorPoint to endPoint."
	| d v angle wingBase arrowSpec length width |
	v := endPoint - priorPoint.
	angle := v degrees.
	d := borderWidth max: 1.
	arrowSpec := self valueOfProperty: #arrowSpec ifAbsent: [PolygonMorph defaultArrowSpec].
	length := arrowSpec x abs.  width := arrowSpec y abs.
	wingBase := endPoint + (Point r: d * length degrees: angle + 180.0).
	arrowSpec x >= 0
		ifTrue: [^ {	endPoint.
					wingBase + (Point r: d * width degrees: angle + 125.0).
					wingBase + (Point r: d * width degrees: angle - 125.0) }]
		ifFalse: ["Negative length means concave base."
				^ {	endPoint.
					wingBase + (Point r: d * width degrees: angle + 125.0).
					wingBase.
					wingBase + (Point r: d * width degrees: angle - 125.0) }]
]

{ #category : #private }
PolygonMorph >> arrowForms [
	"ArrowForms are computed only upon demand"
	arrowForms
		ifNotNil: [^ arrowForms].
	arrowForms := Array new.
	self hasArrows
		ifFalse: [^ arrowForms].
	(arrows == #forward
			or: [arrows == #both])
		ifTrue: [arrowForms := arrowForms
						copyWith: (self computeArrowFormAt: vertices last from: self nextToLastPoint)].
	(arrows == #back
			or: [arrows == #both])
		ifTrue: [arrowForms := arrowForms
						copyWith: (self computeArrowFormAt: vertices first from: self nextToFirstPoint)].
	^ arrowForms
]

{ #category : #menu }
PolygonMorph >> arrowLength: aLength [
	"Assumes that I have exactly two vertices"

	| theta horizontalOffset verticalOffset newTip delta |
	delta := vertices second - vertices first.
	theta := delta theta.
	horizontalOffset := aLength * (theta cos).
	verticalOffset := aLength * (theta sin).
	newTip := vertices first + (horizontalOffset @ verticalOffset).
	self verticesAt: 2 put: newTip
]

{ #category : #menu }
PolygonMorph >> arrowSpec: specPt [
	"Specify a custom arrow for this line.
	specPt x abs gives the length of the arrow (point to base) in terms of borderWidth.
	If specPt x is negative, then the base of the arrow will be concave.
	specPt y abs gives the width of the arrow.
	The standard arrow is equivalent to arrowSpec: PolygonMorph defaultArrowSpec.
	See arrowBoundsAt:From: for details."

	self setProperty: #arrowSpec toValue: specPt.
	self computeBounds
]

{ #category : #menu }
PolygonMorph >> arrows [
	^arrows
]

{ #category : #geometry }
PolygonMorph >> arrowsContainPoint: aPoint [
	"Answer an Array of two Booleans that indicate whether the given point is inside either arrow"

	| retval f |
	retval := { false . false }.
	(super containsPoint: aPoint) ifFalse: [^ retval ].
	(closed or: [arrows == #none or: [vertices size < 2]]) ifTrue: [^ retval].

	(arrows == #forward or: [arrows == #both]) ifTrue: [	"arrowForms first has end form"
		f := self arrowForms first.
		retval at: 2 put: ((f pixelValueAt: aPoint - f offset) > 0)
	].
	(arrows == #back or: [arrows == #both]) ifTrue: [ "arrowForms last has start form"
		f := self arrowForms last.
		retval at: 1 put: ((f pixelValueAt: aPoint - f offset) > 0)
	].
	^retval.
]

{ #category : #initialization }
PolygonMorph >> beSmoothCurve [

	smoothCurve == true ifFalse:
		[smoothCurve := true.
		self computeBounds]
]

{ #category : #initialization }
PolygonMorph >> beStraightSegments [

	smoothCurve == false ifFalse:
		[smoothCurve := false.
		self computeBounds]
]

{ #category : #accessing }
PolygonMorph >> borderColor: aColor [ 

	super borderColor: aColor.
	(borderColor isColor and: [borderColor isTranslucentButNotTransparent]) 
		== (aColor isColor and: [aColor isTranslucentButNotTransparent]) 
			ifFalse: 
				["Need to recompute fillForm and borderForm
					if translucency of border changes."

				self releaseCachedState ]
]

{ #category : #dashes }
PolygonMorph >> borderDashOffset [
	borderDashSpec size < 4 ifTrue: [^0.0].
	^ (borderDashSpec fourth) asFloat
]

{ #category : #private }
PolygonMorph >> borderForm [
	"A form must be created for drawing the border whenever the borderColor is translucent."

	| borderCanvas |
	borderForm ifNotNil: [^ borderForm].
	borderCanvas := (Display defaultCanvasClass extent: bounds extent depth: 1)
		asShadowDrawingCanvas: Color black.
	borderCanvas translateBy: bounds topLeft negated
		during:[:tempCanvas| self drawBorderOn: tempCanvas].
	borderForm := borderCanvas form.
	self arrowForms do:
		[:f |  "Eliminate overlap between line and arrowheads if transparent."
		borderForm copy: f boundingBox from: f to: f offset - self position rule: Form erase].
	^ borderForm
]

{ #category : #accessing }
PolygonMorph >> borderWidth: anInteger [

	borderColor ifNil: [ borderColor := Color black ].
	borderWidth := anInteger max: 0.
	self computeBounds
]

{ #category : #geometry }
PolygonMorph >> bounds: newBounds [
	"This method has to be reimplemented since self extent: will also change self bounds origin,
	super bounds would leave me in wrong position when container is growing.
	Always change extent first then position"
	
	self extent: newBounds extent; position: newBounds topLeft

]

{ #category : #'cubic support' }
PolygonMorph >> changeInSlopes: slopes of: verts [ 
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
PolygonMorph >> changeOfChangesInSlopes: slopes of: verts [
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

{ #category : #editing }
PolygonMorph >> clickVertex: ix event: evt fromHandle: handle [
	"Backstop for MixedCurveMorph"
]

{ #category : #'cubic support' }
PolygonMorph >> closedCubicSlopesOf: knots [ 
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

{ #category : #geometry }
PolygonMorph >> closestPointTo: aPoint [ 
	| closestPoint minDist |
	closestPoint := minDist := nil.
	self lineSegmentsDo: 
			[:p1 :p2 | | dist curvePoint | 
			curvePoint := aPoint nearestPointOnLineFrom: p1 to: p2.
			dist := curvePoint distanceTo: aPoint.
			(closestPoint isNil or: [dist < minDist]) 
				ifTrue: 
					[closestPoint := curvePoint.
					minDist := dist]].
	^closestPoint
]

{ #category : #geometry }
PolygonMorph >> closestSegmentTo: aPoint [
	"Answer the starting index of my (big) segment nearest to aPoint"
	| closestPoint minDist vertexIndex closestVertexIndex |
	vertexIndex := 0.
	closestVertexIndex := 0.
	closestPoint := minDist := nil.
	self lineSegmentsDo:
		[:p1 :p2 | | curvePoint dist | 
		(p1 = (self vertices at: vertexIndex + 1))
			ifTrue: [ vertexIndex := vertexIndex + 1 ].
		curvePoint := aPoint nearestPointOnLineFrom: p1 to: p2.
		dist := curvePoint distanceTo: aPoint.
		(closestPoint isNil or: [dist < minDist])
			ifTrue: [closestPoint := curvePoint.
					minDist := dist.
					closestVertexIndex := vertexIndex. ]].
	^ closestVertexIndex
]

{ #category : #smoothing }
PolygonMorph >> coefficients [
	curveState ifNotNil: [^ curveState at: 1].
	^ self vertices size < 1 
		ifTrue: [ self  ]
		ifFalse: [ self coefficientsForMoreThanThreePoints ].  
]

{ #category : #smoothing }
PolygonMorph >> coefficientsForMoreThanThreePoints [ 
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
PolygonMorph >> computeArrowFormAt: endPoint from: priorPoint [ 
	"Compute a triangle oriented along the line from priorPoint to  
	endPoint. Then draw those lines in a form and return that  
	form, with appropriate offset"

	| p1 pts box arrowForm bb origin |
	pts := self arrowBoundsAt: endPoint from: priorPoint.
	box := ((pts first rectangle: pts last) encompass: (pts second)) expandBy: 1.
	arrowForm := Form extent: box extent asIntegerPoint.
	bb := (BitBlt toForm: arrowForm)
				sourceForm: nil;
				fillColor: Color black;
				combinationRule: Form over;
				width: 1;
				height: 1.
	origin := box topLeft.
	p1 := pts last - origin.
	pts do: 
			[:p | 
			bb drawFrom: p1 to: p - origin.
			p1 := p - origin].
	arrowForm convexShapeFill: Color black.
	^arrowForm offset: box topLeft
]

{ #category : #private }
PolygonMorph >> computeBounds [
	| oldBounds delta excludeHandles |
	vertices ifNil: [^ self].

	self changed.
	oldBounds := bounds.
	self releaseCachedState.
	bounds := self curveBounds expanded.
	self arrowForms do:
		[:f | bounds := bounds merge: (f offset extent: f extent)].
	handles ifNotNil: [self updateHandles].

	"since we are directly updating bounds, see if any ordinary submorphs exist and move them accordingly"
	(oldBounds notNil and: [(delta := bounds origin - oldBounds origin) ~= (0@0)]) ifTrue: [
		excludeHandles := IdentitySet new.
		handles ifNotNil: [excludeHandles addAll: handles].
		self submorphsDo: [ :each |
			(excludeHandles includes: each) ifFalse: [
				each position: each position + delta
			].
		].
	].
	self layoutChanged.
	self changed.

]

{ #category : #smoothing }
PolygonMorph >> computeNextToEndPoints [
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
PolygonMorph >> containsPoint: aPoint [
	(super containsPoint: aPoint) ifFalse: [^ false].

	(closed and: [color isTransparent not]) ifTrue:
		[ ^ (self filledForm pixelValueAt: aPoint - bounds topLeft + 1) > 0].

	self lineSegmentsDo:
		[ :p1 :p2 |
		(aPoint onLineFrom: p1 to: p2 within: (3 max: borderWidth+1//2) asFloat)
				ifTrue: [^ true]].

	self arrowForms do:
		[ :f | (f pixelValueAt: aPoint - f offset) > 0 ifTrue: [^ true]].

	^ false
]

{ #category : #accessing }
PolygonMorph >> cornerStyle: aSymbol [
	"Set the receiver's corner style.  But, in this case, do *not*"

	(extension isNil or: [self cornerStyle == aSymbol]) ifTrue: [^self].
	extension cornerStyle: nil.
	self changed
]

{ #category : #accessing }
PolygonMorph >> couldHaveRoundedCorners [
	^ false
]

{ #category : #private }
PolygonMorph >> curveBounds [

	"Compute the bounds from actual curve traversal, with 
	leeway for borderWidth. 
	Also note the next-to-first and next-to-last points for arrow 
	directions."

	"wiz - to avoid roundoff errors we return unrounded curvebounds."

	"we expect our receiver to take responsibility for approriate rounding adjustment."

	"hint: this is most likely 'self curveBounds expanded' "

	| pointAfterFirst pointBeforeLast oX oY cX cY |

	self isCurvy
		ifFalse: [ ^ ( Rectangle encompassing: vertices ) expandBy: borderWidth * 0.5 ].
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
	^ ( oX @ oY corner: cX @ cY ) expandBy: borderWidth * 0.5
]

{ #category : #menu }
PolygonMorph >> customizeArrows: evt [
	| handle origin aHand |
	aHand := evt ifNil: [self primaryHand] ifNotNil: [evt hand].
	origin := aHand position.
	handle := HandleMorph new
		forEachPointDo:
			[:newPoint | handle removeAllMorphs.
			handle addMorph:
				(LineMorph from: origin to: newPoint color: Color black width: 1).
			self arrowSpec: (newPoint - origin) / 5.0]
		lastPointDo:
			[:newPoint | handle deleteBalloon.
			self halo ifNotNil: [:halo | halo addHandles].].
	aHand attachMorph: handle.
	handle showBalloon:
'Move cursor left and right
to change arrow length and style.
Move it up and down to change width.
Click when done.' hand: evt hand.
	handle startStepping
]

{ #category : #dashes }
PolygonMorph >> dashedBorder [
	^borderDashSpec
	"A dash spec is a 3- or 5-element array with
		{ length of normal border color.
		length of alternate border color.
		alternate border color.
		starting offset.
		amount to add to offset at each step }
	Starting offset is usually = 0, but changing it moves the dashes along the curve."

]

{ #category : #dashes }
PolygonMorph >> dashedBorder: dashSpec [
	"A dash spec is a 3- or 5-element array with
		{ length of normal border color.
		length of alternate border color.
		alternate border color.
		starting offset.
		amount to add to offset at each step }
	Starting offset is usually = 0, but changing it moves the dashes along the curve."

	borderDashSpec := dashSpec.
	self changed
]

{ #category : #attachments }
PolygonMorph >> defaultAttachmentPointSpecs [
	^{ 
		{ #firstVertex } .
		{ #midpoint } .
		{ #lastVertex }
		}
]

{ #category : #initialization }
PolygonMorph >> defaultBorderColor [
	"answer the default border color/fill style for the receiver"
	^ Color
		r: 0.0
		g: 0.419
		b: 0.935
]

{ #category : #initialization }
PolygonMorph >> defaultColor [
	"answer the default color/fill style for the receiver"
	^ Color orange
]

{ #category : #editing }
PolygonMorph >> deleteVertexAt: anIndex [
	"This acts as a backstop for MixedCurveMorph."
	self setVertices: (vertices
						copyReplaceFrom: anIndex
						to: anIndex
						with: #()).
						
]

{ #category : #smoothing }
PolygonMorph >> derivs: a first: point1 second: point2 third: point3 [ 
	"Compute the first, second and third derivitives (in coeffs) from
	the Points in this Path (coeffs at: 1 and coeffs at: 5)."

	| len v anArray |
	len := a size.
	len < 2 ifTrue: [^self].
	len > 2 
		ifTrue: 
			[v := Array new: len.
			v at: 1 put: 4.0.
			anArray := Array new: len.
			anArray at: 1 put: 6.0 * (a first - (a second * 2.0) + (a third)).
			2 to: len - 2
				do: 
					[:i | 
					v at: i put: 4.0 - (1.0 / (v at: i - 1)).
					anArray at: i
						put: 6.0 * ((a at: i) - ((a at: i + 1) * 2.0) + (a at: i + 2)) 
								- ((anArray at: i - 1) / (v at: i - 1))].
			point2 at: len - 1 put: (anArray at: len - 2) / (v at: len - 2).
			len - 2 to: 2
				by: 0 - 1
				do: 
					[:i | 
					point2 at: i
						put: ((anArray at: i - 1) - (point2 at: i + 1)) / (v at: i - 1)]].
	point2 at: 1 put: (point2 at: len put: 0.0).
	1 to: len - 1
		do: 
			[:i | 
			point1 at: i
				put: (a at: i + 1) - (a at: i) 
						- (((point2 at: i) * 2.0 + (point2 at: i + 1)) / 6.0).
			point3 at: i put: (point2 at: i + 1) - (point2 at: i)]
]

{ #category : #shaping }
PolygonMorph >> diamondOval [
	"Set my vertices to an array of edge midpoint vertices. Order of vertices is in the tradion of warpblt quads."
	
	| b |
	b := self bounds.
	self setVertices: { b leftCenter. b bottomCenter. b rightCenter. b topCenter }
]

{ #category : #editing }
PolygonMorph >> dragVertex: ix event: evt fromHandle: handle [
	| p |
	p := evt cursorPoint.
	handle position: p - (handle extent // 2).
	self verticesAt: ix put: p
]

{ #category : #drawing }
PolygonMorph >> drawArrowOn: aCanvas at: endPoint from: priorPoint [ 
	"Draw a triangle oriented along the line from priorPoint to  
	endPoint. Answer the wingBase."

	| pts spec wingBase |
	pts := self arrowBoundsAt: endPoint from: priorPoint.
	wingBase := pts size = 4 
				ifTrue: [pts third]
				ifFalse: [(pts copyFrom: 2 to: 3) average].
	spec := self valueOfProperty: #arrowSpec ifAbsent: [PolygonMorph defaultArrowSpec].
	spec x sign = spec y sign 
		ifTrue: [aCanvas drawPolygon: pts fillStyle: borderColor]
		ifFalse: 
			[aCanvas 
				drawPolygon: pts
				fillStyle: Color transparent
				borderWidth: (borderWidth + 1) // 2
				borderColor: borderColor].
	^wingBase
]

{ #category : #drawing }
PolygonMorph >> drawArrowsOn: aCanvas [ 
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
				drawArrowOn: aCanvas
				at: vertices last
				from: self nextToLastPoint) ].

	(arrows == #back or: [arrows == #both])
		ifTrue: [ array at: 1 put: (self
				drawArrowOn: aCanvas
				at: vertices first
				from: self nextToFirstPoint) ].

	^array
]

{ #category : #drawing }
PolygonMorph >> drawBorderOn: aCanvas [ 
	self
		drawClippedBorderOn: aCanvas
		usingEnds: (Array with: vertices first with: vertices last)
]

{ #category : #drawing }
PolygonMorph >> drawBorderOn: aCanvas usingEnds: anArray [ 
	"Display my border on the canvas."
	"NOTE: Much of this code is also copied in  
	drawDashedBorderOn:  
	(should be factored)"
	| bigClipRect style |
	borderDashSpec
		ifNotNil: [^ self drawDashedBorderOn: aCanvas usingEnds: anArray].
	style := self borderStyle.
	bigClipRect := aCanvas clipRect expandBy: self borderWidth + 1 // 2.
	self
		lineSegmentsDo: [:p1 :p2 | | p1i p2i | 
			p1i := p1 asIntegerPoint.
			p2i := p2 asIntegerPoint.
			self hasArrows
				ifTrue: ["Shorten line ends so as not to interfere with tip  
					of arrow."
					((arrows == #back
								or: [arrows == #both])
							and: [p1 = vertices first])
						ifTrue: [p1i := anArray first asIntegerPoint].
					((arrows == #forward
								or: [arrows == #both])
							and: [p2 = vertices last])
						ifTrue: [p2i := anArray last asIntegerPoint]].
			(closed
					or: ["bigClipRect intersects: (p1i rect: p2i)  
						optimized:"
						((p1i min: p2i)
							max: bigClipRect origin)
							<= ((p1i max: p2i)
									min: bigClipRect corner)])
				ifTrue: [style
						drawLineFrom: p1i
						to: p2i
						on: aCanvas]]
]

{ #category : #drawing }
PolygonMorph >> drawClippedBorderOn: aCanvas usingEnds: anArray [ 
	aCanvas clipBy: self bounds during:[:cc| self drawBorderOn: cc usingEnds: anArray].
]

{ #category : #drawing }
PolygonMorph >> drawDashedBorderOn: aCanvas [ 
	self
		drawDashedBorderOn: aCanvas
		usingEnds: (Array with: vertices first with: vertices last)
]

{ #category : #drawing }
PolygonMorph >> drawDashedBorderOn: aCanvas usingEnds: anArray [ 
	"Display my border on the canvas. NOTE: mostly copied from  
	drawBorderOn:"
	| bevel topLeftColor bottomRightColor bigClipRect lineColor segmentOffset |
	(borderColor isNil
			or: [borderColor isColor
					and: [borderColor isTransparent]])
		ifTrue: [^ self].
	lineColor := borderColor.
	bevel := false.
	"Border colors for bevelled effects depend on CW ordering of  
	vertices"
	borderColor == #raised
		ifTrue: [topLeftColor := color lighter.
			bottomRightColor := color darker.
			bevel := true].
	borderColor == #inset
		ifTrue: [topLeftColor := owner colorForInsets darker.
			bottomRightColor := owner colorForInsets lighter.
			bevel := true].
	bigClipRect := aCanvas clipRect expandBy: self borderWidth + 1 // 2.
	segmentOffset := self borderDashOffset.
	self
		lineSegmentsDo: [:p1 :p2 | | p2i p1i | 
			p1i := p1 asIntegerPoint.
			p2i := p2 asIntegerPoint.
			self hasArrows
				ifTrue: ["Shorten line ends so as not to interfere with tip  
					of arrow."
					((arrows == #back
								or: [arrows == #both])
							and: [p1 = vertices first])
						ifTrue: [p1i := anArray first asIntegerPoint].
					((arrows == #forward
								or: [arrows == #both])
							and: [p2 = vertices last])
						ifTrue: [p2i := anArray last asIntegerPoint]].
			(closed
					or: ["bigClipRect intersects: (p1i rect: p2i)  
						optimized:"
						((p1i min: p2i)
							max: bigClipRect origin)
							<= ((p1i max: p2i)
									min: bigClipRect corner)])
				ifTrue: [bevel
						ifTrue: [lineColor := (p1i quadrantOf: p2i)
											> 2
										ifTrue: [topLeftColor]
										ifFalse: [bottomRightColor]].
					segmentOffset := aCanvas
								line: p1i
								to: p2i
								width: borderWidth
								color: lineColor
								dashLength: borderDashSpec first
								secondColor: borderDashSpec third
								secondDashLength: borderDashSpec second
								startingOffset: segmentOffset]]
]

{ #category : #drawing }
PolygonMorph >> drawDropShadowOn: aCanvas [ 
	"Display the receiver, a spline curve, approximated by straight 
	line segments."
	self assert: [vertices notEmpty] description: 'a polygon must have at least one point'.
	closed 
		ifTrue: [aCanvas drawPolygon: self getVertices fillStyle: self shadowColor].
]

{ #category : #drawing }
PolygonMorph >> drawOn: aCanvas [ 
	"Display the receiver, a spline curve, approximated by straight 
	line segments."
	| array |
	vertices size < 1
		ifTrue: [self error: 'a polygon must have at least one point'].
	closed ifTrue:
		[aCanvas drawPolygon: self getVertices fillStyle: self fillStyle ].
	array := self drawArrowsOn: aCanvas.
	self drawClippedBorderOn: aCanvas usingEnds: array.

]

{ #category : #drawing }
PolygonMorph >> drawOnFormCanvas: aCanvas [ 
	"Display the receiver, a spline curve, approximated by straight line segments."

	vertices size < 1 ifTrue: [self error: 'a polygon must have at least one point'].
	closed & color isTransparent not
		ifTrue: [aCanvas stencil: self filledForm at: bounds topLeft - 1 color: color].
	(borderColor isColor and: [borderColor isTranslucentButNotTransparent])
		ifTrue: [aCanvas stencil: self borderForm at: bounds topLeft color: borderColor]
		ifFalse: [self drawBorderOn: aCanvas].
	self arrowForms do:
		[:f | aCanvas 
				stencil: f 
				at: f offset
				color: (borderColor isColor ifTrue: [borderColor] ifFalse: [color])]
]

{ #category : #editing }
PolygonMorph >> dropVertex: ix event: evt fromHandle: handle [
	"Leave vertex in new position. If dropped ontop another vertex delete this one. Check for too few vertices before deleting. The alternative is not pretty"
	
	| p |
	p := vertices at: ix.
	(vertices size >= 2
			and: ["check for too few vertices before deleting. The alternative 
				is not pretty -wiz"
				((vertices atWrap: ix - 1)
						distanceTo: p)
						< 3
					or: [((vertices atWrap: ix + 1)
							distanceTo: p)
							< 3]])
		ifTrue: ["Drag a vertex onto its neighbor means delete"
				self deleteVertexAt: ix .].
	evt shiftPressed
		ifTrue: [self removeHandles]
		ifFalse: [self addHandles
			"remove then add to recreate"]
]

{ #category : #attachments }
PolygonMorph >> endShapeColor: aColor [
	self borderColor: aColor.
	self isClosed ifTrue: [ self color: aColor ].
]

{ #category : #attachments }
PolygonMorph >> endShapeWidth: aWidth [
	| originalWidth originalVertices transform |
	originalWidth := self valueOfProperty: #originalWidth ifAbsentPut: [ self borderWidth isZero ifFalse: [ self borderWidth ] ifTrue: [ 2 ] ].
	self borderWidth: aWidth.
	originalVertices := self valueOfProperty: #originalVertices ifAbsentPut: [
		self vertices collect: [ :ea | (ea - (self referencePosition)) rotateBy: self heading degreesToRadians about: 0@0 ]
	].
	transform := MorphicTransform offset: 0@0 angle: self heading degreesToRadians scale: originalWidth / aWidth.
	self setVertices: (originalVertices collect: [ :ea |
		((transform transform: ea) + self referencePosition) asIntegerPoint
	]).
	self computeBounds.
]

{ #category : #geometry }
PolygonMorph >> extent: newExtent [ 
	"Not really advisable, but we can preserve most of the geometry if we don't
	shrink things too small."
	| safeExtent center |
	center := self referencePosition.
	safeExtent := newExtent max: 20@20.
	self setVertices: (vertices collect:
		[:p | p - center * (safeExtent asFloatPoint / (bounds extent max: 1@1)) + center])
]

{ #category : #accessing }
PolygonMorph >> fillStyle [

	^ self isOpen
		ifTrue: [ self borderColor  "easy access to line color from halo"]
		ifFalse: [ super fillStyle ]
]

{ #category : #accessing }
PolygonMorph >> fillStyle: newColor [

	^ self isOpen
		ifTrue: [ self borderColor: newColor asColor "easy access to line color from halo"]
		ifFalse: [ super fillStyle: newColor ]
]

{ #category : #private }
PolygonMorph >> filledForm [
	"Note: The filled form is actually 2 pixels bigger than bounds, and the point corresponding to this morphs' position is at 1@1 in the form.  This is due to the details of the fillig routines, at least one of which requires an extra 1-pixel margin around the outside.  Computation of the filled form is done only on demand."
	| bb origin |
	closed ifFalse: [^ filledForm := nil].
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

{ #category : #attachments }
PolygonMorph >> firstVertex [
	^vertices first
]

{ #category : #geometry }
PolygonMorph >> flipHAroundX: centerX [
	"Flip me horizontally around the center.  If centerX is nil, compute my center of gravity."

	| cent |
	cent := centerX 
		ifNil: [ bounds center x ] "average is the center"
		ifNotNil: [ centerX ].
	self setVertices: (vertices collect: [ :vv | ((vv x - cent) * -1 + cent) @ vv y ]) reversed.
]

{ #category : #geometry }
PolygonMorph >> flipVAroundY: centerY [
	"Flip me vertically around the center.  If centerY is nil, compute my center of gravity."

	| cent |
	cent := centerY 
		ifNil: [ bounds center y ]		"average is the center"
		ifNotNil: [ centerY ].
	self setVertices: (vertices collect: [:vv |
			vv x @ ((vv y - cent) * -1 + cent)]) reversed.
]

{ #category : #private }
PolygonMorph >> getVertices [

	smoothCurve ifFalse: [^ vertices].

	"For curves, enumerate the full set of interpolated points"
	^ Array streamContents:
		[:s | self lineSegmentsDo: [:pt1 :pt2 | s nextPut: pt1]]
]

{ #category : #editing }
PolygonMorph >> handleColorAt: vertIndex [
      "This is a backstop for MixedCurveMorph"

	^ Color yellow

]

{ #category : #'event handling' }
PolygonMorph >> handlesMouseDown: evt [

	^ (super handlesMouseDown: evt) or: [evt shiftPressed]
]

{ #category : #menu }
PolygonMorph >> handlesShowingPhrase [
	^ (self showingHandles
		ifTrue: ['hide handles']
		ifFalse: ['show handles']) translated
]

{ #category : #testing }
PolygonMorph >> hasArrows [
	"Are all the conditions meet for having arrows?"
	^ (closed or: [arrows == #none or: [vertices size < 2]]) not
]

{ #category : #private }
PolygonMorph >> includesHandle: aMorph [

	handles ifNil: [^ false].
	^ handles includes: aMorph
]

{ #category : #initialization }
PolygonMorph >> initialize [
	
	super initialize.
	vertices := Array
				with: 5 @ 0
				with: 20 @ 10
				with: 0 @ 20.
	closed := true.
	smoothCurve := false.
	arrows := #none.
	self computeBounds.
	self beSmoothCurve.
	self diamondOval.
]

{ #category : #editing }
PolygonMorph >> insertVertexAt: anIndex put: aValue [
	"This serves as a hook and a backstop for MixedCurveMorph."
	self setVertices: (vertices copyReplaceFrom: anIndex + 1 to: anIndex with: (Array with: aValue)).
]

{ #category : #geometry }
PolygonMorph >> intersectionsWith: aRectangle [
	"Answer a Set of points where the given Rectangle intersects with me.
	Ignores arrowForms."

	| retval |
	retval := IdentitySet new: 4.
	(self bounds intersects: aRectangle) ifFalse: [^ retval].

	self lineSegmentsDo: [ :lp1 :lp2 | | polySeg |
		polySeg := LineSegment from: lp1 to: lp2.
		aRectangle lineSegmentsDo: [ :rp1 :rp2 | | rectSeg int |
			rectSeg := LineSegment from: rp1 to: rp2.
			int := polySeg intersectionWith: rectSeg.
			int ifNotNil: [ retval add: int ].
		].
	].

	^retval

]

{ #category : #geometry }
PolygonMorph >> intersects: aRectangle [ 
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
PolygonMorph >> isAnimated [

	borderDashSpec ifNil: [^false].
	^ borderDashSpec size = 5 and: [(borderDashSpec fifth) > 0]
]

{ #category : #geometry }
PolygonMorph >> isBordered [
	^false
]

{ #category : #testing }
PolygonMorph >> isClosed [
	^ closed
]

{ #category : #testing }
PolygonMorph >> isCurve [
	^ smoothCurve
]

{ #category : #testing }
PolygonMorph >> isCurvy [
	"Test for significant curves.  
	Small smoothcurves in practice are straight."
	^ smoothCurve
		and: [vertices size > 2]
]

{ #category : #testing }
PolygonMorph >> isLineMorph [
	^closed not
]

{ #category : #testing }
PolygonMorph >> isOpen [
	^ closed not
]

{ #category : #attachments }
PolygonMorph >> lastVertex [
	^vertices last
]

{ #category : #geometry }
PolygonMorph >> lineBorderColor [
	^self borderColor
]

{ #category : #geometry }
PolygonMorph >> lineBorderColor: aColor [
	self borderColor: aColor
]

{ #category : #geometry }
PolygonMorph >> lineBorderWidth [

	^self borderWidth
]

{ #category : #geometry }
PolygonMorph >> lineBorderWidth: anInteger [

	self borderWidth: anInteger
]

{ #category : #geometry }
PolygonMorph >> lineColor [
	^self borderColor
]

{ #category : #geometry }
PolygonMorph >> lineColor: aColor [
	self borderColor: aColor
]

{ #category : #private }
PolygonMorph >> lineSegments [
	| lineSegments |
	lineSegments := OrderedCollection new.
	self lineSegmentsDo: [:p1 :p2 | lineSegments addLast: (Array with: p1 with: p2)].
	^ lineSegments
]

{ #category : #smoothing }
PolygonMorph >> lineSegmentsDo: endPointsBlock [ 
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
			(closed
					or: [vertices size = 1])
				ifTrue: [endPointsBlock value: beginPoint value: vertices first].
			^ self].
	"For curves we include all the interpolated sub segments."
	"self assert: [(vertices size > 2 )].	"
	cs := self coefficients.
	beginPoint := (x := cs first first) @ (y := cs fifth first).
	(closed
		ifTrue: [1 to: cs first size]
		ifFalse: [1 to: cs first size - 1])
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
PolygonMorph >> lineWidth [

	^self borderWidth
]

{ #category : #geometry }
PolygonMorph >> lineWidth: anInteger [

	self borderWidth: (anInteger rounded max: 1)
]

{ #category : #menu }
PolygonMorph >> makeBackArrow [
	arrows := #back.
	self computeBounds
]

{ #category : #menu }
PolygonMorph >> makeBothArrows [
	arrows := #both.
	self computeBounds
]

{ #category : #menu }
PolygonMorph >> makeClosed [
	closed := true.
	handles ifNotNil: [self removeHandles; addHandles].
	self computeBounds
]

{ #category : #menu }
PolygonMorph >> makeForwardArrow [
	arrows := #forward.
	self computeBounds
]

{ #category : #menu }
PolygonMorph >> makeNoArrows [
	arrows := #none.
	self computeBounds
]

{ #category : #menu }
PolygonMorph >> makeOpen [
	closed := false.
	handles ifNotNil: [self removeHandles; addHandles].
	self computeBounds
]

{ #category : #geometry }
PolygonMorph >> merge: aPolygon [ 
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
PolygonMorph >> mergeDropThird: mv in: hv from: shared [ 
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
PolygonMorph >> midVertices [
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

{ #category : #attachments }
PolygonMorph >> midpoint [
	"Answer the midpoint along my segments"
	| middle |
	middle := self totalLength.
	middle < 2 ifTrue: [ ^ self center ].
	middle := middle / 2.
	self lineSegmentsDo: [ :a :b | | dist |
		dist := (a distanceTo: b).
		middle < dist
			ifTrue: [ ^(a + ((b - a) * (middle / dist))) asIntegerPoint ].
		middle := middle - dist.
	].
	self error: 'can''t happen'
]

{ #category : #'event handling' }
PolygonMorph >> mouseDown: evt [

	^ evt shiftPressed
		ifTrue: [((owner isKindOf: PolygonMorph) and: [owner includesHandle: self])
					ifTrue: ["Prevent insertion handles from getting edited"
							^ super mouseDown: evt].
				self toggleHandles.
				handles ifNil: [^ self].
				vertices withIndexDo:  "Check for click-to-drag at handle site"
					[:vertPt :vertIndex |
					((handles at: vertIndex*2-1 ifAbsent: [ ^self ]) containsPoint: evt cursorPoint) ifTrue:
						["If clicked near a vertex, jump into drag-vertex action"
						evt hand newMouseFocus: (handles at: vertIndex*2-1)]]]
		ifFalse: [super mouseDown: evt]
]

{ #category : #'cubic support' }
PolygonMorph >> naturalCubicSlopesOf: knots [ 
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

{ #category : #editing }
PolygonMorph >> newVertex: ix event: evt fromHandle: handle [
	"Insert a new vertex and fix everything up! Install the drag-handle of the new vertex as recipient of further mouse events."

	| pt |
	"(self hasProperty: #noNewVertices) ifFalse:
		[pt := evt cursorPoint.
		self setVertices: (vertices copyReplaceFrom: ix + 1 to: ix with: (Array with: pt)).
		evt hand newMouseFocus: (handles at: ((ix + 1) * 2) - 1)]"
	"modified to remove now vestigial test. see PolygonMorph class>>arrowprototype"
	pt := evt cursorPoint.
	self  insertVertexAt: ix put:  pt .
	evt hand newMouseFocus: (handles at: ((ix + 1) * 2) - 1)
]

{ #category : #geometry }
PolygonMorph >> nextDuplicateVertexIndex [
	vertices
		doWithIndex: [:vert :index | ((index between: 2 and: vertices size - 1)
					and: [| epsilon v1 v2 | 
						v1 := vertices at: index - 1.
						v2 := vertices at: index + 1.
						epsilon := ((v1 x - v2 x) abs max: (v1 y - v2 y) abs)
									// 32 max: 1.
						vert
							onLineFrom: v1
							to: v2
							within: epsilon])
				ifTrue: [^ index]].
	^ 0
]

{ #category : #smoothing }
PolygonMorph >> nextToFirstPoint [
	"For arrow direction"
	self isCurvy
		ifTrue: [curveState
				ifNil: [self coefficients].
			^ curveState second]
		ifFalse: [^ vertices second]
]

{ #category : #smoothing }
PolygonMorph >> nextToLastPoint [
	"For arrow direction"
	self isCurvy
		ifTrue: [curveState
				ifNil: [self coefficients].
			^ curveState third]
		ifFalse: [^ vertices at: vertices size - 1]
]

{ #category : #attachments }
PolygonMorph >> nudgeForLabel: aRectangle [
	"Try to move the label off me. Prefer labels on the top and right."

	| i flags nudge |
	(self bounds intersects: aRectangle) ifFalse: [ ^ 0 @ 0 ].
	flags := 0.
	nudge := 0 @ 0.
	i := 1.
	aRectangle lineSegmentsDo: [ :rp1 :rp2 | | rectSeg |
		rectSeg := LineSegment from: rp1 to: rp2.
		self straightLineSegmentsDo: [ :lp1 :lp2 | 
			| polySeg int |
			polySeg := LineSegment from: lp1 to: lp2.
			int := polySeg intersectionWith: rectSeg.
			int ifNotNil: [ flags := flags bitOr: i ].
		].
		i := i * 2.
	].
	"Now flags has bitflags for which sides"
	nudge := flags caseOf: {
	"no intersection"
		[ 0 ] -> [ 0 @ 0 ].
		"2 adjacent sides only" 
		[ 9 ] -> [ 1 @ 1 ].
		[ 3 ] -> [ -1 @ 1 ].
		[ 12 ] -> [ 1 @ -1 ].
		[ 6 ] -> [ -1 @ -1 ].
		"2 opposite sides only" 
		[ 10 ] -> [ 0 @ -1 ].
		[ 5 ] -> [ 1 @ 0 ].
		"only 1 side" 
		[ 8 ] -> [ -1 @ 0 ].
		[ 1 ] -> [ 0 @ -1 ].
		[ 2 ] -> [ 1 @ 0 ].
		[ 4 ] -> [ 0 @ 1 ].
		"3 sides" 
		[ 11 ] -> [ 0 @ 1 ].
		[ 13 ] -> [ 1 @ 0 ].
		[ 14 ] -> [ 0 @ -1 ].
		[ 7 ] -> [ -1 @ 0 ].
 		"all sides" 
		[ 15 ] -> [ 1 @ -1 "move up and to the right" ].
	}.
	^nudge
]

{ #category : #accessing }
PolygonMorph >> openOrClosePhrase [
	| curveName |
	curveName := (self isCurve
				ifTrue: ['curve']
				ifFalse: ['polygon']) translated.
	^ closed
		ifTrue: ['make open {1}' translated format: {curveName}]
		ifFalse: ['make closed {1}' translated format: {curveName}]
]

{ #category : #'rotate scale and flex' }
PolygonMorph >> prepareForRotating [
	"When rotating from a halo, I can do this without a flex shell"

	^ self

]

{ #category : #'rotate scale and flex' }
PolygonMorph >> prepareForScaling [
	"When scaling from a halo, I can do this without a flex shell"

	^ self

]

{ #category : #private }
PolygonMorph >> privateMoveBy: delta [
	super privateMoveBy: delta.
	vertices := vertices collect: [:p | p + delta].
	self arrowForms do: [:f | f offset: f offset + delta].
	curveState := nil.  "Force recomputation"
	(self valueOfProperty: #referencePosition) ifNotNil:
		[:oldPos | self setProperty: #referencePosition toValue: oldPos + delta]
]

{ #category : #shaping }
PolygonMorph >> rectangleOval [
	"Set my vertices to an array of corner vertices.
	Order of vertices is in the tradion of warpblt quads."

	self setVertices: self bounds corners.
]

{ #category : #geometry }
PolygonMorph >> reduceVertices [
	"Reduces the vertices size, when 3 vertices are on the same line with a little epsilon."
	| dup |
	[ (dup := self nextDuplicateVertexIndex) > 0 ] whileTrue: [
		self setVertices: (vertices copyWithoutIndex: dup)
	].
	^vertices size.
]

{ #category : #geometry }
PolygonMorph >> referencePosition [ 
	"Return the current reference position of the receiver"
	^ self valueOfProperty: #referencePosition ifAbsent: [super referencePosition]

]

{ #category : #caching }
PolygonMorph >> releaseCachedState [

	super releaseCachedState.
	filledForm := nil.
	arrowForms := nil.
	borderForm := nil.
	curveState := nil.
	(self hasProperty: #flex) ifTrue:
		[self removeProperty: #flex].

]

{ #category : #menu }
PolygonMorph >> removeHandles [
	handles ifNotNil: [
		handles do: [:h | h delete].
		handles := nil].
]

{ #category : #dashes }
PolygonMorph >> removeVertex: aVert [
	"Make sure that I am not left with less than two vertices"
	| newVertices |
	vertices size < 2 ifTrue: [ ^self ].
	newVertices := vertices copyWithout: aVert.
	newVertices size caseOf: {
		[1] -> [ newVertices := { newVertices first . newVertices first } ].
		[0] -> [ newVertices := { aVert . aVert } ]
	} otherwise: [].
	self setVertices: newVertices 

]

{ #category : #geometry }
PolygonMorph >> rotationCenter [
	"Return the rotation center of the receiver. The rotation center defines the relative offset inside the receiver's bounds for locating the reference position."
	| refPos |
	refPos := self valueOfProperty: #referencePosition ifAbsent: [^ 0.5@0.5].
	^ (refPos - self bounds origin) / self bounds extent asFloatPoint
]

{ #category : #geometry }
PolygonMorph >> rotationCenter: aPointOrNil [

	"Set the new rotation center of the receiver. The rotation center defines the relative offset inside the receiver's bounds for locating the reference position."

	| box |

	aPointOrNil
		ifNil: [ self removeProperty: #referencePosition ]
		ifNotNil: [ box := self bounds.
			self setProperty: #referencePosition toValue: box origin + ( aPointOrNil * box extent )
			]
]

{ #category : #'rotate scale and flex' }
PolygonMorph >> rotationDegrees [

	^ self forwardDirection
]

{ #category : #geometry }
PolygonMorph >> rotationDegrees: degrees [ 
	| flex center |
	(center := self valueOfProperty: #referencePosition) ifNil:
		[self setProperty: #referencePosition toValue: (center := self bounds center)].
	flex := (MorphicTransform offset: center negated)
			withAngle: (degrees - self forwardDirection) degreesToRadians.
	self setVertices: (vertices collect: [:v | (flex transform: v) - flex offset]).
	self forwardDirection: degrees.


]

{ #category : #geometry }
PolygonMorph >> scale: scaleFactor [ 
	| flex center ratio |
	ratio := self scaleFactor / scaleFactor.
	self borderWidth: ((self borderWidth / ratio) rounded max: 0).
	center := self referencePosition.
	flex := (MorphicTransform offset: center negated) withScale: ratio.
	self setVertices: (vertices collect: [:v | (flex transform: v) - flex offset]).
	super scale: scaleFactor.
]

{ #category : #'cubic support' }
PolygonMorph >> segmentedSlopesOf: knots [ 
	"For a collection of floats. Returns the slopes for straight 
	segments between vertices."
	"last slope closes the polygon. Always return same size as 
	self. "
	^ knots collectWithIndex: [:x :i | (knots atWrap: i + 1) - x]
]

{ #category : #menu }
PolygonMorph >> setRotationCenterFrom: aPoint [
	"Polygons store their referencePosition."
	self setProperty: #referencePosition toValue: aPoint
]

{ #category : #private }
PolygonMorph >> setVertices: newVertices [
	vertices := newVertices.
	handles ifNotNil: [self removeHandles; addHandles].
	self computeBounds
]

{ #category : #menu }
PolygonMorph >> showOrHideHandles [
	self showingHandles
		ifTrue:	[self removeHandles]
		ifFalse:	[self addHandles]
]

{ #category : #menu }
PolygonMorph >> showingHandles [
	^ handles notNil
]

{ #category : #smoothing }
PolygonMorph >> slopes: knots [ 
	"Choose slopes according to state of polygon"
	self isCurvy ifFalse: [^ self segmentedSlopesOf: knots ].
	^ closed
		ifTrue: [ self closedCubicSlopesOf: knots ]
		ifFalse: [ self naturalCubicSlopesOf: knots ]
]

{ #category : #accessing }
PolygonMorph >> smoothOrSegmentedPhrase [
	| lineName |
	lineName := (closed
					ifTrue: ['outline']
					ifFalse: ['line']) translated.
	^ self isCurve
		ifTrue: [ 'make segmented {1}' translated format: {lineName} ]
		ifFalse: [ 'make smooth {1}' translated format: {lineName} ].
]

{ #category : #menu }
PolygonMorph >> specifyDashedLine [

	| executableSpec newSpec |
	executableSpec := UIManager default
		request:
'Enter a dash specification as
{ major dash length. minor dash length. minor dash color }
The major dash will have the normal border color.
A blank response will remove the dash specification.
[Note: You may give 5 items as, eg, {10. 5. Color white. 0. 3}
where the 4th ityem is zero, and the 5th is the number of pixels
by which the dashes will move in each step of animation]' translated
		initialAnswer: '{ 10. 5. Color red }'.
	executableSpec isEmptyOrNil ifTrue:
		[^ self stopStepping; dashedBorder: nil].
	newSpec := [self class compiler evaluate: executableSpec] ifError:
		[^ self stopStepping; dashedBorder: nil].
	(newSpec first isNumber and: [newSpec second isNumber and: [newSpec third isColor]]) ifFalse:
		[^ self stopStepping; dashedBorder: nil].
	newSpec size = 3 ifTrue:
		[^ self stopStepping; dashedBorder: newSpec].
	(newSpec size = 5 and: [newSpec fourth isNumber and: [newSpec fifth isNumber]]) ifTrue:
		[^ self dashedBorder: newSpec; startStepping].

]

{ #category : #menu }
PolygonMorph >> standardArrows [

	self removeProperty: #arrowSpec.
	self computeBounds
]

{ #category : #stepping }
PolygonMorph >> step [
	borderDashSpec ifNil: [^super step].
	borderDashSpec size < 5 ifTrue: [^super step].

	"Only for dashed lines with creep"
	borderDashSpec at: 4 put: (borderDashSpec fourth) + borderDashSpec fifth.
	self changed.
	^ super step
]

{ #category : #stepping }
PolygonMorph >> stepTime [

	^ 100
]

{ #category : #smoothing }
PolygonMorph >> straightLineSegmentsDo: endPointsBlock [
	"Emit a sequence of segment endpoints into endPointsBlock.
	Work the same way regardless of whether I'm curved."
	| beginPoint |
	beginPoint := nil.
		vertices do:
			[:vert | beginPoint ifNotNil:
				[endPointsBlock value: beginPoint
								value: vert].
			beginPoint := vert].
		(closed or: [vertices size = 1])
			ifTrue: [endPointsBlock value: beginPoint
									value: vertices first].
]

{ #category : #geometry }
PolygonMorph >> straighten [
	self setVertices: { vertices first . vertices last }
]

{ #category : #menu }
PolygonMorph >> toggleHandles [

	handles ifNil: [self addHandles] ifNotNil: [self removeHandles].


]

{ #category : #accessing }
PolygonMorph >> toggleOpenOrClosed [
	"toggle the open/closed status of the receiver"
	closed ifTrue: [ self makeOpen ] ifFalse: [ self makeClosed ]
]

{ #category : #menu }
PolygonMorph >> toggleSmoothing [

	smoothCurve := smoothCurve not.
	handles ifNotNil: [self removeHandles; addHandles].
	self computeBounds
]

{ #category : #attachments }
PolygonMorph >> totalLength [
	"Answer the full length of my segments. Can take a long time if I'm curved."
	| length |
	length := 0.
	self lineSegmentsDo: [ :a :b | length := length + (a distanceTo: b) ].
	^ length.
]

{ #category : #'cubic support' }
PolygonMorph >> transform: coefficients toCubicPointPolynomialAt: vIndex [ 
	"From curve information assemble a 4-array of points representing the coefficents for curve segment between to points. Beginning point is first point in array endpoint is the pointSum of the array. Meant to be sent to newcurves idea of curve coefficents." 
	| transformed |
	transformed := (1 to: 4) collect: [:i | 
			((coefficients at: i) at: vIndex) @ ((coefficients at: 4 + i) at: vIndex)].
	^ Cubic withAll: transformed
]

{ #category : #private }
PolygonMorph >> transformVerticesFrom: oldOwner to: newOwner [
	| oldTransform newTransform world newVertices |
	world := self world.
	oldTransform := oldOwner
		ifNil: [ IdentityTransform new ]
		ifNotNil: [ oldOwner transformFrom: world ].
	newTransform := newOwner
		ifNil: [ IdentityTransform new ]
		ifNotNil: [ newOwner transformFrom: world ].
	newVertices := vertices collect: [ :ea | newTransform globalPointToLocal:
		(oldTransform localPointToGlobal: ea) ].
	self setVertices: newVertices.

]

{ #category : #geometry }
PolygonMorph >> transformedBy: aTransform [
	self setVertices: (self vertices collect:[:v| aTransform localPointToGlobal: v])
]

{ #category : #menu }
PolygonMorph >> unrotatedLength [
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"

	vertices size = 2 ifTrue:
		[^ (vertices second - vertices first) r].

	^ ((PolygonMorph new setVertices: vertices) rotationDegrees: self rotationDegrees negated) height
]

{ #category : #menu }
PolygonMorph >> unrotatedLength: aLength [
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"

	vertices size = 2 ifTrue: [^ self arrowLength: aLength].

	self setVertices: ((((PolygonMorph new setVertices: vertices) rotationDegrees: self rotationDegrees negated) height: aLength) rotationDegrees: 0) vertices
]

{ #category : #menu }
PolygonMorph >> unrotatedWidth [
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"
	
	vertices size = 2 ifTrue: [^ self borderWidth].
	^ ((PolygonMorph new setVertices: vertices) rotationDegrees: self rotationDegrees negated) width
]

{ #category : #menu }
PolygonMorph >> unrotatedWidth: aWidth [
	"If the receiver bears rotation without a transformation morph, answer what its length in the direction it is headed is"

	self borderWidth: aWidth
]

{ #category : #editing }
PolygonMorph >> updateHandles [
	| newVert |
	self isCurvy
		ifTrue: [handles first center: vertices first.
			handles last center: vertices last.
			self midVertices
				withIndexDo: [:midPt :vertIndex | (closed
							or: [vertIndex < vertices size])
						ifTrue: [newVert := handles atWrap: vertIndex * 2.
							newVert position: midPt - (newVert extent // 2)]]]
		ifFalse: [vertices
				withIndexDo: [:vertPt :vertIndex | | oldVert | 
					oldVert := handles at: vertIndex * 2 - 1.
					oldVert position: vertPt - (oldVert extent // 2).
					(closed
							or: [vertIndex < vertices size])
						ifTrue: [newVert := handles at: vertIndex * 2.
							newVert position: vertPt
									+ (vertices atWrap: vertIndex + 1) - newVert extent // 2 + (1 @ -1)]]]
]

{ #category : #dashes }
PolygonMorph >> vertexAt: n [
	^vertices at: (n min: vertices size).
]

{ #category : #accessing }
PolygonMorph >> vertices [
	^ vertices
]

{ #category : #initialization }
PolygonMorph >> vertices: verts color: aColor borderWidth: borderWidthInteger borderColor: anotherColor [ 
	super initialize.
	vertices := verts.
	color := aColor.
	borderWidth := borderWidthInteger.
	borderColor := anotherColor.
	closed := vertices size > 2.
	arrows := #none.
	self computeBounds
]

{ #category : #editing }
PolygonMorph >> verticesAt: ix put: newPoint [
	vertices at: ix put: newPoint.
	self computeBounds
]

{ #category : #stepping }
PolygonMorph >> wantsSteps [
	super wantsSteps ifTrue: [^true].

	"For crawling ants effect of dashed line."
	^ self isAnimated
]
