"
i implement cairo-specific path builder. see my superclass for more details.
"
Class {
	#name : #AthensCairoPathBuilder,
	#superclass : #AthensPathBuilder,
	#traits : 'TCairoLibrary',
	#classTraits : 'TCairoLibrary classTrait',
	#instVars : [
		'context',
		'absolute',
		'endPoint',
		'lastControlPoint'
	],
	#category : #'Athens-Cairo-Paths'
}

{ #category : #utilities }
AthensCairoPathBuilder class >> buildPathFrom:  aPathCreatingBlock [
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> absolute [
	absolute := true
]

{ #category : #private }
AthensCairoPathBuilder >> angleOfVector: v [

	| n acos |

	n := v normalized.
	acos := n x arcCos.

	^ v y < 0 ifTrue: [ Float pi * 2 - acos ] ifFalse: [ acos ]
]

{ #category : #private }
AthensCairoPathBuilder >> arcCenterX: xc centerY: yc radius: radius startAngle: angle1 endAngle: angle2 [
	^ self ffiCall: #(void cairo_arc (AthensCairoCanvas context,
				double xc,
				double yc,
				double radius,
				double angle1,
				double angle2) )
]

{ #category : #private }
AthensCairoPathBuilder >> arcNegativeCenterX: xc centerY: yc radius: radius startAngle: angle1 endAngle: angle2 [
	^ self ffiCall: #(void cairo_arc_negative (AthensCairoCanvas context,
				double xc,
				double yc,
				double radius,
				double angle1,
				double angle2) )
]

{ #category : #private }
AthensCairoPathBuilder >> arcTo: newEndPoint angle: angle cw: aBool [

	" Add a clockwise arc segment, starting from current path endpoint and
	ending at andPt. Angle should be specified in radians
	"

	| start end center v radius startAngle endAngle cwAngle |
	lastControlPoint := nil.
	angle isZero ifTrue: [ ^ self lineTo: newEndPoint ].

	start := endPoint.
	endPoint := end := self toAbsolute: newEndPoint.

	start = end ifTrue: [ ^ self ].

	"we have to transform the input. because Cario expects
	the center , radius, starting and ending angle,
	and we have the starting point, the ending point , and the angle.
	"
	aBool ifTrue: [cwAngle := angle] ifFalse: [cwAngle := angle negated].
	center := self calcCenter: start end: end  angle: cwAngle.
	v := (start - center).
	radius := v r.
	startAngle := self angleOfVector: v.
	endAngle := self angleOfVector: (end-center).
	aBool ifTrue: [ self arcCenterX: center x centerY: center y  radius: radius  startAngle: startAngle endAngle: endAngle ]
		ifFalse: [ self arcNegativeCenterX: center x centerY: center y  radius: radius  startAngle: startAngle  endAngle: endAngle ]
]

{ #category : #private }
AthensCairoPathBuilder >> calcCenter: start end: end angle: angle [

	| v center radius len m |

	v := end - start.

	m := AthensAffineTransform new rotateByRadians: (Float pi - angle   /2).

	v := m transform: v.
	len := v r.
	radius := len / 2 / (angle /2) sin.

	center := v * (radius/len) + start.

	^ center
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> ccwArcTo: newEndPoint angle: angle [
	" Add a counter-clockwise arc segment, starting from current path endpoint and
	ending at andPt. Angle should be specified in radians
	"
	^ self arcTo: newEndPoint angle: angle cw: false
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> close [

	self closePath.

	endPoint := self getCurrentPoint.
	lastControlPoint := nil
]

{ #category : #private }
AthensCairoPathBuilder >> closePath [
	^ self ffiCall: #(void cairo_close_path (AthensCairoCanvas context))
]

{ #category : #accessing }
AthensCairoPathBuilder >> context: anObject [

	context := anObject
]

{ #category : #private }
AthensCairoPathBuilder >> copyPath [
	^ self ffiCall: #( AthensCairoPath cairo_copy_path (AthensCairoCanvas context) )
]

{ #category : #accessing }
AthensCairoPathBuilder >> createPath: aBlock [

	self newPath.
	"set default to relative"
	absolute := false.
	endPoint := ZeroPoint.

	"set the implicit path origin"
	self moveToX: 0.0 Y: 0.0.

	aBlock value: self.

	^ self copyPath initialize
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> curveVia: p1 and: p2 to: aPoint [
	|  pt1 |

	pt1 := self toAbsolute: p1.
	lastControlPoint := self toAbsolute: p2.
	endPoint := self toAbsolute: aPoint.

	self curveViaX: pt1 x Y: pt1 y viaX: lastControlPoint x Y: lastControlPoint y toX: endPoint x Y: endPoint y
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> curveVia: p1 to: aPoint [
	| pt0 pt1 cp1 cp2 twoThirds |
	"Quad bezier curve"

	pt0 := endPoint.
	pt1 := self toAbsolute: p1.
	endPoint := self toAbsolute: aPoint.
	lastControlPoint := nil.

"Any quadratic spline can be expressed as a cubic (where the cubic term is zero). The end points of the cubic will be the same as the quadratic's.

	CP0 = QP0
	CP3 = QP2

The two control points for the cubic are:

	CP1 = QP0 + 2/3 *(QP1-QP0)
	CP2 = QP2 + 2/3 *(QP1-QP2)"

	twoThirds := (2/3) asFloat.

	cp1 := pt1 - pt0 * twoThirds + pt0.
	cp2 := pt1 - endPoint * twoThirds + endPoint.

	self curveViaX: cp1 x Y: cp1 y viaX: cp2 x Y: cp2 y toX: endPoint x Y: endPoint y
]

{ #category : #private }
AthensCairoPathBuilder >> curveViaX: x1 Y: y1 viaX: x2 Y: y2 toX: x3 Y: y3 [

	^ self
		  primCurveViaX: x1 asFloat
		  Y: y1 asFloat
		  viaX: x2 asFloat
		  Y: y2 asFloat
		  toX: x3 asFloat
		  Y: y3 asFloat
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> cwArcTo: newEndPoint angle: angle [
	" Add a clockwise arc segment, starting from current path endpoint and
	ending at andPt. Angle should be specified in radians
	"
	^ self arcTo: newEndPoint angle: angle cw: true
]

{ #category : #private }
AthensCairoPathBuilder >> getCurrentPoint [
	^ context getCurrentPoint
]

{ #category : #private }
AthensCairoPathBuilder >> glyphPath: glyphs size: numGlyphs [
	"void cairo_glyph_path (cairo_t *cr,
                  const cairo_glyph_t *glyphs,
                  int num_glyphs);"
	^ self ffiCall: #(

		void cairo_glyph_path ( AthensCairoCanvas context ,
			void * glyphs,
			int numGlyphs) )
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> hLineTo: x [
	^ self lineTo: (absolute ifTrue: [ x @ endPoint y] ifFalse: [ x @ 0 ])
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> lineTo: aPoint [

	endPoint := self toAbsolute: aPoint.
	lastControlPoint := nil.
	^ self lineToX: endPoint x asFloat Y: endPoint y asFloat
]

{ #category : #private }
AthensCairoPathBuilder >> lineToX: x Y: y [
	^ self ffiCall: #(void cairo_line_to (AthensCairoCanvas context,
		 double x, double y ) )
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> moveTo: aPoint [

	endPoint := self toAbsolute: aPoint.
	lastControlPoint := nil.

	^ self moveToX: endPoint x asFloat Y: endPoint y asFloat
]

{ #category : #private }
AthensCairoPathBuilder >> moveToX: x Y: y [
	" move command always starts a new contour "
	^ self ffiCall: #(void cairo_move_to (AthensCairoCanvas context,
		 double x, double y ) )
]

{ #category : #private }
AthensCairoPathBuilder >> newPath [
 ^ self ffiCall: #( void cairo_new_path ( AthensCairoCanvas context ) )
]

{ #category : #private }
AthensCairoPathBuilder >> primCurveViaX: x1 Y: y1 viaX: x2 Y: y2 toX: x3 Y: y3 [
	^ self ffiCall: #(void cairo_curve_to(AthensCairoCanvas context,
                                                         double x1,
                                                         double y1,
                                                         double x2,
                                                         double y2,
                                                         double x3,
                                                         double y3))
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> reflectedCurveVia: p2 to: aPoint [
	|  pt1 |

	pt1 := lastControlPoint
		ifNil: [ endPoint ]
		ifNotNil: [ endPoint * 2 - lastControlPoint].
	lastControlPoint := self toAbsolute: p2.
	endPoint := self toAbsolute: aPoint.

	self curveViaX: pt1 x Y: pt1 y viaX: lastControlPoint x Y: lastControlPoint y toX: endPoint x Y: endPoint y
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> relative [
	absolute := false
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> string: aString font: aFont [
	"Adds closed paths for the string to the current path. The generated path if filled, achieves an effect similar to that of cairo_show_glyphs()."
	|glyphs metricsProvider|
	metricsProvider := CairoFontMetricsProvider new
		font: aFont.
	glyphs := metricsProvider glyphsOf: aString.
	context setScaledFont: metricsProvider cairoFont.
	self glyphPath: glyphs getHandle size: glyphs size
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> toAbsolute: aPoint [

	^ absolute ifTrue: [ aPoint ] ifFalse: [ endPoint + aPoint ]
]

{ #category : #'path commands' }
AthensCairoPathBuilder >> vLineTo: y [
	^ self lineTo: (absolute ifTrue: [ endPoint x @ y] ifFalse: [ 0 @ y ])
]
