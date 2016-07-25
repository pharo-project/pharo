private
curveBounds
	"Compute the bounds from actual curve traversal, with 
	leeway for borderWidth. 
	Also note the next-to-first and next-to-last points for arrow 
	directions."
	"wiz - to avoid roundoff errors we return unrounded curvebounds."
	"we expect our receiver to take responsibility for approriate rounding adjustment."
	"hint: this is most likely 'self curveBounds expanded' "
	| pointAfterFirst pointBeforeLast  oX oY cX cY |
	self isCurvy
		ifFalse: [^ (Rectangle encompassing: vertices)
				expandBy: borderWidth * 0.5 ].
	curveState := nil.
	"Force recomputation"
	"curveBounds := vertices first corner: vertices last."
	pointAfterFirst := nil.
	self
		lineSegmentsDo: [:p1 :p2 | 
			pointAfterFirst isNil
				ifTrue: [pointAfterFirst := p2 floor .
					oX := cX := p1 x.
					oY := cY := p1 y. ].
			"curveBounds := curveBounds encompass: p2 ."
			oX:= oX min: p2 x.
			cX := cX max: p2 x.
			oY := oY min: p2 y.
			cY := cY max: p2 y.
			pointBeforeLast := p1 floor ].
	curveState at: 2 put: pointAfterFirst.
	curveState at: 3 put: pointBeforeLast.
	^ ( oX @ oY corner: cX @ cY )  expandBy: borderWidth * 0.5 