primitives
sourceQuad: pts destRect: aRectangle 
	| fixedPt1 |
	sourceX := sourceY := 0.
	self destRect: aRectangle.
	fixedPt1 := (pts at: 1) x isInteger 
		ifTrue: [ 16384 ]
		ifFalse: [ 16384.0 ].
	p1x := (pts at: 1) x * fixedPt1.
	p2x := (pts at: 2) x * fixedPt1.
	p3x := (pts at: 3) x * fixedPt1.
	p4x := (pts at: 4) x * fixedPt1.
	p1y := (pts at: 1) y * fixedPt1.
	p2y := (pts at: 2) y * fixedPt1.
	p3y := (pts at: 3) y * fixedPt1.
	p4y := (pts at: 4) y * fixedPt1.
	p1z := p2z := p3z := p4z := 16384	"z-warp ignored for now"