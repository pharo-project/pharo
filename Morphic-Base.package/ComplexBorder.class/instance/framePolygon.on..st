drawing
framePolygon: vertices on: aCanvas
	| dir1 dir2 dir3 nrm1 nrm2 nrm3 point1 point2 point3 
	 cross1 cross2 pointA pointB pointC pointD w p1 p2 p3 p4 balloon ends pointE pointF |
	balloon := aCanvas asBalloonCanvas.
	balloon == aCanvas ifFalse:[balloon deferred: true].
	ends := Array new: 6.
	w := width * 0.5.
	pointA := nil.
	1 to: vertices size do:[:i|
		p1 := vertices atWrap: i.
		p2 := vertices atWrap: i+1.
		p3 := vertices atWrap: i+2.
		p4 := vertices atWrap: i+3.

		dir1 := p2 - p1.
		dir2 := p3 - p2.
		dir3 := p4 - p3.

		(i = 1 | true) ifTrue:[
			"Compute the merge points of p1->p2 with p2->p3"
			cross1 := dir2 crossProduct: dir1.
			nrm1 := dir1 normalized. nrm1 := (nrm1 y * w) @ (0 - nrm1 x * w).
			nrm2 := dir2 normalized. nrm2 := (nrm2 y * w) @ (0 - nrm2 x * w).
			cross1 < 0 ifTrue:[nrm1 := nrm1 negated. nrm2 := nrm2 negated].
			point1 := (p1 x + nrm1 x) @ (p1 y + nrm1 y).
			point2 := (p2 x + nrm2 x) @ (p2 y + nrm2 y).
			pointA := self intersectFrom: point1 with: dir1 to: point2 with: dir2.
			point1 := (p1 x - nrm1 x) @ (p1 y - nrm1 y).
			point2 := (p2 x - nrm2 x) @ (p2 y - nrm2 y).
			pointB := point1 + dir1 + point2 * 0.5.
			pointB := p2 + ((pointB - p2) normalized * w).
			pointC := point2.
		].

		"Compute the merge points of p2->p3 with p3->p4"
		cross2 := dir3 crossProduct: dir2.
		nrm2 := dir2 normalized. nrm2 := (nrm2 y * w) @ (0 - nrm2 x * w).
		nrm3 := dir3 normalized. nrm3 := (nrm3 y * w) @ (0 - nrm3 x * w).
		cross2 < 0 ifTrue:[nrm2 := nrm2 negated. nrm3 := nrm3 negated].
		point2 := (p2 x + nrm2 x) @ (p2 y + nrm2 y).
		point3 := (p3 x + nrm3 x) @ (p3 y + nrm3 y).
		pointD := self intersectFrom: point2 with: dir2 to: point3 with: dir3.
		point2 := (p2 x - nrm2 x) @ (p2 y - nrm2 y).
		point3 := (p3 x - nrm3 x) @ (p3 y - nrm3 y).
		pointF := point2 + dir2.
		pointE := pointF + point3 * 0.5.
		pointE := p3 + ((pointE - p3) normalized * w).
		cross1 * cross2 < 0.0 ifTrue:[
			ends
				at: 1 put: pointA;
				at: 2 put: pointB;
				at: 3 put: pointC;
				at: 4 put: pointD;
				at: 5 put: pointE;
				at: 6 put: pointF.
		] ifFalse:[
			ends 
				at: 1 put: pointA; 
				at: 2 put: pointB;
				at: 3 put: pointC; 
				at: 4 put: pointF; 
				at: 5 put: pointE;
				at: 6 put: pointD.
		].
		self drawPolyPatchFrom: p2 to: p3 on: balloon usingEnds: ends.
		pointA := pointD.
		pointB := pointE.
		pointC := pointF.
		cross1 := cross2.
	].
	balloon == aCanvas ifFalse:[balloon flush].