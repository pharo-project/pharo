subdivision
subdivideAt: t do: aBinaryBlock
	| x12 y12 x23 y23 x34 y34 x1223 y1223 x2334 y2334 xsplit ysplit |
	
	x12 := x1 interpolateTo: x2 at: t.
	y12 := y1 interpolateTo: y2 at: t.
	x23 := x2 interpolateTo: x3 at: t.
	y23 := y2 interpolateTo: y3 at: t.
	x34 := x3 interpolateTo: x4 at: t.
	y34 := y3 interpolateTo: y4 at: t.

	x1223 := x12 interpolateTo: x23 at: t.
	x2334 := x23 interpolateTo: x34 at: t.
	y1223 := y12 interpolateTo: y23 at: t.
	y2334 := y23 interpolateTo: y34 at: t.
	
	xsplit := x1223 interpolateTo: x2334 at: t.
	ysplit := y1223 interpolateTo: y2334 at: t.
	
	^ aBinaryBlock 
		value: 
		"p1 - p12 - p1223 - psplit"
		(self copy 
			x2: x12;
			y2: y12;
			x3: x1223;
			y3: y1223;
			x4: xsplit;
			y4: ysplit )
		value:
		"psplit - p2334 - p34 - p4" 
		(self copy 
			x1: xsplit;
			y1: ysplit;
			x2: x2334;
			y2: y2334;
			x3: x34;
			y3: y34
		)	
