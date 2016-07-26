helpers
flattenQuadBezier: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3

	"Recursively subdive quadric bezier curve as long as #isFlatBezier.. answers false "

	"The points here is unboxed intentionally to avoid generating extra garbage
	(which contributes to performance loss)"

	(self isFlatQuadBezier: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3) ifTrue: [
		| midx midy |
		midx := (x2 + x1 + x2+x3) * 0.25 .  "mid ( mid(pt1,pt2), mid(pt2,pt3)) "
		midy := (y2 + y1 + y2+y3) * 0.25 . 

		dest 
			lineTo: (self inverseTransform: midx @ midy);
			lineTo: (self inverseTransform: x3 @ y3)
			
	"	dest 
			lineTo: (self inverseTransform: x2 @ y2);
			lineTo: (self inverseTransform: x3 @ y3)
	"
	
	] ifFalse: [
		| x12 y12 x23 y23 x123 y123 |	
	"calculate midpoints of line segments "
		x12 := (x1 + x2) * 0.5.
		y12 := (y1 + y2) * 0.5 .

		x23 := (x2 + x3) * 0.5 .
		y23 := (y2 + y3) * 0.5 .

		x123 := (x12 + x23) * 0.5.
		y123 := (y12 + y23) * 0.5.
		
		self flattenQuadBezier: x1 y1: y1 
			x2: x12 
			y2: y12 
			x3: x123 
			y3: y123.
			
		self flattenQuadBezier: x123 
			y1: y123
			x2: x23
			y2: y23 
			x3: x3 
			y3: y3.
	]
