helpers
recursiveBezier2_x1: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3

	"recursively subdive bezier curve as long as #isFlatBezier2.. answers false "

	(self isFlatBezier2_x1: x1 y1: y1 x2: x2 y2: y2 x3: x3 y3: y3) ifTrue: [
		
		dest 
			lineTo: x2 @ y2;
			lineTo: x3 @ y3
	] ifFalse: [
		| x12 y12 x23 y23 x123 y123 |	
	"calculate midpoints of line segments "
		x12 := (x1 + x2) * 0.5.
		y12 := (y1 + y2) * 0.5 .

		x23 := (x2 + x3) * 0.5 .
		y23 := (y2 + y3) * 0.5 .

		x123 := (x12 + x23) * 0.5.
		y123 := (y12 + y23) * 0.5.
		
		self recursiveBezier2_x1: x1 y1: y1 
			x2: x12 
			y2: y12 
			x3: x123 
			y3: y123.
			
		self recursiveBezier2_x1: x123 
			y1: y123
			x2: x23
			y2: y23 
			x3: x3 
			y3: y3.
	]
