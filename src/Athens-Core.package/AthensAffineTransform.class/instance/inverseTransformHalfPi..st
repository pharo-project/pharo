private
inverseTransformHalfPi: aPoint
	"assume sx,sy = 0
		
	px := y0*shx + x.
	py := x0*shy + y.
	
	we doing inverse one, trying to find x0,y0 for rest of given variables (px,py,x,y,sx,sy,shx,shy)"
	| px py y0 x0 |
	
	px := aPoint x.
	py := aPoint y.

	x0 := py - y  / shy.
	y0 := px - x  / shx.

	^ x0@y0
	