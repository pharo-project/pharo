private
inverseTransformPiOrZero: aPoint
	"assume shx,shy = 0
		
	px := x0*sx + x.
	py := y0*sy + y.
	we doing inverse one, trying to find x0,y0 for rest of given variables (px,py,x,y,sx,sy,shx,shy)"
	
	| px py y0 x0 |
	
	px := aPoint x.
	py := aPoint y.

	x0 := px - x / sx.
	y0 := py - y / sy.

	^ x0@y0
	