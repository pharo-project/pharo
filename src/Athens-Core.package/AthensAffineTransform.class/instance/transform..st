vector-transform
transform: aPoint 
	| px py |
	
	px := aPoint x.
	py := aPoint y.
	^ Point 
		x: (sx*px +(shx*py) + x)
		y: (shy*px + (sy*py) + y) 