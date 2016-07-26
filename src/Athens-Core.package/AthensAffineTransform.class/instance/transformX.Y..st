vector-transform
transformX: px Y: py
	" transform x and y coordinates by receiver. Answer a Point"
	^ Point 
		x: (sx*px +(shx*py) + x)
		y: (shy*px + (sy*py) + y) 