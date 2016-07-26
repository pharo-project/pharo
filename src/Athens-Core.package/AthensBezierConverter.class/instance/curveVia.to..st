converting path commands
curveVia: pt1 to: pt2


	self	 recursiveBezier2_x1: endPoint x y1: endPoint y
		x2: pt1 x y2: pt1 y
		x3: pt2 x y3: pt2 y