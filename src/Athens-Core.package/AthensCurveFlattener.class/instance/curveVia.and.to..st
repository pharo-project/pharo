path commands
curveVia: via1 and: via2 to: end 
	| pt1 pt2 pt3 pt4 curve |
	
	pt1 := transform transform: endPoint.
	pt2 := transform transform: via1.
	pt3 := transform transform: via2.
	pt4 := transform transform: end.
	
	endPoint := end.

	curve := AthensCubicBezier new
		x1: pt1 x;
		y1: pt1 y;
		x2: pt2 x;
		y2: pt2 y;
		x3: pt3 x;
		y3: pt3 y;
		x4: pt4 x;
		y4: pt4 y.
		
	curve recursiveSubDiv: self level: 0.
	self lineTo: endPoint.
	
	