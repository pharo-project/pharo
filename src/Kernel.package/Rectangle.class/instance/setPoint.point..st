private
setPoint: pt1 point: pt2

	origin := (pt1 x min: pt2 x)@(pt1 y min: pt2 y).
	corner := (pt1 x max: pt2 x)@(pt1 y max: pt2 y).
	
