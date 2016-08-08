modifying
swapAtPoint: p1 withPoint: p2
	"Swap the contents of p1 (x1,y1) with the contents of p2 (x2,y2)"
	
	self swapAtRow: p1 y atColumn: p1 x  withAtRow: p2 y atColumn: p2 x 