rectangle functions
withLeft: x 
	"Return a copy of me with a different left x (but not going over right side)"
	^ (x min: corner x) @ origin y corner: corner x @ corner y