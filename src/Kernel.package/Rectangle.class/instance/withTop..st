rectangle functions
withTop: y 
	"Return a copy of me with a different top y"
	^ origin x @ (y min: corner y) corner: corner x @ corner y