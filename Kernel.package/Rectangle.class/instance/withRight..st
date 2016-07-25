rectangle functions
withRight: x 
	"Return a copy of me with a different right x"
	^ origin x @ origin y corner: (x max: origin x) @ corner y