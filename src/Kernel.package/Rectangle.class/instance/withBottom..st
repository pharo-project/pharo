rectangle functions
withBottom: y 
	"Return a copy of me with a different bottom y"
	^ origin x @ origin y corner: corner x @ (y max: origin y)