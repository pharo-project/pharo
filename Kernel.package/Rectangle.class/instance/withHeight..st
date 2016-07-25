rectangle functions
withHeight: height 
	"Return a copy of me with a different height"
	^ origin corner: corner x @ (origin y + height)