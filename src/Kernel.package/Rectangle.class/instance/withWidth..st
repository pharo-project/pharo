rectangle functions
withWidth: width 
	"Return a copy of me with a different width"
	^ origin corner: (origin x + width) @ corner y