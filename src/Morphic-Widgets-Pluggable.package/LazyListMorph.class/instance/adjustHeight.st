drawing
adjustHeight
	"private.  Adjust our height to match that of the underlying list"
	self height: (listItems size max: 1) * font height
