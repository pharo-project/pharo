drawing
adjustHeight
	"private.  Adjust our height to match the length of the underlying list"
	self height: (listItems size max: 1) * font height
