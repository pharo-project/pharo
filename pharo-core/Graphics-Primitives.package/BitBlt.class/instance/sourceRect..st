accessing
sourceRect: aRectangle 
	"Set the receiver's source form top left x and y, width and height to be 
	the top left coordinate and extent of the argument, aRectangle."
	sourceX := aRectangle left.
	sourceY := aRectangle top.
	width := aRectangle width.
	height := aRectangle height