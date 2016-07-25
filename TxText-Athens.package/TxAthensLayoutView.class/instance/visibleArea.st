accessing
visibleArea
	"Answer the approximate x@y portion of text currently visible in the view in terms of fraction 0..1"
	

	| x y |
	x := layout width / self width.
	y := self text numberOfLines / (layout endPosition lineNumber - layout startPosition lineNumber).
	
	^ x@y